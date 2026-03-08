# =============================================================================
# AUTOMATIZACIONES BIBLIOMÉTRICAS — GUÍA COMPLETA DE APIs
# Autor: Frank Zela-Coila
# Versión: 1.0 — Marzo 2026
#
# ÍNDICE:
#  BLOQUE 1 — APIs de CITAS (Scopus ya cubierto)
#    1A. OpenAlex     → GRATIS, sin límite, mejor alternativa open
#    1B. Semantic Scholar → GRATIS, citas + referencias completas
#    1C. CrossRef     → GRATIS, citas desde CrossRef (parcial)
#    1D. WoS InCites  → PAGO institucional, Times Cited en WoS
#    1E. PubMed/NCBI  → GRATIS, metadatos (NO exporta citas)
#
#  BLOQUE 2 — ENRIQUECIMIENTO AUTOMÁTICO
#    2A. DOIs faltantes    → CrossRef por título+autor
#    2B. Cuartiles (SJR)   → SCImago/SJR por ISSN
#    2C. Open Access       → Unpaywall por DOI
#    2D. ORCID de autores  → ORCID API por nombre
#    2E. Normalización afiliaciones → OpenAlex Institutions
#
#  BLOQUE 3 — PIPELINE MAESTRO (todo junto, llamada por llamada)
# =============================================================================



# =============================================================================
# INSTALACIÓN Y PAQUETES
# =============================================================================

paquetes <- c("httr", "jsonlite", "dplyr", "stringr", "purrr",
              "openxlsx", "bibliometrix", "tibble", "readr")
nuevos   <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(nuevos) > 0) install.packages(nuevos, dependencies = TRUE)
invisible(lapply(paquetes, library, character.only = TRUE))



# =============================================================================
# BLOQUE 1A — OPENALEX (GRATIS · SIN KEY · SIN LÍMITE DIARIO)
# =============================================================================
# OpenAlex es la alternativa open source a Scopus/WoS.
# Indexa >250 millones de obras. Incluye citas, instituciones, temas.
# Documentación: https://docs.openalex.org
#
# QUÉ PUEDES OBTENER:
#   cited_by_count  → citas totales (actualizado diariamente)
#   is_oa           → ¿es Open Access?
#   oa_url          → URL al PDF gratuito
#   concepts        → temas/conceptos asignados automáticamente
#   authorships     → autores + ORCID + institución normalizada
#   referenced_works → referencias citadas (para co-citación)
#   citing_works_count → cuántos trabajos citan a este
# =============================================================================

# Función: enriquecer 1 registro por DOI ────────────────────────────────────
openalex_por_doi <- function(doi, email = "fzela@unsa.edu.pe",
                              pausa = 0.1) {
  # Usar email en el User-Agent: da acceso a pool premium (más rápido)
  if (is.na(doi) || trimws(doi) == "") return(NULL)

  doi_limpio <- sub("^https?://(dx\\.)?doi\\.org/", "", doi, ignore.case = TRUE)
  url        <- paste0("https://api.openalex.org/works/doi:", doi_limpio)

  resp <- tryCatch(
    httr::GET(url,
              httr::add_headers("User-Agent" = paste0("bibliometrix-peru/1.0 (", email, ")")),
              httr::timeout(10)),
    error = function(e) NULL
  )
  Sys.sleep(pausa)

  if (is.null(resp) || httr::status_code(resp) != 200) return(NULL)

  d <- tryCatch(
    jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"),
                       simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(d)) return(NULL)

  # Extraer campos clave
  tibble::tibble(
    doi_input         = doi,
    OA_citas          = as.integer(d$cited_by_count %||% NA),
    OA_es_openaccess  = as.logical(d$open_access$is_oa %||% NA),
    OA_url_pdf        = as.character(d$open_access$oa_url %||% NA),
    OA_id             = as.character(d$id %||% NA),
    OA_tipo_OA        = as.character(d$open_access$oa_status %||% NA),
    OA_conceptos      = {
      cs <- d$concepts
      if (length(cs) > 0) {
        nombres <- sapply(cs[1:min(5, length(cs))], function(x) x$display_name)
        paste(nombres, collapse = "; ")
      } else NA_character_
    },
    OA_primer_autor_inst = {
      auth <- d$authorships
      if (length(auth) > 0 && length(auth[[1]]$institutions) > 0)
        as.character(auth[[1]]$institutions[[1]]$display_name)
      else NA_character_
    }
  )
}

# Operador %||% (devuelve b si a es NULL)
`%||%` <- function(a, b) if (!is.null(a)) a else b


# ── Función vectorizada (para toda la base) ───────────────────────────────────
enriquecer_openalex <- function(df, col_doi = "DI",
                                 email = "fzela@unsa.edu.pe") {

  n   <- nrow(df)
  cat("=== ENRIQUECIMIENTO OPENALEX ===\n")
  cat("  Registros      :", n, "\n")
  cat("  Con DOI        :", sum(!is.na(df[[col_doi]]) & df[[col_doi]] != ""), "\n")
  cat("  Sin costo      : SÍ (API pública)\n")
  cat("  Tiempo estimado: ~", round(n * 0.15 / 60, 1), "minutos\n\n")

  resultados <- vector("list", n)

  for (i in seq_len(n)) {
    resultados[[i]] <- openalex_por_doi(df[[col_doi]][i], email)
    if (i %% 20 == 0 || i == n)
      cat(sprintf("  [%d/%d] OK: %d\n", i, n,
                  sum(!sapply(resultados[1:i], is.null))))
  }

  # Combinar resultados con la base
  res_df <- bind_rows(resultados)
  if (nrow(res_df) > 0) {
    df <- left_join(df,
                    res_df %>% rename(!!col_doi := doi_input),
                    by = col_doi)
  }

  cat("\n  Citas OpenAlex recuperadas    :",
      sum(!is.na(df$OA_citas)), "\n")
  cat("  Artículos Open Access         :",
      sum(df$OA_es_openaccess == TRUE, na.rm = TRUE), "\n")
  cat("  Con URL PDF gratuito          :",
      sum(!is.na(df$OA_url_pdf)), "\n\n")

  return(df)
}



# =============================================================================
# BLOQUE 1B — SEMANTIC SCHOLAR (GRATIS · API KEY OPCIONAL · 100 req/seg)
# =============================================================================
# Semantic Scholar (Allen Institute for AI) indexa >200 millones de papers.
# Registrar API key gratis en: https://www.semanticscholar.org/product/api
#
# VENTAJA ÚNICA: devuelve las REFERENCIAS CITADAS completas (para análisis
# de co-citación y acoplamiento bibliográfico).
# =============================================================================

SS_API_KEY <- ""  # opcional pero aumenta a 100 req/seg; sin key: 10 req/seg

semantic_scholar_doi <- function(doi, api_key = "", pausa = 0.15) {
  if (is.na(doi) || trimws(doi) == "") return(NULL)

  doi_limpio <- sub("^https?://(dx\\.)?doi\\.org/", "", doi, ignore.case = TRUE)
  url    <- paste0("https://api.semanticscholar.org/graph/v1/paper/DOI:",
                   utils::URLencode(doi_limpio))

  headers <- httr::add_headers("Accept" = "application/json")
  if (nchar(api_key) > 0)
    headers <- httr::add_headers("x-api-key" = api_key, "Accept" = "application/json")

  resp <- tryCatch(
    httr::GET(url,
              query   = list(fields = "citationCount,influentialCitationCount,isOpenAccess,openAccessPdf,references.externalIds"),
              headers,
              httr::timeout(10)),
    error = function(e) NULL
  )
  Sys.sleep(pausa)

  if (is.null(resp) || httr::status_code(resp) != 200) return(NULL)
  d <- tryCatch(
    jsonlite::fromJSON(httr::content(resp, as="text", encoding="UTF-8"),
                       simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(d)) return(NULL)

  tibble::tibble(
    doi_input                = doi,
    SS_citas                 = as.integer(d$citationCount %||% NA),
    SS_citas_influyentes     = as.integer(d$influentialCitationCount %||% NA),
    SS_openaccess            = as.logical(d$isOpenAccess %||% NA),
    SS_n_referencias         = length(d$references %||% list())
  )
}



# =============================================================================
# BLOQUE 1C — CROSSREF (GRATIS · SIN KEY · 50 req/seg con email)
# =============================================================================
# CrossRef indexa DOIs registrados. Devuelve "is-referenced-by-count"
# (citas de artículos con DOI registrado en CrossRef — subestima respecto
# a Scopus/WoS pero es 100% gratuito y abierto).
#
# DOCUMENTACIÓN: https://www.crossref.org/documentation/retrieve-metadata/
# =============================================================================

crossref_doi <- function(doi, email = "fzela@unsa.edu.pe", pausa = 0.05) {
  if (is.na(doi) || trimws(doi) == "") return(NULL)

  doi_limpio <- sub("^https?://(dx\\.)?doi\\.org/", "", doi, ignore.case = TRUE)
  url        <- paste0("https://api.crossref.org/works/",
                       utils::URLencode(doi_limpio))

  resp <- tryCatch(
    httr::GET(url,
              httr::add_headers("User-Agent" = paste0("bibliometrix/1.0 (", email, ")")),
              httr::timeout(10)),
    error = function(e) NULL
  )
  Sys.sleep(pausa)

  if (is.null(resp) || httr::status_code(resp) != 200) return(NULL)
  d <- tryCatch(
    jsonlite::fromJSON(httr::content(resp, as="text", encoding="UTF-8"),
                       simplifyVector = TRUE),
    error = function(e) NULL
  )
  if (is.null(d)) return(NULL)

  m <- d$message
  tibble::tibble(
    doi_input       = doi,
    CR_citas        = as.integer(m$`is-referenced-by-count` %||% NA),
    CR_tipo_doc     = as.character(m$type %||% NA),
    CR_issn_tipo    = as.character(paste(m$`issn-type`$value %||% "", collapse=";")),
    CR_editorial    = as.character(m$publisher %||% NA),
    CR_licencia     = as.character(m$license[[1]]$URL %||% NA)
  )
}


# =============================================================================
# BLOQUE 1D — WoS InCites API (INSTITUCIONAL — Times Cited en WoS)
# =============================================================================
# Requiere suscripción institucional a Web of Science.
# API Key: https://developer.clarivate.com
#
# QUÉ DEVUELVE:
#   timesCited          → citas en WoS
#   normalizedCitationImpact (NCI)
#   percentileCitations
# =============================================================================

WOS_API_KEY <- "66f74ac8754aeb0f4a85f16405f4da19f46da346"  # Poner aquí si tienes acceso institucional

incites_doi <- function(doi, api_key = WOS_API_KEY, pausa = 0.3) {
  if (is.na(doi) || trimws(doi) == "" || nchar(api_key) == 0) return(NULL)

  doi_limpio <- sub("^https?://(dx\\.)?doi\\.org/", "", doi, ignore.case = TRUE)

  resp <- tryCatch(
    httr::GET("https://api.clarivate.com/apis/wos-starter/v1/documents",
              query   = list(db = "WOS", q = paste0("DO=(", doi_limpio, ")"),
                             limit = 1, page = 1),
              httr::add_headers("X-ApiKey" = api_key),
              httr::timeout(15)),
    error = function(e) NULL
  )
  Sys.sleep(pausa)

  if (is.null(resp) || httr::status_code(resp) != 200) return(NULL)
  d <- tryCatch(
    jsonlite::fromJSON(httr::content(resp, as="text", encoding="UTF-8"),
                       simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(d)) return(NULL)

  hits <- d$hits
  if (length(hits) == 0) return(NULL)

  tibble::tibble(
    doi_input   = doi,
    WOS_citas   = as.integer(hits[[1]]$times_cited %||% NA),
    WOS_uid     = as.character(hits[[1]]$uid %||% NA)
  )
}
# Prueba rápida con un DOI de ejemplo
test_wos <- incites_doi("10.1016/j.jclinepi.2017.12.022")
print(test_wos)


# =============================================================================
# BLOQUE 2A — DOIs FALTANTES VÍA CROSSREF (por título + primer autor)
# =============================================================================
# Si un registro no tiene DOI, CrossRef puede encontrarlo por título y autor.
# Cobertura ~70-80% para artículos de revistas biomédicas en inglés.
# =============================================================================

buscar_doi_crossref <- function(titulo, primer_autor = NULL,
                                 anio = NULL, email = "fzela@unsa.edu.pe",
                                 pausa = 0.1) {
  if (is.na(titulo) || nchar(trimws(titulo)) < 15) return(NA_character_)

  query_parts <- list(
    `query.title`      = substr(titulo, 1, 120),
    `rows`             = "1",
    `select`           = "DOI,score,title"
  )
  if (!is.null(primer_autor) && !is.na(primer_autor))
    query_parts$`query.author` <- primer_autor
  if (!is.null(anio) && !is.na(anio))
    query_parts$`filter` <- paste0("from-pub-date:", anio, ",until-pub-date:", anio)

  resp <- tryCatch(
    httr::GET("https://api.crossref.org/works",
              query   = query_parts,
              httr::add_headers("User-Agent" = paste0("bibliometrix/1.0 (", email, ")")),
              httr::timeout(10)),
    error = function(e) NULL
  )
  Sys.sleep(pausa)

  if (is.null(resp) || httr::status_code(resp) != 200) return(NA_character_)
  d <- tryCatch(
    jsonlite::fromJSON(httr::content(resp, as="text", encoding="UTF-8"),
                       simplifyVector = TRUE),
    error = function(e) NULL
  )
  if (is.null(d)) return(NA_character_)

  items <- d$message$items
  if (length(items) == 0 || nrow(items) == 0) return(NA_character_)

  # Solo aceptar si la puntuación de similitud es alta (>80)
  score <- items$score[1]
  if (!is.na(score) && score > 80) {
    return(items$DOI[1])
  }
  return(NA_character_)
}

# ── Función vectorizada: completar DOIs faltantes ────────────────────────────
completar_dois <- function(df, col_doi = "DI", col_titulo = "TI",
                            col_autor = "AU", col_anio = "PY",
                            email = "fzela@unsa.edu.pe") {
  sin_doi <- which(is.na(df[[col_doi]]) | df[[col_doi]] == "")
  cat("=== COMPLETANDO DOIs FALTANTES VÍA CROSSREF ===\n")
  cat("  Registros sin DOI:", length(sin_doi), "\n")

  nuevos_dois <- integer(0)
  for (i in sin_doi) {
    primer_autor <- if (!is.na(df[[col_autor]][i])) {
      strsplit(df[[col_autor]][i], "[;,]")[[1]][1]
    } else NULL

    doi_nuevo <- buscar_doi_crossref(
      titulo       = df[[col_titulo]][i],
      primer_autor = primer_autor,
      anio         = df[[col_anio]][i],
      email        = email
    )

    if (!is.na(doi_nuevo)) {
      df[[col_doi]][i] <- doi_nuevo
      nuevos_dois       <- c(nuevos_dois, i)
    }

    if (length(sin_doi) > 0 && which(sin_doi == i) %% 10 == 0)
      cat(sprintf("  [%d/%d] DOIs nuevos encontrados: %d\n",
                  which(sin_doi == i), length(sin_doi), length(nuevos_dois)))
  }

  cat("  ✓ DOIs recuperados:", length(nuevos_dois), "de", length(sin_doi), "\n\n")
  return(df)
}



# =============================================================================
# BLOQUE 2B — CUARTILES SJR POR ISSN (GRATIS · SCImago)
# =============================================================================
# SCImago Journal Rankings (SJR) publica el cuartil de cada revista por año.
# Descargamos el CSV anual oficial y hacemos JOIN por ISSN.
# Cubre Scopus → ideal para tu proyecto.
# CORREGIDO ----
# =============================================================================
# FUNCIÓN 1: cargar_sjr()
# Reemplaza a descargar_sjr() — lee el CSV local ya descargado
# =============================================================================
#
# PARÁMETROS:
#   ruta_csv → ruta al archivo CSV de SCImago (descargado manualmente)
#   anio     → año del ranking (solo para mensajes informativos)
#
# DESCARGA MANUAL (si necesitas otro año):
#   1. Ir a https://www.scimagojr.com/journalrank.php
#   2. Seleccionar el año deseado
#   3. Clic en el ícono de descarga (CSV) al final de la página
#   4. Guardar como "data/sjr_XXXX.csv"

cargar_sjr <- function(ruta_csv, anio = 2023) {
  
  if (!file.exists(ruta_csv)) {
    stop(
      "Archivo no encontrado: ", ruta_csv, "\n",
      "Descárgalo manualmente desde:\n",
      "https://www.scimagojr.com/journalrank.php?year=", anio
    )
  }
  
  cat("--- Cargando SJR", anio, "desde:", ruta_csv, "---\n")
  
  # ── PASO 1: Leer el CSV con los parámetros correctos ────────────────────────
  # El separador es ";" y el decimal es "," (europeo)
  # Hay una columna Publisher duplicada → usamos name_repair para no crashear
  sjr_raw <- readr::read_delim(
    ruta_csv,
    delim          = ";",
    locale         = readr::locale(decimal_mark = ",", encoding = "UTF-8"),
    show_col_types = FALSE,
    name_repair    = "unique"   # renombra automáticamente columnas duplicadas
    # Publisher → Publisher...6 y Publisher...24
  )
  
  cat("  Filas leídas:", nrow(sjr_raw), "| Columnas:", ncol(sjr_raw), "\n")
  
  # ── PASO 2: Seleccionar solo columnas necesarias por nombre exacto ───────────
  # Usamos matches() con regex para no depender del orden exacto de columnas
  sjr <- sjr_raw %>%
    select(
      Title         = Title,
      Issn          = Issn,
      SJR_raw       = SJR,
      SJR_Q_raw     = `SJR Best Quartile`,
      H_index       = `H index`,
      OA            = `Open Access`,
      Country       = Country,
      Areas         = Areas,
      Categories    = Categories
    )
  
  # ── PASO 3: Corregir SJR_Q — filtrar solo Q1/Q2/Q3/Q4 ──────────────────────
  # Problema: cuando readr parsea mal la coma decimal, el número SJR queda
  # en la columna SJR_Best_Quartile. Ejemplo: "18,663" → SJR_Q = "18,663"
  sjr <- sjr %>%
    mutate(
      SJR_Best_Quartile = case_when(
        SJR_Q_raw %in% c("Q1", "Q2", "Q3", "Q4") ~ SJR_Q_raw,
        TRUE                                       ~ NA_character_
        # Los ~291 registros con número en vez de Q quedan como NA
        # (son revistas sin cuartil asignado por SCImago ese año)
      )
    ) %>%
    select(-SJR_Q_raw)
  
  # ── PASO 4: Convertir SJR a numérico (coma decimal → punto) ─────────────────
  sjr <- sjr %>%
    mutate(
      SJR = as.numeric(str_replace(as.character(SJR_raw), ",", "."))
    ) %>%
    select(-SJR_raw)
  
  # ── PASO 5: Separar y normalizar ISSNs ──────────────────────────────────────
  # El CSV guarda ISSNs SIN guión y pueden ser dos: "15424863, 00079235"
  # Los normalizamos al formato estándar CON guión: "1542-4863"
  # porque así es como vienen en bibliometrix (campo SN)
  
  normalizar_issn <- function(issn_str) {
    # Extrae un ISSN (8 dígitos con o sin guión, X permitida al final)
    raw <- str_trim(str_extract(issn_str, "[0-9]{4}-?[0-9]{3}[0-9Xx]"))
    if (is.na(raw) || nchar(raw) == 0) return(NA_character_)
    # Quitar guión si ya lo tenía y reformatear siempre con guión
    digits <- str_remove_all(raw, "-")
    if (nchar(digits) != 8) return(NA_character_)
    paste0(substr(digits, 1, 4), "-", substr(digits, 5, 8))
  }
  
  # Dividir el campo Issn en dos (pueden venir hasta 2 ISSNs separados por coma)
  sjr <- sjr %>%
    mutate(
      issn_part1 = str_trim(str_extract(Issn, "^[^,]+")),
      issn_part2 = str_trim(str_extract(Issn, "(?<=,)[^,]+")),
      ISSN1      = sapply(issn_part1, normalizar_issn, USE.NAMES = FALSE),
      ISSN2      = sapply(issn_part2, normalizar_issn, USE.NAMES = FALSE)
    ) %>%
    select(-issn_part1, -issn_part2)
  
  # ── PASO 6: Resumen ───────────────────────────────────────────────────────────
  cat("  Total revistas     :", nrow(sjr), "\n")
  cat("  Con cuartil Q1-Q4  :", sum(!is.na(sjr$SJR_Best_Quartile)), "\n")
  cat("  Sin cuartil (NA)   :", sum( is.na(sjr$SJR_Best_Quartile)), "\n")
  cat("  Con ISSN1 válido   :", sum(!is.na(sjr$ISSN1)), "\n")
  cat("  Con ISSN2 válido   :", sum(!is.na(sjr$ISSN2)), "\n")
  cat("\n  Distribución por cuartil:\n")
  print(table(sjr$SJR_Best_Quartile, useNA = "no"))
  cat("\n")
  
  return(sjr)
}


# =============================================================================
# FUNCIÓN 2: asignar_cuartil_sjr() — corregida
# =============================================================================
#
# CAMBIOS respecto a la versión anterior:
#  - El JOIN ahora funciona porque ambos lados tienen ISSNs con guión
#  - Se añade también el campo OA (Open Access) del ranking SCImago
#  - Manejo explícito cuando la columna SN tiene formato "XXXX-XXXX eISSN XXXX-XXXX"

asignar_cuartil_sjr <- function(df, sjr_df, col_issn = "SN") {
  
  cat("=== ASIGNANDO CUARTILES SJR ===\n")
  
  # ── Preparar tabla lookup: ISSN (con guión) → cuartil ──────────────────────
  # Duplicamos las filas para que cada ISSN (impreso y electrónico) tenga su fila
  lookup <- bind_rows(
    sjr_df %>%
      filter(!is.na(ISSN1) & ISSN1 != "") %>%
      select(ISSN        = ISSN1,
             SJR_Q       = SJR_Best_Quartile,
             SJR_score   = SJR,
             SJR_H       = H_index,
             SJR_OA      = OA,
             SJR_Areas   = Areas,
             SJR_Cats    = Categories),
    sjr_df %>%
      filter(!is.na(ISSN2) & ISSN2 != "") %>%
      select(ISSN        = ISSN2,
             SJR_Q       = SJR_Best_Quartile,
             SJR_score   = SJR,
             SJR_H       = H_index,
             SJR_OA      = OA,
             SJR_Areas   = Areas,
             SJR_Cats    = Categories)
  ) %>%
    filter(!is.na(ISSN)) %>%
    # Si hay duplicados (mismo ISSN en ISSN1 e ISSN2 de distintas filas),
    # quedarse con el de mayor SJR_score
    arrange(ISSN, desc(SJR_score)) %>%
    distinct(ISSN, .keep_all = TRUE)
  
  cat("  Revistas en tabla lookup:", nrow(lookup), "\n")
  
  # ── Normalizar el ISSN de la base bibliométrica ──────────────────────────────
  # En bibliometrix el campo SN puede venir como:
  #   "1542-4863"                    → un solo ISSN
  #   "1542-4863 1097-4172"          → dos ISSNs separados por espacio
  #   "1542-4863; 1097-4172"         → dos ISSNs separados por punto y coma
  # Extraemos el primero siempre y verificamos formato
  
  df <- df %>%
    mutate(
      ISSN_norm = {
        sn <- trimws(.data[[col_issn]])
        # Extraer primer ISSN con guión (formato estándar)
        issn1 <- str_extract(sn, "[0-9]{4}-[0-9]{3}[0-9Xx]")
        # Si no tiene guión, buscar 8 dígitos seguidos y formatear
        issn_raw <- str_extract(sn, "[0-9]{7}[0-9Xx]")
        issn_fmt <- ifelse(
          !is.na(issn_raw),
          paste0(substr(issn_raw, 1, 4), "-", substr(issn_raw, 5, 8)),
          NA_character_
        )
        # Usar el que tenga guión, si no usar el formateado
        ifelse(!is.na(issn1), issn1, issn_fmt)
      }
    )
  
  cat("  Registros con ISSN válido en base:", sum(!is.na(df$ISSN_norm)), "\n")
  
  # ── JOIN ────────────────────────────────────────────────────────────────────
  df <- df %>%
    left_join(lookup, by = c("ISSN_norm" = "ISSN")) %>%
    select(-ISSN_norm)
  
  # ── Resumen ─────────────────────────────────────────────────────────────────
  cat("\n  RESULTADO:\n")
  cat("  Con cuartil asignado:", sum(!is.na(df$SJR_Q)), "\n")
  cat("  Sin cuartil (NA)    :", sum( is.na(df$SJR_Q)), "\n")
  cat("\n  Distribución por cuartil:\n")
  print(table(df$SJR_Q, useNA = "ifany"))
  cat("\n")
  
  return(df)
}


# NO FUNCIONAL =============================================================================

descargar_sjr <- function(anio = 2023) {
  url_sjr <- paste0("https://www.scimagojr.com/journalrank.php?year=", anio,
                    "&out=xls")
  ruta_local <- paste0("data/scimagojr 2023.csv", anio, ".csv")

  if (!file.exists(ruta_local)) {
    cat("Descargando SJR", anio, "...\n")
    # SCImago exporta como CSV con separador ";"
    download.file(url_sjr, destfile = ruta_local, mode = "wb", quiet = TRUE)
    cat("  ✓ Guardado:", ruta_local, "\n")
  }

  sjr <- tryCatch(
    readr::read_delim(ruta_local, delim = ";",
                      locale         = readr::locale(decimal_mark = ",",
                                                     encoding     = "UTF-8"),
                      show_col_types = FALSE),
    error = function(e) {
      readr::read_delim(ruta_local, delim = ";",
                        locale         = readr::locale(decimal_mark = ",",
                                                       encoding     = "latin1"),
                        show_col_types = FALSE)
    }
  )

  # Normalizar columnas de interés
  sjr <- sjr %>%
    rename_with(~ gsub(" ", "_", .)) %>%
    select(Title, Issn, SJR_Best_Quartile, SJR, H_index,
           matches("Total_Doc")) %>%
    mutate(
      # El ISSN viene como "XXXX-XXXX, YYYY-YYYY" (puede haber dos)
      ISSN1 = str_trim(str_extract(Issn, "\\d{4}-[\\dXx]{4}")),
      ISSN2 = str_trim(str_extract(str_replace(Issn, "^[^,]+,", ""), "\\d{4}-[\\dXx]{4}"))
    )

  cat("SJR", anio, "cargado:", nrow(sjr), "revistas\n")
  return(sjr)
}

asignar_cuartil_sjr <- function(df, sjr_df, col_issn = "SN", col_anio = "PY") {

  cat("=== ASIGNANDO CUARTILES SJR ===\n")

  # Preparar tabla de lookup: ISSN → cuartil
  lookup <- bind_rows(
    sjr_df %>% select(ISSN = ISSN1, SJR_Q = SJR_Best_Quartile,
                      SJR_score = SJR, SJR_H = H_index),
    sjr_df %>% select(ISSN = ISSN2, SJR_Q = SJR_Best_Quartile,
                      SJR_score = SJR, SJR_H = H_index)
  ) %>% filter(!is.na(ISSN) & ISSN != "") %>%
    distinct(ISSN, .keep_all = TRUE)

  # Normalizar ISSN en la base fusionada
  df <- df %>%
    mutate(
      ISSN_norm = str_trim(str_extract(df[[col_issn]], "\\d{4}-[\\dXx]{4}"))
    ) %>%
    left_join(lookup, by = c("ISSN_norm" = "ISSN")) %>%
    select(-ISSN_norm)

  cat("  Con cuartil asignado:", sum(!is.na(df$SJR_Q)), "\n")
  cat("  Sin cuartil          :", sum( is.na(df$SJR_Q)), "\n")
  print(table(df$SJR_Q, useNA = "ifany"))
  cat("\n")

  return(df)
}



# =============================================================================
# BLOQUE 2C — ESTADO OPEN ACCESS VÍA UNPAYWALL (GRATIS)
# =============================================================================
# Unpaywall determina si un artículo tiene versión gratuita accesible.
# Requiere solo un email de contacto (para rate limiting cortés).
# Documentación: https://unpaywall.org/products/api
# =============================================================================

unpaywall_doi <- function(doi, email = "fzela@unsa.edu.pe", pausa = 0.1) {
  if (is.na(doi) || trimws(doi) == "") return(NULL)

  doi_limpio <- sub("^https?://(dx\\.)?doi\\.org/", "", doi, ignore.case = TRUE)
  url        <- paste0("https://api.unpaywall.org/v2/",
                       utils::URLencode(doi_limpio),
                       "?email=", email)

  resp <- tryCatch(
    httr::GET(url, httr::timeout(8)),
    error = function(e) NULL
  )
  Sys.sleep(pausa)

  if (is.null(resp) || httr::status_code(resp) != 200) return(NULL)
  d <- tryCatch(
    jsonlite::fromJSON(httr::content(resp, as="text", encoding="UTF-8"),
                       simplifyVector = TRUE),
    error = function(e) NULL
  )
  if (is.null(d)) return(NULL)

  tibble::tibble(
    doi_input        = doi,
    UW_es_oa         = as.logical(d$is_oa %||% NA),
    UW_tipo_oa       = as.character(d$oa_status %||% NA),
    # gold=en la propia revista; green=repositorio; bronze=gratis sin licencia
    UW_url_mejor     = as.character(d$best_oa_location$url %||% NA),
    UW_fuente_mejor  = as.character(d$best_oa_location$host_type %||% NA)
  )
}

enriquecer_oa <- function(df, col_doi = "DI", email = "fzela@unsa.edu.pe") {
  cat("=== ESTADO OPEN ACCESS (Unpaywall) ===\n")
  n_doi <- sum(!is.na(df[[col_doi]]) & df[[col_doi]] != "")
  cat("  DOIs a consultar:", n_doi, "\n")

  resultados <- df %>%
    filter(!is.na(.data[[col_doi]]) & .data[[col_doi]] != "") %>%
    pull(col_doi) %>%
    map(~ unpaywall_doi(.x, email)) %>%
    bind_rows()

  if (nrow(resultados) > 0)
    df <- left_join(df, resultados, by = setNames("doi_input", col_doi))

  cat("  Open Access confirmados:", sum(df$UW_es_oa == TRUE, na.rm = TRUE), "\n")
  cat("  Distribución OA:\n")
  print(table(df$UW_tipo_oa, useNA = "ifany"))
  cat("\n")

  return(df)
}



# =============================================================================
# BLOQUE 2D — NORMALIZACIÓN DE FILIACIONES HOSPITALARIAS
# =============================================================================
# Problema real: el mismo hospital aparece de 10+ formas distintas en Scopus.
# Ejemplo: "Hosp Nacl Cayetano Heredia" / "Hospital Cayetano Heredia" /
#          "H.N. Cayetano Heredia" / "HNCH" → todos son el mismo hospital.
#
# ESTRATEGIA:
#  1. Lista de alias conocidos por hospital
#  2. Asignación automática con detección por palabras clave
#  3. Revisión manual de no asignados
# =============================================================================

# ── Diccionario de aliases para los 50 hospitales del proyecto ───────────────
diccionario_hospitales <- list(

  "Hospital Goyeneche" = c(
    "Goyeneche", "Hospital III Goyeneche", "Hospital Regional Goyeneche",
    "H.R. Goyeneche", "Hosp Goyeneche"
  ),

  "HNCASE (Seguin Escobedo)" = c(
    "Carlos Alberto Seguín Escobedo", "Seguín Escobedo", "HNCASE",
    "Seguin Escobedo", "Hospital Nacional Carlos A. Seguín", "CASE"
  ),

  "HRHDE (Honorio Delgado)" = c(
    "Honorio Delgado", "HRHDE", "Hospital Regional Honorio Delgado",
    "Hosp Honorio Delgado", "H.R.H.D. Espinoza"
  ),

  "HNC (Daniel Alcides Carrion - Lima)" = c(
    "Daniel A. Carrion", "Daniel Alcides Carrion", "HNC", "HNDC",
    "Hospital Nacional Daniel", "Carrion"
  ),

  "Centro Medico Naval" = c(
    "Centro Medico Naval", "CMN", "Naval", "CMST", "Santiago Tavara",
    "Hospital Naval", "Centro Medico Naval Tavara"
  ),

  "HASS (Sabogal)" = c(
    "Alberto Sabogal", "Sabogal", "HASS", "Hospital Sabogal", "Sabogal Sologuren",
    "Red Asistencial Sabogal"
  ),

  "HRCU (Cusco)" = c(
    "Hospital Regional del Cusco", "Cusco Regional", "HRCU",
    "Hospital de Apoyo Cusco", "Hospital Regional Cusco"
  ),

  "Antonio Lorena" = c(
    "Antonio Lorena", "Lorena", "Hospital Antonio Lorena",
    "H. Antonio Lorena", "Hosp. Antonio Lorena del Cusco"
  ),

  "EsSalud Cusco (Guevara Velasco)" = c(
    "Adolfo Guevara", "Guevara Velasco", "EsSalud Cusco",
    "Hospital Nacional Adolfo Guevara"
  ),

  "HNRPP (Ramiro Priale)" = c(
    "Ramiro Priale", "Priale", "HNRPP", "Hospital Nacional Ramiro Priale",
    "Hosp. Ramiro Priale"
  ),

  "Hospital Belen Trujillo" = c(
    "Belen", "Belén de Trujillo", "HBT", "Hospital Belen",
    "Hospital Belén", "Hosp Belen Trujillo"
  ),

  "HVLE (Victor Lazarte)" = c(
    "Victor Lazarte", "Lazarte", "HVLE", "Víctor Lazarte Echegaray",
    "Hospital Victor Lazarte"
  ),

  "HRDT (Regional Trujillo)" = c(
    "Regional Docente de Trujillo", "HRDT", "Hospital Regional Trujillo",
    "Regional de Trujillo", "Docente Trujillo"
  ),

  "HRL (Lambayeque)" = c(
    "Hospital Regional Lambayeque", "HRL", "Lambayeque Regional",
    "Hosp. Reg. Lambayeque"
  ),

  "HNAAA (Almanzor Aguinaga)" = c(
    "Almanzor Aguinaga", "HNAAA", "Aguinaga Asenjo",
    "Hospital Almanzor Aguinaga", "EsSalud Chiclayo"
  ),

  "Clinica San Pablo" = c(
    "San Pablo", "Clínica San Pablo", "Clinica San Pablo SAC",
    "C. San Pablo"
  ),

  "HNHU (Hipolito Unanue)" = c(
    "Hipólito Unanue", "Hipolito Unanue", "HNHU",
    "Hospital Nacional Hipólito Unanue", "Hosp. Hipolito Unanue"
  ),

  "HMA (Maria Auxiliadora)" = c(
    "María Auxiliadora", "Maria Auxiliadora", "HMA",
    "Hospital María Auxiliadora", "Hosp. Maria Auxiliadora"
  ),

  "HNDM (Dos de Mayo)" = c(
    "Dos de Mayo", "HNDM", "Hospital Dos de Mayo",
    "Hospital Nacional Dos de Mayo", "H.N. Dos de Mayo"
  ),

  "HNCH (Cayetano Heredia)" = c(
    "Cayetano Heredia", "HNCH", "Hospital Cayetano Heredia",
    "Hospital Nacional Cayetano Heredia", "H.N. Cayetano Heredia",
    "Hosp. Cayetano Heredia"
  ),

  "HNSEB (Sergio Bernales)" = c(
    "Sergio Bernales", "Bernales", "HNSEB",
    "Hospital Nacional Sergio E. Bernales", "Hospital Bernales"
  ),

  "Clinica Ricardo Palma" = c(
    "Ricardo Palma", "Clínica Ricardo Palma", "Clinica Ricardo Palma",
    "C. Ricardo Palma"
  ),

  "Hospital FAP" = c(
    "Fuerza Aérea", "Fuerza Aerea", "FAP", "Hospital FAP",
    "Hospital Central FAP", "Hospital Central de la Fuerza Aerea"
  ),

  "Hospital Militar" = c(
    "Militar", "Luis Arias Schreiber", "Hospital Militar",
    "Hospital Militar Central", "Schreiber"
  ),

  "HEVES (Villa El Salvador)" = c(
    "Villa El Salvador", "HEVES", "Hospital Villa El Salvador",
    "Hospital de Emergencias Villa El Salvador"
  ),

  "Clinica Delgado (AUNA)" = c(
    "Delgado", "Clínica Delgado", "AUNA", "AUNA Clínica Delgado",
    "Clinica Delgado"
  ),

  "HNAL (Loayza)" = c(
    "Arzobispo Loayza", "Loayza", "HNAL",
    "Hospital Nacional Arzobispo Loayza", "H.N. Loayza"
  ),

  "HASR (Santa Rosa)" = c(
    "Santa Rosa", "HASR", "Hospital Santa Rosa", "Hospital de Apoyo Santa Rosa"
  ),

  "Hospital PNP (Luis Saenz)" = c(
    "Luis N. Sáenz", "Luis Saenz", "PNP", "Hospital PNP",
    "Hospital de la PNP", "Policía Nacional"
  ),

  "Clinica San Borja" = c(
    "San Borja", "Clínica San Borja", "Clinica San Borja", "CSB"
  ),

  "Clinica San Felipe" = c(
    "San Felipe", "Clínica San Felipe", "Clinica San Felipe"
  ),

  "Hospital Iquitos" = c(
    "Iquitos", "Hospital III Iquitos", "Hospital Regional Iquitos"
  ),

  "HRLI (Loreto Felipe Arriola)" = c(
    "Felipe Santiago Arriola", "Arriola Iglesias", "HRLI",
    "Hospital Regional de Loreto", "Hospital Loreto"
  ),

  "Hospital Piura (Cayetano Heredia)" = c(
    "Piura", "José Cayetano Heredia", "Hospital III Piura",
    "Hospital José Cayetano Heredia", "EsSalud Piura"
  ),

  "IREN Sur" = c(
    "IREN Sur", "IREN SUR", "Instituto Regional de Enfermedades Neoplasicas del Sur",
    "IREN Arequipa"
  ),

  "IREN Norte (Pinillos Ganoza)" = c(
    "IREN Norte", "IREN NORTE", "Luis Pinillos Ganoza", "Pinillos Ganoza",
    "Instituto Regional de Enfermedades Neoplasicas Norte"
  ),

  "IRO (Justo Sierra Urrego)" = c(
    "IRO", "Instituto Regional de Oncología", "Justo Sierra"
  ),

  "INO (Oftalmologia)" = c(
    "Oftalmología", "INO", "Instituto Nacional de Oftalmología",
    "Contreras Campos", "Francisco Contreras"
  ),

  "INMP (Materno Perinatal)" = c(
    "Materno Perinatal", "INMP", "Instituto Nacional Materno Perinatal",
    "Instituto Materno Perinatal", "INMP Lima"
  ),

  "INCN (Ciencias Neurologicas)" = c(
    "Ciencias Neurológicas", "INCN", "Instituto Nacional de Ciencias Neurologicas",
    "Instituto Neurológico", "Neurociencias"
  ),

  "INEN" = c(
    "INEN", "Instituto Nacional de Enfermedades Neoplásicas",
    "Instituto Neoplasicas", "National Cancer Institute Peru",
    "INEN Lima", "Enfermedades Neoplasicas"
  ),

  "HNGAI (Guillermo Almenara)" = c(
    "Guillermo Almenara", "Almenara", "HNGAI",
    "Hospital Nacional Guillermo Almenara", "H.N. Guillermo Almenara Irigoyen"
  ),

  "Clinica del Inca" = c(
    "Clínica del Inca", "Clinica del Inca", "C. del Inca"
  ),

  "HNERM (Rebagliati)" = c(
    "Rebagliati", "HNERM", "Edgardo Rebagliati",
    "Hospital Nacional Edgardo Rebagliati", "Hospital Rebagliati Martins",
    "H.N.E.R.M."
  ),

  "INSNSB (INSN San Borja)" = c(
    "INSN San Borja", "INSNSB", "Instituto Nacional de Salud del Niño San Borja",
    "INSN-SB", "San Borja Pediatrico"
  ),

  "INSN (Instituto Nacional Salud del Niño)" = c(
    "INSN", "Instituto Nacional de Salud del Niño",
    "Instituto Salud del Niño", "Breña", "INSN Breña"
  ),

  "INSM (Salud Mental)" = c(
    "Honorio Delgado Hideyo Noguchi", "INSM", "Salud Mental",
    "Instituto Nacional de Salud Mental", "Hideyo Noguchi"
  ),

  "IML (Medicina Legal)" = c(
    "Medicina Legal", "IML", "Leonidas Avendaño", "Avendaño Ureta",
    "Instituto de Medicina Legal"
  ),

  "Hospital Lazo Peralta" = c(
    "Lazo Peralta", "Víctor Alfredo Lazo", "Hospital Lazo Peralta"
  ),

  "HNDAC (Centro - Tarma)" = c(
    "Hospital Nacional del Centro", "Daniel Alcides Carrión Tarma",
    "HNDAC", "Hospital del Centro", "Hospital Tarma"
  )
)

# ── Función: asignar hospital normalizado a cada registro ────────────────────
asignar_hospital <- function(df, col_afil = "C1") {

  cat("=== NORMALIZACIÓN DE FILIACIONES ===\n")

  df$Hospital_norm    <- NA_character_
  df$Hospital_n_match <- 0L

  for (i in seq_len(nrow(df))) {
    afil_i <- toupper(trimws(df[[col_afil]][i] %||% ""))
    if (is.na(afil_i) || afil_i == "") next

    matches <- character(0)
    for (hosp_nombre in names(diccionario_hospitales)) {
      aliases <- toupper(diccionario_hospitales[[hosp_nombre]])
      if (any(str_detect(afil_i, fixed(aliases, ignore_case = TRUE)))) {
        matches <- c(matches, hosp_nombre)
      }
    }

    if (length(matches) > 0) {
      df$Hospital_norm[i]    <- paste(unique(matches), collapse = " | ")
      df$Hospital_n_match[i] <- length(unique(matches))
    }
  }

  cat("  Con hospital asignado:", sum(!is.na(df$Hospital_norm)), "\n")
  cat("  Sin asignación       :", sum( is.na(df$Hospital_norm)), "\n")
  cat("  Multi-hospital       :", sum(df$Hospital_n_match > 1, na.rm = TRUE), "\n")
  cat("\n  Top 10 hospitales por frecuencia:\n")

  top10 <- df %>%
    filter(!is.na(Hospital_norm)) %>%
    separate_rows(Hospital_norm, sep = " \\| ") %>%
    count(Hospital_norm, sort = TRUE) %>%
    slice_head(n = 10)
  print(top10)
  cat("\n")

  return(df)
}



# =============================================================================
# BLOQUE 3 — PIPELINE MAESTRO COMPLETO
# =============================================================================
# Llama a todas las funciones anteriores en el orden correcto.
# Configura tus datos en la sección de parámetros y ejecuta línea a línea.
# =============================================================================

# ── PARÁMETROS (editar según tu proyecto) ─────────────────────────────────────
MI_EMAIL          <- "fzela@unsa.edu.pe"          # para Crossref/Unpaywall/OpenAlex
SCOPUS_API_KEY    <- Sys.getenv("SCOPUS_API_KEY")  # o pon tu key directamente
AÑO_SJR           <- 2023                         # año del ranking SJR a usar

if (SCOPUS_API_KEY == "") SCOPUS_API_KEY <- "3925b30e9cbd00208d4f3b3a5dcbf48f"

# ── CARGAR BASE FUSIONADA (si ya la tienes guardada) ─────────────────────────
base_fusionada <- readRDS("resultados/base_fusionada.rds")

# ── PASO 1: Completar DOIs faltantes (CrossRef) ──────────────────────────────
cat("PASO 1: Completando DOIs faltantes...\n")
base_fusionada <- completar_dois(base_fusionada, email = MI_EMAIL)

# ── PASO 2: Citas Scopus API (más exactas, requiere IP institucional) ─────────
cat("PASO 2: Citas Scopus API...\n")
# (ya incluido en el script anterior — omitir si ya lo corriste)

# ── PASO 3: Citas OpenAlex (gratis, sin IP institucional) ────────────────────
cat("PASO 3: Citas OpenAlex...\n")
base_fusionada <- enriquecer_openalex(base_fusionada, email = MI_EMAIL)

# ── PASO 4: Estado Open Access (Unpaywall) ───────────────────────────────────
cat("PASO 4: Estado Open Access...\n")
base_fusionada <- enriquecer_oa(base_fusionada, email = MI_EMAIL)

# ── PASO 5: Cuartiles SJR ────────────────────────────────────────────────────
cat("PASO 5: Cuartiles SJR...\n")
sjr_data       <- descargar_sjr(AÑO_SJR)
base_fusionada <- asignar_cuartil_sjr(base_fusionada, sjr_data)
# ── Cargar SJR desde el archivo que ya tienes ────────────────────────────────
sjr_data <- cargar_sjr(
  ruta_csv = "data/scimagojr 2023.csv",   # ← ruta donde guardaste el archivo
  anio     = 2023
)

# ── Asignar cuartiles a tu base fusionada ────────────────────────────────────
base_fusionada <- asignar_cuartil_sjr(
  df      = base_fusionada,
  sjr_df  = sjr_data,
  col_issn = "SN"        # nombre de la columna ISSN en tu base bibliometrix
)

# ── Verificación rápida ───────────────────────────────────────────────────────
cat("Top 10 revistas por cuartil en tu base:\n")
base_fusionada %>%
  filter(!is.na(SJR_Q)) %>%
  count(SO, SJR_Q, SJR_score) %>%
  arrange(SJR_Q, desc(SJR_score)) %>%
  slice_head(n = 10) %>%
  print()

head(base_fusionada)

# ── PASO 6: Normalización de filiaciones hospitalarias ───────────────────────
cat("PASO 6: Normalizando filiaciones...\n")
base_fusionada <- asignar_hospital(base_fusionada)

# ── PASO 7: Tabla comparativa de citas (todas las fuentes) ───────────────────
cat("PASO 7: Consolidando indicadores de citas...\n")

base_fusionada <- base_fusionada %>%
  mutate(
    TC_original  = as.integer(TC),
    # Usar citas en este orden de prioridad: Scopus API > OpenAlex > original
    TC_final = case_when(
      !is.na(TC_API)  & TC_API  >= 0 ~ TC_API,
      !is.na(OA_citas)              ~ OA_citas,
      TRUE                           ~ TC_original
    ),
    TC_fuente_final = case_when(
      !is.na(TC_API)  & TC_API  >= 0 ~ "Scopus_API",
      !is.na(OA_citas)              ~ "OpenAlex",
      TRUE                           ~ "Exportacion_original"
    )
  )

# ── PASO 8: Exportar Excel enriquecido ───────────────────────────────────────
cat("PASO 8: Exportando Excel final enriquecido...\n")

wb <- createWorkbook()

# Hoja principal
addWorksheet(wb, "Base_enriquecida")
writeData(wb, "Base_enriquecida", base_fusionada %>%
            select(
              # Identificación
              TI, AU, PY, SO, DI, DT, LA,
              # Citas — todas las fuentes
              TC_original, TC_API, OA_citas, TC_final, TC_fuente_final,
              # Impacto de revista
              SJR_Q, SJR_score, SJR_H,
              # Open Access
              UW_es_oa, UW_tipo_oa, UW_url_mejor,
              # Temas (OpenAlex)
              OA_conceptos,
              # Filiación normalizada
              Hospital_norm, Hospital_n_match,
              # Trazabilidad
              SOURCE_DB, Bases_indexadoras, N_bases,
              # IDs
              UT, SN
            ))

# Hoja de comparación de citas
addWorksheet(wb, "Comparacion_citas")
writeData(wb, "Comparacion_citas", base_fusionada %>%
            select(TI, DI, PY, TC_original, TC_API, OA_citas, TC_final, TC_fuente_final) %>%
            arrange(desc(TC_final)))

# Hoja resumen Open Access
addWorksheet(wb, "Open_Access")
writeData(wb, "Open_Access",
          base_fusionada %>%
            count(UW_tipo_oa, SJR_Q) %>%
            arrange(UW_tipo_oa, SJR_Q))

# Hoja resumen por hospital
addWorksheet(wb, "Por_Hospital")
writeData(wb, "Por_Hospital",
          base_fusionada %>%
            filter(!is.na(Hospital_norm)) %>%
            group_by(Hospital_norm) %>%
            summarise(
              N_articulos    = n(),
              Citas_total    = sum(TC_final, na.rm = TRUE),
              Citas_promedio = round(mean(TC_final, na.rm = TRUE), 1),
              Pct_OA         = round(mean(UW_es_oa == TRUE, na.rm = TRUE) * 100, 1),
              Pct_Q1Q2       = round(mean(SJR_Q %in% c("Q1","Q2"), na.rm = TRUE) * 100, 1),
              .groups = "drop"
            ) %>%
            arrange(desc(N_articulos)))

saveWorkbook(wb, "resultados/base_enriquecida_final.xlsx", overwrite = TRUE)
saveRDS(base_fusionada, "resultados/base_fusionada_enriquecida.rds")

cat("\n=== PIPELINE COMPLETO TERMINADO ===\n")
cat("Archivo: resultados/base_enriquecida_final.xlsx\n\n")
cat("COLUMNAS NUEVAS AÑADIDAS:\n")
cat("  TC_API           → citas Scopus (tiempo real, API)\n")
cat("  OA_citas         → citas OpenAlex (gratis)\n")
cat("  TC_final         → citas consolidadas (mejor fuente disponible)\n")
cat("  UW_es_oa         → ¿es Open Access? (TRUE/FALSE)\n")
cat("  UW_tipo_oa       → tipo OA (gold/green/bronze/closed)\n")
cat("  UW_url_mejor     → URL al PDF gratuito\n")
cat("  SJR_Q            → cuartil de la revista (Q1/Q2/Q3/Q4)\n")
cat("  SJR_score        → puntuación SJR de la revista\n")
cat("  OA_conceptos     → temas asignados por OpenAlex\n")
cat("  Hospital_norm    → nombre normalizado del hospital\n")
cat("  DI (completado)  → DOIs recuperados por CrossRef donde faltaban\n")

