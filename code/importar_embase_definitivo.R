# =============================================================================
# IMPORTADOR DEFINITIVO DE EMBASE PARA BIBLIOMETRIX
# Autor: Frank Zela-Coila
# Versión: 3.0 — Marzo 2026
#
# ANÁLISIS REAL DE TUS ARCHIVOS:
# ─────────────────────────────────────────────────────────────────────────────
# embase.ris  → 231 registros │ Formato RIS estándar │ MEJOR OPCIÓN ✓
# embase.txt  → 231 registros │ Formato propietario Embase (CRLF)
# embase.csv  → 231 registros │ Formato transpuesto (campos como filas)
#
# HALLAZGOS CLAVE:
# • Embase NO exporta número de citas en ningún formato → TC siempre = 0
#   → Las citas reales se recuperarán con la API de Scopus (Bloque B)
# • Tags RIS de Embase difieren del estándar: A1=Autor, M1=Afiliación, etc.
# • 123/231 registros tienen DOI │ 108/231 solo tienen título
# • Tipos de doc: M3 (Article, Review, Letter, Conference Abstract, etc.)
# =============================================================================



# ─────────────────────────────────────────────────────────────────────────────
# BLOQUE A — PARSER RIS PERSONALIZADO PARA EMBASE
# ─────────────────────────────────────────────────────────────────────────────

importar_embase_ris_definitivo <- function(ruta_ris) {

  cat("=== IMPORTANDO EMBASE RIS ===\n")
  cat("Archivo:", ruta_ris, "\n\n")

  # ── 1. Leer el archivo completo ────────────────────────────────────────────
  lineas <- readLines(ruta_ris, encoding = "UTF-8", warn = FALSE)

  # ── 2. Dividir en bloques por registro (cada registro empieza con "TY  - ")
  starts <- grep("^TY  - ", lineas)
  ends   <- grep("^ER  - ", lineas)

  if (length(starts) != length(ends)) {
    stop("El RIS tiene ", length(starts), " inicios y ", length(ends),
         " finales. Archivo posiblemente corrupto.")
  }
  cat("Registros detectados:", length(starts), "\n")

  # ── 3. Función para parsear UN registro ───────────────────────────────────
  parsear_registro <- function(bloque) {

    # Extraer valor de un tag (puede haber múltiples → devuelve el primero)
    get1 <- function(tag) {
      patron <- paste0("^", tag, "  - (.+)$")
      hits   <- regmatches(bloque, regexpr(patron, bloque, perl = TRUE))
      if (length(hits) == 0) return(NA_character_)
      sub(patron, "\\1", hits[1], perl = TRUE)
    }

    # Extraer TODOS los valores de un tag y unirlos con " ; "
    getAll <- function(tag, sep = " ; ") {
      patron <- paste0("^", tag, "  - (.+)$")
      hits   <- regmatches(bloque, regexpr(patron, bloque, perl = TRUE))
      vals   <- sub(patron, "\\1", hits, perl = TRUE)
      # Repetir para múltiples líneas con el mismo tag
      all_hits <- grep(patron, bloque, value = TRUE, perl = TRUE)
      all_vals <- sub(patron, "\\1", all_hits, perl = TRUE)
      if (length(all_vals) == 0) return(NA_character_)
      paste(all_vals, collapse = sep)
    }

    # ── Mapeo de tags Embase RIS → campos bibliometrix ──────────────────────
    #
    # TAG EMBASE │ SIGNIFICADO                   │ CAMPO BIBLIOMETRIX
    # ──────────────────────────────────────────────────────────────
    # TY         │ Tipo de registro               │ (auxiliar)
    # M3         │ Subtipo de documento (Article) │ DT
    # T1         │ Título                         │ TI
    # A1         │ Autores (uno por línea)        │ AU
    # M1         │ Afiliaciones (una por línea)   │ C1  ← TAG NO ESTÁNDAR EMBASE
    # AD         │ Afiliación alternativa         │ C1
    # Y1         │ Año de publicación             │ PY
    # JF         │ Nombre completo revista        │ SO
    # JO         │ Abreviatura revista            │ J9
    # SN         │ ISSN                           │ SN
    # VL         │ Volumen                        │ VL
    # IS         │ Número/Issue                   │ IS
    # SP         │ Página inicio                  │ SP
    # EP         │ Página fin                     │ EP
    # DO         │ DOI                            │ DI
    # N2         │ Resumen/Abstract               │ AB
    # KW         │ Palabras clave (todas)         │ DE (autor) + ID (EMTREE)
    # LA         │ Idioma                         │ LA
    # U2         │ Número acceso Embase (L...)    │ UT
    # C5         │ PubMed ID                      │ PM
    # DB         │ Base de datos fuente           │ (info adicional)
    # L2         │ URL del DOI                    │ (auxiliar)
    # UR         │ URL Embase                     │ (auxiliar)
    # U3         │ Fecha entrada AiP              │ (auxiliar)

    tipo_doc <- get1("M3")
    if (is.na(tipo_doc)) tipo_doc <- get1("TY")

    # Autores: múltiples A1 → "APELLIDO, N.;APELLIDO2, N."
    autores_raw <- {
      patron_a1 <- "^A1  - (.+)$"
      hits_a1   <- grep(patron_a1, bloque, value = TRUE, perl = TRUE)
      if (length(hits_a1) > 0) {
        vals <- sub(patron_a1, "\\1", hits_a1, perl = TRUE)
        # Normalizar: "Apellido, A." → ya está bien en Embase RIS
        toupper(paste(vals, collapse = ";"))
      } else {
        NA_character_
      }
    }

    # Afiliaciones: múltiples M1 (o AD) → pegadas con ";"
    afiliac_raw <- {
      patron_m1 <- "^M1  - (.+)$"
      hits_m1   <- grep(patron_m1, bloque, value = TRUE, perl = TRUE)
      vals_m1   <- sub(patron_m1, "\\1", hits_m1, perl = TRUE)

      patron_ad <- "^AD  - (.+)$"
      hits_ad   <- grep(patron_ad, bloque, value = TRUE, perl = TRUE)
      vals_ad   <- sub(patron_ad, "\\1", hits_ad, perl = TRUE)

      todos     <- c(vals_m1, vals_ad)
      if (length(todos) > 0) paste(todos, collapse = ";") else NA_character_
    }

    # Keywords: múltiples KW → unir con ";"
    # En Embase las KW incluyen tanto Author Keywords como EMTREE
    keywords_raw <- {
      patron_kw <- "^KW  - (.+)$"
      hits_kw   <- grep(patron_kw, bloque, value = TRUE, perl = TRUE)
      vals_kw   <- sub(patron_kw, "\\1", hits_kw, perl = TRUE)
      if (length(vals_kw) > 0) paste(vals_kw, collapse = ";") else NA_character_
    }

    # Páginas combinadas
    sp  <- get1("SP")
    ep  <- get1("EP")
    pp  <- if (!is.na(sp) && !is.na(ep)) paste0(sp, "-", ep) else sp

    # DOI limpio (quitar "https://doi.org/" si viene en L2)
    doi <- get1("DO")
    if (!is.na(doi)) {
      doi <- sub("^https?://(dx\\.)?doi\\.org/", "", doi, ignore.case = TRUE)
      doi <- trimws(doi)
    }

    # Año: solo primeros 4 dígitos de Y1
    anio_raw <- get1("Y1")
    anio     <- if (!is.na(anio_raw)) {
      as.integer(substr(trimws(anio_raw), 1, 4))
    } else NA_integer_

    # UT: número de acceso Embase con prefijo
    acceso_embase <- get1("U2")
    ut <- if (!is.na(acceso_embase)) {
      paste0("EMBASE:", acceso_embase)
    } else NA_character_

    # SR (clave bibliográfica corta): "APELLIDO1 AÑO SO"
    primer_autor <- if (!is.na(autores_raw)) {
      strsplit(autores_raw, ";")[[1]][1]
    } else "UNKNOWN"
    so_corta <- get1("JO")
    if (is.na(so_corta)) so_corta <- get1("JF")
    sr <- paste(primer_autor, anio, so_corta)

    # Construir fila del data frame
    data.frame(
      TI      = toupper(trimws(get1("T1"))),
      AU      = autores_raw,
      AF      = afiliac_raw,          # Nombre completo autores (reusar)
      C1      = afiliac_raw,          # Afiliaciones
      PY      = anio,
      SO      = trimws(get1("JF")),   # Nombre completo revista
      J9      = trimws(get1("JO")),   # Abreviatura revista
      JI      = trimws(get1("JO")),
      SN      = trimws(get1("SN")),
      VL      = trimws(get1("VL")),
      IS      = trimws(get1("IS")),
      SP      = sp,
      EP      = ep,
      PP      = pp,
      DI      = doi,
      AB      = trimws(get1("N2")),
      DE      = keywords_raw,         # Keywords (Author + EMTREE juntas)
      ID      = keywords_raw,         # También en ID para compatibilidad Shiny
      LA      = trimws(get1("LA")),
      DT      = tipo_doc,
      UT      = ut,
      PM      = trimws(get1("C5")),   # PubMed ID
      TC      = 0L,   # Embase NO exporta citas → se completará con API Scopus
      DB      = "EMBASE",
      SR_FULL = paste0(primer_autor, ", ", anio, ", ", so_corta),
      SR      = substr(sr, 1, 40),
      stringsAsFactors = FALSE
    )
  }

  # ── 4. Iterar sobre todos los registros ───────────────────────────────────
  registros <- vector("list", length(starts))

  for (i in seq_along(starts)) {
    bloque_i     <- lineas[starts[i]:ends[i]]
    registros[[i]] <- tryCatch(
      parsear_registro(bloque_i),
      error = function(e) {
        message("  Error en registro #", i, ": ", conditionMessage(e))
        NULL
      }
    )
  }

  # Eliminar registros nulos (por errores)
  registros_ok <- Filter(Negate(is.null), registros)
  embase_df    <- do.call(rbind, registros_ok)

  # ── 5. Limpiar y estandarizar ─────────────────────────────────────────────
  # Quitar filas sin título ni año (registros inválidos)
  embase_df <- embase_df[!is.na(embase_df$TI) & !is.na(embase_df$PY), ]

  # TC como entero (siempre 0 — Embase no exporta citas)
  embase_df$TC <- 0L

  # Marcar origen
  embase_df$SOURCE_DB <- "Embase"

  # ── 6. Resumen ────────────────────────────────────────────────────────────
  cat("\n=== RESUMEN IMPORTACIÓN EMBASE ===\n")
  cat("  Registros importados :", nrow(embase_df), "\n")
  cat("  Con DOI              :", sum(!is.na(embase_df$DI) & embase_df$DI != ""), "\n")
  cat("  Sin DOI              :", sum( is.na(embase_df$DI) | embase_df$DI == ""), "\n")
  cat("  Años                 :", min(embase_df$PY, na.rm=T), "–",
      max(embase_df$PY, na.rm=T), "\n")
  cat("  Con resumen          :", sum(!is.na(embase_df$AB)), "\n")
  cat("  Con afiliación       :", sum(!is.na(embase_df$C1)), "\n")
  cat("  Con PubMed ID (C5)   :", sum(!is.na(embase_df$PM) & embase_df$PM != ""), "\n")
  cat("\n  Tipos de documento:\n")
  print(sort(table(embase_df$DT), decreasing = TRUE))
  cat("\n  NOTA: TC = 0 en todos los registros.\n")
  cat("  Embase no incluye conteo de citas en ningún formato de exportación.\n")
  cat("  → Usa el Bloque B (API Scopus) para obtener citas reales.\n\n")

  return(embase_df)
}



# ─────────────────────────────────────────────────────────────────────────────
# BLOQUE B — ENRIQUECIMIENTO DE CITAS VÍA API SCOPUS
# (Funciona igual para Embase, Scopus, WoS, PubMed, SciELO)
# ─────────────────────────────────────────────────────────────────────────────

# Configura tu API key (recomendado: guardar en .Renviron)
# usethis::edit_r_environ()  → añade: SCOPUS_API_KEY=tu_key_aqui
SCOPUS_API_KEY <- Sys.getenv("SCOPUS_API_KEY")
if (SCOPUS_API_KEY == "") SCOPUS_API_KEY <- "3925b30e9cbd00208d4f3b3a5dcbf48f"

# ── Función: obtener citas por DOI ────────────────────────────────────────────
get_citas_doi <- function(doi, api_key, pausa = 0.4) {
  if (is.na(doi) || trimws(doi) == "") return(NA_integer_)

  url <- paste0("https://api.elsevier.com/content/abstract/doi/",
                utils::URLencode(doi, repeated = TRUE))

  resp <- tryCatch(
    httr::GET(url,
              httr::add_headers("X-ELS-APIKey" = api_key,
                                "Accept"       = "application/json"),
              httr::timeout(12)),
    error = function(e) NULL
  )
  Sys.sleep(pausa)

  if (is.null(resp) || httr::status_code(resp) != 200) {
    if (!is.null(resp) && httr::status_code(resp) == 429) {
      message("  Rate limit → espera 15 seg...")
      Sys.sleep(15)
    }
    return(NA_integer_)
  }

  contenido <- tryCatch(
    jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"),
                       simplifyVector = TRUE),
    error = function(e) NULL
  )
  if (is.null(contenido)) return(NA_integer_)

  tryCatch(
    as.integer(contenido[["abstracts-retrieval-response"]][["coredata"]][["citedby-count"]]),
    error = function(e) NA_integer_
  )
}

# ── Función: obtener citas por título (fallback sin DOI) ─────────────────────
get_citas_titulo <- function(titulo, api_key, pausa = 0.7) {
  if (is.na(titulo) || nchar(trimws(titulo)) < 15) return(NA_integer_)

  titulo_q <- substr(gsub('["]', "", titulo), 1, 120)

  resp <- tryCatch(
    httr::GET("https://api.elsevier.com/content/search/scopus",
              query = list(query = paste0('TITLE("', titulo_q, '")'),
                           field = "citedby-count", count = "1"),
              httr::add_headers("X-ELS-APIKey" = api_key,
                                "Accept"       = "application/json"),
              httr::timeout(12)),
    error = function(e) NULL
  )
  Sys.sleep(pausa)

  if (is.null(resp) || httr::status_code(resp) != 200) return(NA_integer_)

  contenido <- tryCatch(
    jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"),
                       simplifyVector = TRUE),
    error = function(e) NULL
  )
  if (is.null(contenido)) return(NA_integer_)

  tryCatch(
    as.integer(contenido[["search-results"]][["entry"]][[1]][["citedby-count"]]),
    error = function(e) NA_integer_
  )
}

# ── Función principal: enriquecer CUALQUIER data frame con citas vía API ─────
enriquecer_citas_API <- function(df, api_key,
                                  col_doi    = "DI",
                                  col_titulo = "TI",
                                  backup_cada = 50) {

  n <- nrow(df)
  df$TC_API         <- NA_integer_
  df$TC_API_fuente  <- NA_character_
  df$TC_API_fecha   <- format(Sys.Date(), "%Y-%m-%d")

  cat("=== ENRIQUECIMIENTO DE CITAS VÍA API SCOPUS ===\n")
  cat("  Total registros :", n, "\n")
  cat("  Con DOI         :", sum(!is.na(df[[col_doi]]) & df[[col_doi]] != ""), "\n")
  cat("  Sin DOI (título):", sum( is.na(df[[col_doi]]) | df[[col_doi]] == ""), "\n")
  cat("  Tiempo estimado : ~",
      round((sum(!is.na(df[[col_doi]])) * 0.5 +
               sum(is.na(df[[col_doi]])) * 0.9) / 60, 1), "minutos\n\n")

  for (i in seq_len(n)) {

    doi_i <- df[[col_doi]][i]

    if (!is.na(doi_i) && trimws(doi_i) != "") {
      df$TC_API[i]        <- get_citas_doi(doi_i, api_key)
      df$TC_API_fuente[i] <- "DOI"
    } else {
      df$TC_API[i]        <- get_citas_titulo(df[[col_titulo]][i], api_key)
      df$TC_API_fuente[i] <- "Titulo"
    }

    # Progreso cada 10
    if (i %% 10 == 0 || i == n) {
      cat(sprintf("  [%d/%d] | OK: %d | NA: %d\n",
                  i, n,
                  sum(!is.na(df$TC_API[1:i])),
                  sum( is.na(df$TC_API[1:i]))))
    }

    # Backup periódico
    if (i %% backup_cada == 0) {
      saveRDS(df, "data/backup_citas_api.rds")
    }
  }

  # Resumen
  cat("\n--- RESULTADO ---\n")
  cat("  Citas recuperadas   :", sum(!is.na(df$TC_API)), "\n")
  cat("  Por DOI             :", sum(df$TC_API_fuente == "DOI",   na.rm = TRUE), "\n")
  cat("  Por título          :", sum(df$TC_API_fuente == "Titulo", na.rm = TRUE), "\n")
  cat("  Sin datos (NA)      :", sum( is.na(df$TC_API)), "\n")
  cat("  Cobertura           :",
      round(mean(!is.na(df$TC_API)) * 100, 1), "%\n\n")

  return(df)
}



# ─────────────────────────────────────────────────────────────────────────────
# BLOQUE C — PIPELINE COMPLETO (USAR ESTE EN TU PROYECTO)
# ─────────────────────────────────────────────────────────────────────────────

library(bibliometrix)
library(dplyr)
library(httr)
library(jsonlite)
library(openxlsx)

# ── PASO 1: Importar Embase con el parser definitivo ──────────────────────────
# Usa el RIS → es el mejor de tus tres formatos
embase <- importar_embase_ris_definitivo("data/embase.ris")

# Verificación rápida
cat("Muestra de 3 registros:\n")
print(embase[1:3, c("TI", "AU", "PY", "SO", "DI", "DT")])


# ── PASO 2: Importar las otras 4 bases (sin cambios) ─────────────────────────
scopus <- convert2df("data/scopus.bib",  dbsource = "scopus", format = "bibtex")
wos    <- convert2df("data/wos.bib",     dbsource = "wos",    format = "bibtex")
scielo <- convert2df("data/scielo.txt",  dbsource = "wos",    format = "plaintext")
pubmed <- convert2df("data/pubmed.txt",  dbsource = "pubmed", format = "pubmed")

scopus$SOURCE_DB <- "Scopus"
wos$SOURCE_DB    <- "WoS"
scielo$SOURCE_DB <- "SciELO"
pubmed$SOURCE_DB <- "PubMed"
# embase$SOURCE_DB ya está definido por el parser


# ── PASO 3: Merge de las 5 bases ─────────────────────────────────────────────
# Embase a veces falla en mergeDbSources por columnas faltantes → usamos bind_rows + dedup manual
library(bibliometrix)
library(tidyverse)
merge_5_bases <- function(scopus, wos, scielo, pubmed, embase) {

  # Primero las 4 bases estándar
  base_4 <- mergeDbSources(scopus, wos, scielo, pubmed,
                            remove.duplicated = TRUE)
  cat("Registros tras merge de 4 bases  :", nrow(base_4), "\n")

  # Asegurar columnas mínimas en embase antes de unir
  cols_base4 <- names(base_4)
  for (col in cols_base4) {
    if (!(col %in% names(embase))) embase[[col]] <- NA
  }
  for (col in names(embase)) {
    if (!(col %in% names(base_4))) base_4[[col]] <- NA
  }

  # Normalizar TI para deduplicación
  base_4$TI_norm  <- toupper(trimws(base_4$TI))
  embase$TI_norm  <- toupper(trimws(embase$TI))

  # DOIs ya presentes en las 4 bases
  dois_existentes  <- base_4$DI[!is.na(base_4$DI) & base_4$DI != ""]
  titles_existentes <- base_4$TI_norm[!is.na(base_4$TI_norm)]

  # Filtrar Embase: solo los que NO están (por DOI primero, luego por título)
  embase_new <- embase %>%
    filter(
      # No tiene DOI que ya existe en las 4 bases
      (is.na(DI) | DI == "" | !(DI %in% dois_existentes)) &
      # No tiene título que ya existe
      !(TI_norm %in% titles_existentes)
    )

  cat("Registros Embase únicos (no en otras bases):", nrow(embase_new), "\n")

  # Unir
  base_final <- bind_rows(base_4, embase_new) %>%
    select(-TI_norm)

  cat("Total tras merge de 5 bases:", nrow(base_final), "\n")
  return(base_final)
}

base_fusionada <- merge_5_bases(scopus, wos, scielo, pubmed, embase)

# ── PASO 4: Trazabilidad ─────────────────────────────────────────────────────
inventario <- bind_rows(
  scopus %>% select(TI, DI, SOURCE_DB),
  wos    %>% select(TI, DI, SOURCE_DB),
  scielo %>% select(TI, DI, SOURCE_DB),
  pubmed %>% select(TI, DI, SOURCE_DB),
  embase %>% select(TI, DI, SOURCE_DB)
) %>% mutate(TI = toupper(trimws(TI)))

trazabilidad <- inventario %>%
  group_by(TI) %>%
  summarise(
    Bases_indexadoras = paste(unique(SOURCE_DB), collapse = " | "),
    N_bases           = n_distinct(SOURCE_DB),
    .groups = "drop"
  )

base_fusionada <- base_fusionada %>%
  mutate(TI_norm = toupper(trimws(TI))) %>%
  left_join(trazabilidad %>% rename(TI_norm = TI), by = "TI_norm") %>%
  select(-TI_norm)


# ── PASO 5: Enriquecimiento con citas vía API Scopus ─────────────────────────
# Primero prueba que la API responde:
citas_test <- get_citas_doi("10.1016/j.joi.2017.08.007", SCOPUS_API_KEY)
cat("Prueba API Scopus (esperado >1000):", citas_test, "\n")

if (!is.na(citas_test)) {
  cat("✓ API disponible. Iniciando enriquecimiento...\n")
  base_fusionada <- enriquecer_citas_API(base_fusionada, SCOPUS_API_KEY)
} else {
  cat("✗ API no disponible (comprueba IP institucional / VPN universitaria).\n")
  base_fusionada$TC_API        <- as.integer(base_fusionada$TC)
  base_fusionada$TC_API_fuente <- "Exportacion_original"
  base_fusionada$TC_API_fecha  <- format(Sys.Date(), "%Y-%m-%d")
}


# ── PASO 6: Exportar ─────────────────────────────────────────────────────────
if (!dir.exists("resultados")) dir.create("resultados")

# Excel con múltiples hojas
wb <- createWorkbook()

addWorksheet(wb, "Base_completa")
writeData(wb, "Base_completa", base_fusionada %>%
            select(TI, AU, PY, SO, DI, DT, LA, TC, TC_API,
                   TC_API_fuente, TC_API_fecha, C1, DE, ID, AB,
                   SOURCE_DB, Bases_indexadoras, N_bases, UT, SN, VL))

addWorksheet(wb, "Embase_importada")
writeData(wb, "Embase_importada", embase)

addWorksheet(wb, "Trazabilidad")
writeData(wb, "Trazabilidad", trazabilidad)

addWorksheet(wb, "Resumen_cobertura")
writeData(wb, "Resumen_cobertura",
          inventario %>%
            count(SOURCE_DB, name = "N_bruto") %>%
            mutate(N_tras_dedup = nrow(base_fusionada),
                   Fecha        = format(Sys.Date(), "%Y-%m-%d")))

saveWorkbook(wb, "resultados/base_final_5bases.xlsx", overwrite = TRUE)
cat("✓ Exportado: resultados/base_final_5bases.xlsx\n")

saveRDS(base_fusionada, "resultados/base_fusionada.rds")
cat("✓ Guardado RDS: resultados/base_fusionada.rds\n")

cat("\n=== PROCESO COMPLETADO ===\n")
cat("Total registros únicos:", nrow(base_fusionada), "\n")
cat("Cobertura citas API   :",
    round(mean(!is.na(base_fusionada$TC_API)) * 100, 1), "%\n")

