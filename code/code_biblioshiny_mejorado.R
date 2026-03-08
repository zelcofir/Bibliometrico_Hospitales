# =============================================================================
# ANÁLISIS BIBLIOMÉTRICO — HOSPITALES NIVEL III DEL PERÚ
# Autor : Frank Zela-Coila
# Versión: 2.0 — Febrero 2026
# Descripción: Importación, fusión, trazabilidad, análisis bibliométrico y
#              enriquecimiento automático de citas vía API de Scopus.
# =============================================================================
# ÍNDICE:
#  BLOQUE 0 — Instalación y carga de paquetes
#  BLOQUE 1 — Importación de bases de datos
#  BLOQUE 2 — Etiquetado por origen (trazabilidad)
#  BLOQUE 3 — Fusión y deduplicación
#  BLOQUE 4 — Enriquecimiento automático vía API de Scopus
#  BLOQUE 5 — Preparación para Biblioshiny
#  BLOQUE 6 — Análisis bibliométrico descriptivo
#  BLOQUE 7 — Exportaciones finales
# =============================================================================



# =============================================================================
# BLOQUE 0 — INSTALACIÓN Y CARGA DE PAQUETES
# =============================================================================

# Instalar paquetes si aún no están presentes
paquetes_necesarios <- c(
  "bibliometrix",  # Análisis bibliométrico principal
  "revtools",      # Herramientas complementarias de revisión
  "openxlsx",      # Exportar a Excel (.xlsx)
  "dplyr",         # Manipulación de datos (tidyverse)
  "stringr",       # Manejo de cadenas de texto
  "httr",          # Peticiones HTTP (necesario para API Scopus)
  "jsonlite",      # Parseo de respuestas JSON de la API
  "rscopus",       # Cliente R oficial para la API de Scopus
  "tibble",        # Tablas mejoradas
  "purrr"          # Iteración funcional (para el loop de la API)
)

paquetes_faltantes <- paquetes_necesarios[
  !paquetes_necesarios %in% rownames(installed.packages())
]

if (length(paquetes_faltantes) > 0) {
  message("Instalando paquetes faltantes: ", paste(paquetes_faltantes, collapse = ", "))
  install.packages(paquetes_faltantes, dependencies = TRUE)
}

# Cargar todos los paquetes
invisible(lapply(paquetes_necesarios, library, character.only = TRUE))

# Mostrar versiones para reproducibilidad
cat("=== VERSIONES DE PAQUETES CLAVE ===\n")
cat("bibliometrix :", as.character(packageVersion("bibliometrix")), "\n")
cat("rscopus      :", as.character(packageVersion("rscopus")), "\n")
cat("httr         :", as.character(packageVersion("httr")), "\n")
cat("R version    :", R.version$version.string, "\n")
cat("Fecha análisis:", format(Sys.Date(), "%d/%m/%Y"), "\n\n")


# =============================================================================
# BLOQUE 1 — IMPORTACIÓN DE BASES DE DATOS
# =============================================================================
# NOTA: Asegúrate de que la carpeta "data/" exista en tu directorio de trabajo.
# Usa setwd("ruta/a/tu/proyecto") si es necesario.
# Formatos aceptados por convert2df:
#   Scopus → .bib (BibTeX) o .csv
#   WoS    → .bib (BibTeX) o .txt (plaintext)
#   SciELO → .txt (plaintext, exportado vía interfaz WoS)
#   PubMed → .txt (formato PubMed/MEDLINE)
#   Embase → .ris o .csv (OvidSP o interfaz Embase)

cat("--- Importando Scopus (.bib) ---\n")
scopus <- convert2df(
  file       = "data/scopus.bib",
  dbsource   = "scopus",
  format     = "bibtex"
)
cat("  Registros Scopus:", nrow(scopus), "\n")


cat("--- Importando Web of Science (.bib) ---\n")
wos <- convert2df(
  file       = "data/wos.bib",
  dbsource   = "wos",
  format     = "bibtex"
)
cat("  Registros WoS:", nrow(wos), "\n")


cat("--- Importando SciELO via WoS (.txt plaintext) ---\n")
scielo <- convert2df(
  file       = "data/scielo.txt",
  dbsource   = "wos",      # SciELO exportado por la interfaz WoS usa motor WoS
  format     = "plaintext"
)
cat("  Registros SciELO:", nrow(scielo), "\n")


cat("--- Importando PubMed/Medline (.txt) ---\n")
pubmed <- convert2df(
  file       = "data/pubmed.txt",
  dbsource   = "pubmed",
  format     = "pubmed"
)
cat("  Registros PubMed:", nrow(pubmed), "\n")


# ── Embase: acepta .csv o .ris según cómo exportaste ──────────────────────────
# OPCIÓN A — Si exportaste desde Embase como archivo CSV (más común):
cat("--- Importando Embase (.csv) ---\n")
embase <- convert2df(
  file       = "data/embase.csv",
  dbsource   = "scopus",
  format     = "csv"        # Cambia a "plaintext" si usaste exportación .ris/.txt
)
cat("  Registros Embase:", nrow(embase), "\n\n")

# OPCIÓN B — Si exportaste como RIS (descomenta y usa en lugar de OPCIÓN A):
embase <- convert2df("data/embase.ris", dbsource = "embase", format = "ris")


# =============================================================================
# BLOQUE 2 — ETIQUETADO POR ORIGEN (TRAZABILIDAD)
# =============================================================================
# Añadimos una columna SOURCE_DB para saber de qué base proviene cada registro
# ANTES de la fusión, de modo que quede registrado en la base final.

scopus$SOURCE_DB <- "Scopus"
wos$SOURCE_DB    <- "WoS"
scielo$SOURCE_DB <- "SciELO"
pubmed$SOURCE_DB <- "PubMed"
embase$SOURCE_DB <- "Embase"


# ── Inventario de cobertura cruzada (¿cuántas bases indexan cada artículo?) ──
# Usamos el campo TI (título normalizado) como llave de cruce.
# Para mayor precisión, luego cruzaremos también por DOI.

inventario_raw <- bind_rows(
  scopus %>% select(TI, DI, SOURCE_DB),
  wos    %>% select(TI, DI, SOURCE_DB),
  scielo %>% select(TI, DI, SOURCE_DB),
  pubmed %>% select(TI, DI, SOURCE_DB),
  embase %>% select(TI, DI, SOURCE_DB)
)

# Trazabilidad por título (TI)
trazabilidad_ti <- inventario_raw %>%
  group_by(TI) %>%
  summarise(
    Bases_que_indexan   = paste(unique(SOURCE_DB), collapse = " | "),
    N_bases             = n_distinct(SOURCE_DB),
    .groups = "drop"
  ) %>%
  rename(Titulo_normalizado = TI)

# Trazabilidad por DOI (DI) — más exacta cuando el DOI está disponible
trazabilidad_doi <- inventario_raw %>%
  filter(!is.na(DI) & DI != "") %>%
  group_by(DI) %>%
  summarise(
    Bases_que_indexan_doi = paste(unique(SOURCE_DB), collapse = " | "),
    N_bases_doi           = n_distinct(SOURCE_DB),
    .groups = "drop"
  )

cat("=== RESUMEN DE COBERTURA POR BASE ===\n")
print(table(inventario_raw$SOURCE_DB))
cat("Total registros brutos (con duplicados):", nrow(inventario_raw), "\n")
cat("Artículos únicos por título            :", nrow(trazabilidad_ti), "\n\n")


# =============================================================================
# BLOQUE 3 — FUSIÓN Y DEDUPLICACIÓN
# =============================================================================

cat("--- Fusionando y deduplicando las 5 bases ---\n")

# mergeDbSources elimina duplicados comparando TI (título normalizado).
# Devuelve la versión más completa de cada registro (la de Scopus tiene
# prioridad en la mayoría de los campos si va primera en la lista).
base_fusionada <- mergeDbSources(
  scopus,
  wos,
  scielo,
  pubmed,
  embase,
  remove.duplicated = TRUE
)
cat("  Registros tras fusión y deduplicación:", nrow(base_fusionada), "\n")
cat("  Registros eliminados (duplicados)     :",
    nrow(inventario_raw) - nrow(base_fusionada), "\n\n")


# ── Incorporar trazabilidad a la base fusionada ───────────────────────────────
# Cruce primario por DOI; cruce secundario por título para los sin DOI.

base_fusionada <- base_fusionada %>%
  # Cruce por DOI
  left_join(trazabilidad_doi, by = c("DI")) %>%
  # Cruce por título (para registros sin DOI que no encontraron match anterior)
  left_join(
    trazabilidad_ti %>% rename(TI = Titulo_normalizado),
    by = "TI"
  ) %>%
  # Unificar en una sola columna de bases
  mutate(
    Bases_indexadoras = case_when(
      !is.na(Bases_que_indexan_doi) ~ Bases_que_indexan_doi,
      !is.na(Bases_que_indexan)     ~ Bases_que_indexan,
      TRUE                          ~ SOURCE_DB  # fallback: base propia
    ),
    N_bases_indexadoras = case_when(
      !is.na(N_bases_doi) ~ N_bases_doi,
      !is.na(N_bases)     ~ N_bases,
      TRUE                ~ 1L
    )
  ) %>%
  select(-Bases_que_indexan_doi, -N_bases_doi,
         -Bases_que_indexan,     -N_bases)


# =============================================================================
# BLOQUE 4 — ENRIQUECIMIENTO AUTOMÁTICO VÍA API DE SCOPUS
# =============================================================================
# La API de Scopus permite recuperar el número de citas ACTUAL (al momento
# de ejecutar el script) para cada artículo usando su DOI o EID.
# Esto enriquece la columna TC (Total Citations) con datos actualizados.
#
# REQUISITO: Tener acceso institucional activo en Scopus.
#            La API key de acceso individual funciona para consultas abstractas,
#            pero el conteo de citas requiere acceso desde una IP institucional
#            o uso de tokens de acceso completo.
#            Si estás en casa, usa una VPN de tu universidad.
#
# DOCUMENTACIÓN OFICIAL: https://dev.elsevier.com/documentation/ScopusSearchAPI.wadl
# =============================================================================

# ── 4.0  Configurar la API key ────────────────────────────────────────────────
# ¡IMPORTANTE! Nunca compartas tu API key públicamente.
# Recomendamos guardarla en .Renviron para no escribirla en el código:
#   usethis::edit_r_environ()
#   SCOPUS_API_KEY=3925b30e9cbd00208d4f3b3a5dcbf48f
# Y luego leerla así: Sys.getenv("SCOPUS_API_KEY")

SCOPUS_API_KEY <- Sys.getenv("SCOPUS_API_KEY")

# Si aún no configuraste .Renviron, puedes definirla directamente
# (solo para uso local; NO subas este archivo a GitHub/repositorios):
if (SCOPUS_API_KEY == "") {
  SCOPUS_API_KEY <- "3925b30e9cbd00208d4f3b3a5dcbf48f"
  message("AVISO: API key definida directamente en el script. ",
          "Considera moverla a .Renviron para mayor seguridad.")
}

# Registrar la key en rscopus
rscopus::set_api_key(SCOPUS_API_KEY)


# ── 4.1  Función para obtener citas por DOI desde la API ──────────────────────
#
# La API de Scopus Abstract Retrieval nos devuelve, entre otros campos:
#   - citedby-count : número de citas en Scopus (actualizado en tiempo real)
#   - eid            : identificador EID de Scopus
#   - prism:doi      : DOI confirmado
#
# Endpoint: https://api.elsevier.com/content/abstract/doi/{DOI}

obtener_citas_scopus <- function(doi, api_key, pausa_seg = 0.5) {
  
  # Validar input
  if (is.na(doi) || doi == "") return(NA_integer_)
  
  # Construir URL
  url <- paste0(
    "https://api.elsevier.com/content/abstract/doi/",
    utils::URLencode(doi, repeated = TRUE)
  )
  
  # Petición GET con cabeceras requeridas por Elsevier
  respuesta <- tryCatch({
    httr::GET(
      url,
      httr::add_headers(
        "X-ELS-APIKey"  = api_key,
        "Accept"        = "application/json"
      ),
      httr::timeout(15)
    )
  }, error = function(e) {
    message("  Error de conexión para DOI: ", doi, " — ", conditionMessage(e))
    return(NULL)
  })
  
  # Pausa para respetar el rate limit de la API (máx. 9 req/seg en plan básico)
  Sys.sleep(pausa_seg)
  
  # Verificar respuesta
  if (is.null(respuesta)) return(NA_integer_)
  
  # HTTP 200 = éxito
  if (httr::status_code(respuesta) != 200) {
    # Código 429 = demasiadas peticiones → aumenta pausa_seg
    if (httr::status_code(respuesta) == 429) {
      message("  Rate limit alcanzado. Esperando 10 segundos...")
      Sys.sleep(10)
    }
    return(NA_integer_)
  }
  
  # Parsear JSON
  contenido <- tryCatch({
    jsonlite::fromJSON(
      httr::content(respuesta, as = "text", encoding = "UTF-8"),
      simplifyVector = TRUE
    )
  }, error = function(e) NULL)
  
  if (is.null(contenido)) return(NA_integer_)
  
  # Extraer el campo citedby-count
  citas <- tryCatch({
    as.integer(
      contenido[["abstracts-retrieval-response"]][["coredata"]][["citedby-count"]]
    )
  }, error = function(e) NA_integer_)
  
  return(citas)
}


# ── 4.2  Función alternativa: búsqueda por título si no hay DOI ───────────────
# Usa el endpoint de búsqueda de Scopus y extrae citas del primer resultado.

obtener_citas_por_titulo <- function(titulo, api_key, pausa_seg = 0.8) {
  
  if (is.na(titulo) || nchar(trimws(titulo)) < 10) return(NA_integer_)
  
  # Limpiar el título para la búsqueda
  titulo_limpio <- gsub('["]', "", titulo)
  titulo_limpio <- substr(titulo_limpio, 1, 150)  # API limita la query
  
  url <- "https://api.elsevier.com/content/search/scopus"
  
  respuesta <- tryCatch({
    httr::GET(
      url,
      query = list(
        query  = paste0('TITLE("', titulo_limpio, '")'),
        field  = "citedby-count,doi,title",
        count  = "1"
      ),
      httr::add_headers(
        "X-ELS-APIKey" = api_key,
        "Accept"       = "application/json"
      ),
      httr::timeout(15)
    )
  }, error = function(e) NULL)
  
  Sys.sleep(pausa_seg)
  
  if (is.null(respuesta) || httr::status_code(respuesta) != 200) return(NA_integer_)
  
  contenido <- tryCatch({
    jsonlite::fromJSON(
      httr::content(respuesta, as = "text", encoding = "UTF-8"),
      simplifyVector = TRUE
    )
  }, error = function(e) NULL)
  
  if (is.null(contenido)) return(NA_integer_)
  
  citas <- tryCatch({
    entradas <- contenido[["search-results"]][["entry"]]
    as.integer(entradas[[1]][["citedby-count"]])
  }, error = function(e) NA_integer_)
  
  return(citas)
}


# ── 4.3  Prueba de conexión con la API ────────────────────────────────────────
# Antes de lanzar el loop completo, verifica que la API responde.

cat("=== PROBANDO CONEXIÓN CON LA API DE SCOPUS ===\n")

doi_prueba <- "10.1016/j.joi.2017.08.007"  # Artículo de bibliometrix (Aria & Cuccurullo, 2017)
citas_prueba <- obtener_citas_scopus(doi_prueba, SCOPUS_API_KEY)

if (!is.na(citas_prueba)) {
  cat("✓ Conexión exitosa. El artículo de prueba tiene", citas_prueba, "citas en Scopus.\n\n")
  api_disponible <- TRUE
} else {
  cat("✗ No se pudo conectar a la API de Scopus.\n")
  cat("  Posibles causas:\n")
  cat("  1. No estás en una IP institucional reconocida por Scopus.\n")
  cat("  2. La API key no tiene permisos para Abstract Retrieval.\n")
  cat("  3. Sin conexión a internet o API en mantenimiento.\n")
  cat("  → Las citas se dejarán como NA. Puedes reintentarlo desde la red de tu universidad.\n\n")
  api_disponible <- FALSE
}


# ── 4.4  Loop principal de enriquecimiento de citas ───────────────────────────
# Solo se ejecuta si la API está disponible.
# El proceso puede tomar varios minutos dependiendo del número de artículos.
# ESTRATEGIA:
#   1. Si hay DOI → usar obtener_citas_scopus() [más exacto]
#   2. Si NO hay DOI → usar obtener_citas_por_titulo() [alternativa]
#   3. Guardar resultados intermedios cada 50 registros (protección ante cortes)

if (api_disponible) {
  
  n_total <- nrow(base_fusionada)
  cat(paste0("=== ENRIQUECIENDO CITAS VÍA API (",
             n_total, " artículos) ===\n"))
  cat("  Este proceso puede tomar entre", round(n_total * 0.6 / 60, 1),
      "y", round(n_total * 1.2 / 60, 1), "minutos.\n\n")
  
  # Crear columna para citas actualizadas (TC_Scopus_API)
  base_fusionada$TC_Scopus_API    <- NA_integer_
  base_fusionada$Fuente_citas_API <- NA_character_
  
  # Loop con barra de progreso
  for (i in seq_len(n_total)) {
    
    doi_i    <- base_fusionada$DI[i]
    titulo_i <- base_fusionada$TI[i]
    
    if (!is.na(doi_i) && doi_i != "") {
      # Ruta 1: DOI disponible
      citas_i <- obtener_citas_scopus(doi_i, SCOPUS_API_KEY, pausa_seg = 0.4)
      base_fusionada$Fuente_citas_API[i] <- "DOI"
    } else {
      # Ruta 2: Sólo título
      citas_i <- obtener_citas_por_titulo(titulo_i, SCOPUS_API_KEY, pausa_seg = 0.7)
      base_fusionada$Fuente_citas_API[i] <- "Titulo"
    }
    
    base_fusionada$TC_Scopus_API[i] <- citas_i
    
    # Progreso en consola
    if (i %% 10 == 0 || i == n_total) {
      cat(sprintf("  [%d/%d] Procesados | Citas recuperadas: %d | NAs: %d\n",
                  i, n_total,
                  sum(!is.na(base_fusionada$TC_Scopus_API[1:i])),
                  sum( is.na(base_fusionada$TC_Scopus_API[1:i]))))
    }
    
    # Guardado intermedio de seguridad cada 50 registros
    if (i %% 50 == 0) {
      saveRDS(base_fusionada, file = "data/backup_enriquecimiento.rds")
      cat("  >>> Backup guardado en data/backup_enriquecimiento.rds\n")
    }
  }
  
  # Resumen de enriquecimiento
  cat("\n=== RESUMEN ENRIQUECIMIENTO API ===\n")
  cat("  Citas recuperadas exitosamente :", sum(!is.na(base_fusionada$TC_Scopus_API)), "\n")
  cat("  Recuperadas por DOI            :",
      sum(base_fusionada$Fuente_citas_API == "DOI",   na.rm = TRUE), "\n")
  cat("  Recuperadas por título         :",
      sum(base_fusionada$Fuente_citas_API == "Titulo", na.rm = TRUE), "\n")
  cat("  Sin datos (NA)                 :", sum(is.na(base_fusionada$TC_Scopus_API)), "\n")
  cat("  Cobertura                      :",
      round(mean(!is.na(base_fusionada$TC_Scopus_API)) * 100, 1), "%\n\n")
  
  # ── Comparación: citas originales (TC) vs citas actualizadas (TC_Scopus_API) ──
  # TC = citas al momento de la descarga de la base de datos
  # TC_Scopus_API = citas actualizadas al día de hoy
  
  comparacion_citas <- base_fusionada %>%
    select(TI, DI, TC, TC_Scopus_API, Fuente_citas_API) %>%
    mutate(
      TC_original       = as.integer(TC),
      Diferencia_citas  = TC_Scopus_API - TC_original,
      Citas_actualizadas = TC_Scopus_API  # columna principal para usar en análisis
    ) %>%
    arrange(desc(TC_Scopus_API))
  
  cat("=== TOP 10 ARTÍCULOS POR CITAS ACTUALIZADAS ===\n")
  print(
    comparacion_citas %>%
      slice_head(n = 10) %>%
      select(TI, TC_original, Citas_actualizadas, Diferencia_citas) %>%
      mutate(TI = str_trunc(TI, 60))
  )
  
} else {
  # Si la API no está disponible, usamos TC de la descarga original
  base_fusionada$TC_Scopus_API    <- as.integer(base_fusionada$TC)
  base_fusionada$Fuente_citas_API <- "Exportación_original"
  message("API no disponible. Se usará TC original de las bases de datos.")
}

# Fecha de actualización de citas
base_fusionada$Fecha_consulta_API <- format(Sys.Date(), "%Y-%m-%d")


# =============================================================================
# BLOQUE 5 — PREPARACIÓN PARA BIBLIOSHINY
# =============================================================================
# Biblioshiny espera columnas en un orden específico.
# Creamos una copia reordenada y compatibilizada.

columnas_shiny <- c(
  "AU", "DE", "ID", "C1", "CR", "AB", "affiliations",
  "AR", "EM", "BO", "da", "DI", "GA", "eissn",
  "earlyaccessdate", "BE", "FU", "FX", "BN", "SN",
  "JI", "SO", "LA", "meeting", "month", "note",
  "NR", "PN", "oa", "orcid.numbers", "PP", "PU", "SC",
  "researcherid.numbers", "SE", "TC", "TI", "DT", "UT",
  "usage.count.last.180.days", "U2", "VL",
  "web.of.science.categories.", "web.of.science.index",
  "PY", "AF", "RP", "DB", "J9", "AB_raw", "TI_raw",
  "DE_raw", "AU_UN", "AU1_UN", "AU_UN_NR", "SR_FULL", "SR"
)

# Añadimos columnas propias al final (fuera del esquema Shiny, para Excel)
columnas_extra <- c(
  "SOURCE_DB", "Bases_indexadoras", "N_bases_indexadoras",
  "TC_Scopus_API", "Fuente_citas_API", "Fecha_consulta_API"
)

# Crear columnas faltantes del esquema Shiny como NA
for (col in columnas_shiny) {
  if (!(col %in% names(base_fusionada))) {
    base_fusionada[[col]] <- NA
  }
}

# Versión para Biblioshiny (solo columnas del esquema)
base_para_shiny <- base_fusionada[, columnas_shiny]

cat("Base para Biblioshiny preparada:", nrow(base_para_shiny), "registros,",
    ncol(base_para_shiny), "columnas.\n")


# =============================================================================
# BLOQUE 6 — ANÁLISIS BIBLIOMÉTRICO DESCRIPTIVO (resumen rápido)
# =============================================================================

cat("\n=== ANÁLISIS BIBLIOMÉTRICO DESCRIPTIVO ===\n")

# El análisis de bibliometrix trabaja sobre la base fusionada completa
resultados_bib <- biblioAnalysis(base_fusionada, sep = ";")

# Resumen en consola (los 15 principales en cada categoría)
summary_bib <- summary(resultados_bib, k = 15, pause = FALSE)

# Gráficos básicos de producción, autores e impacto
# (Se generan en el Viewer de RStudio)
plot(resultados_bib, k = 15, pause = FALSE)


# ── 6.1  Estadísticas de citas actualizadas ───────────────────────────────────
if ("TC_Scopus_API" %in% names(base_fusionada)) {
  
  citas_vec <- base_fusionada$TC_Scopus_API
  cat("\n--- Estadísticas de citas (API Scopus actualizada) ---\n")
  cat("  Mediana de citas    :", median(citas_vec, na.rm = TRUE), "\n")
  cat("  Media de citas      :", round(mean(citas_vec, na.rm = TRUE), 2), "\n")
  cat("  Máximo de citas     :", max(citas_vec, na.rm = TRUE), "\n")
  cat("  Artículos sin citas :", sum(citas_vec == 0, na.rm = TRUE), "\n")
  cat("  % artículos citados :",
      round(mean(citas_vec > 0, na.rm = TRUE) * 100, 1), "%\n")
  
  # Artículos altamente citados (≥10 citas)
  altamente_citados <- base_fusionada %>%
    filter(TC_Scopus_API >= 10) %>%
    arrange(desc(TC_Scopus_API)) %>%
    select(TI, DI, PY, SO, TC_Scopus_API, SOURCE_DB)
  
  cat("  Artículos ≥10 citas :", nrow(altamente_citados), "\n")
}


# =============================================================================
# BLOQUE 7 — EXPORTACIONES FINALES
# =============================================================================

cat("\n=== EXPORTANDO ARCHIVOS FINALES ===\n")

# ── 7.1  Base completa con trazabilidad y citas actualizadas ─────────────────
columnas_excel_completo <- c(
  # Identificadores básicos
  "TI", "AU", "PY", "SO", "DI", "DT", "LA",
  # Indicadores de impacto
  "TC", "TC_Scopus_API", "Fecha_consulta_API", "Fuente_citas_API",
  # Colaboración
  "C1", "AU_UN", "AU1_UN",
  # Trazabilidad
  "SOURCE_DB", "Bases_indexadoras", "N_bases_indexadoras",
  # Otros campos útiles
  "AB", "DE", "ID", "J9", "SN", "UT", "DB"
)

# Incluir solo columnas que existen
columnas_excel_completo <- intersect(columnas_excel_completo, names(base_fusionada))

wb <- createWorkbook()

# Hoja 1: Base completa
addWorksheet(wb, "Base_completa")
writeData(wb, "Base_completa",
          base_fusionada[, columnas_excel_completo])

# Hoja 2: Solo artículos con citas API disponibles
addWorksheet(wb, "Citas_actualizadas_API")
writeData(wb, "Citas_actualizadas_API",
          base_fusionada %>%
            filter(!is.na(TC_Scopus_API)) %>%
            select(all_of(columnas_excel_completo)) %>%
            arrange(desc(TC_Scopus_API)))

# Hoja 3: Artículos altamente citados (≥10)
if (exists("altamente_citados")) {
  addWorksheet(wb, "Altamente_citados_10+")
  writeData(wb, "Altamente_citados_10+", altamente_citados)
}

# Hoja 4: Trazabilidad por título
addWorksheet(wb, "Trazabilidad")
writeData(wb, "Trazabilidad", trazabilidad_ti)

# Hoja 5: Cobertura por base
cobertura_resumen <- inventario_raw %>%
  count(SOURCE_DB, name = "N_registros_brutos") %>%
  mutate(
    N_tras_dedup = nrow(base_fusionada),
    Fecha        = format(Sys.Date(), "%Y-%m-%d")
  )
addWorksheet(wb, "Cobertura_por_base")
writeData(wb, "Cobertura_por_base", cobertura_resumen)

# Guardar el workbook
saveWorkbook(wb, file = "resultados/base_final_con_citas_API.xlsx", overwrite = TRUE)
cat("  ✓ Guardado: resultados/base_final_con_citas_API.xlsx\n")


# ── 7.2  Base para Biblioshiny (esquema compatible) ───────────────────────────
write.xlsx(base_para_shiny, file = "resultados/base_para_shiny.xlsx")
cat("  ✓ Guardado: resultados/base_para_shiny.xlsx\n")


# ── 7.3  Backup RDS (formato nativo R, más rápido de recargar) ────────────────
saveRDS(base_fusionada, file = "resultados/base_fusionada_completa.rds")
cat("  ✓ Guardado: resultados/base_fusionada_completa.rds\n")
cat("  → Para recargar: base_fusionada <- readRDS('resultados/base_fusionada_completa.rds')\n\n")

cat("=== PROCESO COMPLETADO ===\n")
cat("Fecha:", format(Sys.Now(), "%d/%m/%Y %H:%M:%S"), "\n")


# =============================================================================
# LANZAR BIBLIOSHINY (opcional — comentar si solo quieres el procesamiento)
# =============================================================================
# biblioshiny()


# =============================================================================
# NOTAS SOBRE LA API DE SCOPUS
# =============================================================================
#
# LIMITACIONES DE LA API KEY BÁSICA (Individual API Key):
# ─────────────────────────────────────────────────────────
# ✓ Abstract Retrieval API  → SÍ permite recuperar citedby-count por DOI
# ✓ Scopus Search API       → SÍ permite buscar por título
# ✗ Sin IP institucional    → Algunas instituciones bloquean acceso externo
# ✗ Rate limit              → 9 peticiones/segundo | 20,000 peticiones/semana
#
# QUÉ CAMPOS ADICIONALES PUEDES RECUPERAR CON LA API:
# ─────────────────────────────────────────────────────────
#   citedby-count       = Citas en Scopus (lo que ya recuperamos)
#   prism:doi           = DOI verificado por Scopus
#   dc:creator          = Primer autor
#   prism:publicationName = Nombre de la revista
#   prism:coverDate     = Fecha de publicación
#   authkeywords        = Palabras clave de autor
#   subject-area        = Áreas temáticas Scopus
#   openaccess          = ¿Es Open Access?
#   eid                 = EID de Scopus (identificador único)
#
# CÓMO AMPLIAR LA FUNCIÓN PARA RECUPERAR MÁS CAMPOS:
# ─────────────────────────────────────────────────────────
# Añade los campos que necesitas al parseo del JSON en obtener_citas_scopus()
# Por ejemplo, para recuperar también el EID y si es Open Access:
#
#   eid <- contenido[["abstracts-retrieval-response"]][["coredata"]][["eid"]]
#   oa  <- contenido[["abstracts-retrieval-response"]][["coredata"]][["openaccess"]]
#
# DOCUMENTACIÓN: https://dev.elsevier.com/
# QUOTA CHECKER: https://dev.elsevier.com/api_key_settings.html
# =============================================================================
