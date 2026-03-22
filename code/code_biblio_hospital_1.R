# ==============================================================================
# ANÁLISIS BIBLIOMÉTRICO — HOSPITALES NIVEL III PERÚ
# code_biblio_hospital.R  |  Versión 5.0  |  Marzo 2026
# Autor: Frank Zela-Coila
# ==============================================================================
#
# QUE HACE ESTE SCRIPT:
#  1.  Importa 5 bases (Scopus, WoS, SciELO, PubMed, Embase)
#  2.  Fusiona y deduplica en una sola base limpia
#  3.  Trazabilidad: en que bases aparece cada articulo
#  4.  Recupera DOIs faltantes via CrossRef
#  5.  Asigna cuartil SJR desde tu CSV local de SCImago
#  6.  Citas actualizadas con API Scopus (institucional)
#  7.  Citas con API WoS Clarivate (institucional)
#  8.  Citas + Open Access + nombres de autores con OpenAlex (gratis)
#  9.  Citas influyentes + referencias con Semantic Scholar (gratis con key)
#  10. Detecta Open Access con Unpaywall (gratis)
#  11. Segregacion de autores: tabla AU1/F1/HOSP1/UNI1/G1/PAIS1 por articulo
#  12. Exporta Excel con base completa + segregacion UNIDAS (una fila por art)
#  13. Genera base lista para Biblioshiny
#
# INDICE:
#   SECCION 0 -- CONFIGURACION (editar aqui antes de ejecutar)
#   SECCION 1 -- PAQUETES
#   SECCION 2 -- FUNCIONES AUXILIARES
#     2A  Parser Embase RIS
#     2B  Fusion 5 bases + trazabilidad
#     2C  Cuartiles SJR desde CSV
#     2D  API Scopus — citas
#     2E  API WoS Clarivate — citas
#     2F  OpenAlex — citas + OA + nombres de autores
#     2G  Semantic Scholar — citas influyentes + referencias
#     2H  CrossRef — DOIs faltantes + citas basicas
#     2I  Unpaywall — Open Access
#     2J  Segregacion de autores (con bugs corregidos)
#     2K  Genderize.io — genero (opcional)
#   SECCION 3 -- PIPELINE PASO A PASO
# ==============================================================================


# ==============================================================================
# SECCION 0 — CONFIGURACION INICIAL
# ==============================================================================
# INSTRUCCION: Edita SOLO esta seccion. El resto lo puedes dejar como esta.
library(bibliometrix)
biblioshiny()
# -- 0A. RUTAS A TUS ARCHIVOS --------------------------------------------------
RUTA_SCOPUS  <- "data/scopus.bib"
RUTA_WOS     <- "data/wos.bib"
RUTA_SCIELO  <- "data/scielo.txt"
RUTA_PUBMED  <- "data/pubmed.txt"
RUTA_EMBASE  <- "data/embase.ris"          # Usar RIS, no CSV ni TXT
RUTA_SJR_CSV <- "data/scimagojr_2023.csv"  # Tu CSV descargado de SCImago

# -- 0B. API KEYS --------------------------------------------------------------
# Recomendado: guardar en .Renviron (usethis::edit_r_environ())
# y anadir: SCOPUS_API_KEY=tu_key  WOS_API_KEY=tu_key  etc.
# Si no, puedes escribirlas directamente aqui (solo uso local, NO subir a GitHub)

SCOPUS_API_KEY  <- Sys.getenv("SCOPUS_API_KEY",  "3925b30e9cbd00208d4f3b3a5dcbf48f")
WOS_API_KEY     <- Sys.getenv("WOS_API_KEY",     "66f74ac8754aeb0f4a85f16405f4da19f46da346")
SS_API_KEY      <- Sys.getenv("SS_API_KEY",      "XXsUDYZWbC29j6D15tSbe1lObzAJvb3m7Ir9xd35")
GENDERIZE_KEY   <- Sys.getenv("GENDERIZE_KEY",   "")     # vacio = plan gratuito (100/dia)
MI_EMAIL        <- Sys.getenv("MI_EMAIL",        "fzela@unsa.edu.pe")  # para OpenAlex y CrossRef

# -- 0C. OPCIONES DE CADA MODULO -----------------------------------------------
# Cambia a FALSE cualquier modulo que no quieras ejecutar.

USAR_API_SCOPUS  <- TRUE   # Citas Scopus (requiere IP institucional o VPN)
USAR_API_WOS     <- TRUE   # Citas WoS (requiere suscripcion institucional)
USAR_OPENALEX    <- TRUE   # Citas + OA + nombres autores (GRATIS, sin limite)
USAR_SEMSCHOLAR  <- TRUE   # Citas influyentes + referencias (GRATIS con key)
USAR_CROSSREF    <- TRUE   # Recuperar DOIs faltantes (GRATIS)
USAR_SJR         <- TRUE   # Cuartiles SCImago (necesita tu CSV local)
USAR_UNPAYWALL   <- TRUE   # Deteccion Open Access (GRATIS con email)
USAR_GENDERIZE   <- FALSE  # Genero (FALSE = columnas vacias para llenar manualmente)

PROB_MIN_GENDER  <- 0.75   # Probabilidad minima para aceptar genero de genderize
PAIS_GENDERIZE   <- "PE"   # Codigo ISO para mejorar precision en nombres hispanicos
AU_MAX           <- 10      # Maximo de autores a segregar por articulo
CARPETA_RESULTADOS <- "resultados"


# ==============================================================================
# SECCION 1 — PAQUETES
# ==============================================================================

paquetes <- c("bibliometrix","openxlsx","dplyr","stringr","httr",
              "jsonlite","tibble","purrr","tidyr","readr")

faltantes <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltantes) > 0) {
  message("Instalando: ", paste(faltantes, collapse=", "))
  install.packages(faltantes, dependencies=TRUE)
}
invisible(lapply(paquetes, library, character.only=TRUE))

# Operador auxiliar: devuelve b si a es NULL
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

if (!dir.exists(CARPETA_RESULTADOS)) dir.create(CARPETA_RESULTADOS, recursive=TRUE)
if (!dir.exists("data")) dir.create("data", recursive=TRUE)

cat("==============================================\n")
cat("  ANALISIS BIBLIOMETRICO — INICIO\n")
cat("  bibliometrix:", as.character(packageVersion("bibliometrix")), "\n")
cat("  Fecha:", format(Sys.Date(), "%d/%m/%Y"), "\n")
cat("==============================================\n\n")


# ==============================================================================
# SECCION 2 — FUNCIONES AUXILIARES
# ==============================================================================

# ------------------------------------------------------------------------------
# 2A — PARSER PERSONALIZADO EMBASE RIS
# ------------------------------------------------------------------------------
# Por que se necesita: bibliometrix >=4.x elimino soporte para dbsource="embase".
# Embase usa tags RIS no estandar: A1 para autores, M1 para afiliaciones, etc.
# Esta funcion lee el .ris y lo convierte al formato que bibliometrix espera.

importar_embase_ris <- function(ruta_ris) {

  cat("=== IMPORTANDO EMBASE RIS ===\n  Archivo:", ruta_ris, "\n")

  lineas <- tryCatch(
    readLines(ruta_ris, encoding="UTF-8", warn=FALSE),
    error=function(e) readLines(ruta_ris, encoding="latin1", warn=FALSE)
  )

  # Cada registro va de "TY  - " (inicio) a "ER  - " (fin)
  inicios <- grep("^TY  - ", lineas)
  fines   <- grep("^ER  - ", lineas)
  if (length(inicios)==0) stop("No se encontraron registros. Verifica que sea .ris de Embase.")
  cat("  Registros detectados:", length(inicios), "\n")

  parsear_uno <- function(bloque) {
    # get1: primer valor de un tag
    get1 <- function(tag) {
      p <- paste0("^", tag, "  - (.+)$")
      h <- grep(p, bloque, value=TRUE, perl=TRUE)
      if (length(h)==0) return(NA_character_)
      sub(p, "\\1", h[1], perl=TRUE)
    }
    # getAll: todos los valores de un tag, unidos con sep
    getAll <- function(tag, sep=";") {
      p <- paste0("^", tag, "  - (.+)$")
      h <- grep(p, bloque, value=TRUE, perl=TRUE)
      v <- sub(p, "\\1", h, perl=TRUE)
      if (length(v)==0) return(NA_character_)
      paste(trimws(v), collapse=sep)
    }

    # AUTORES: tag A1 en Embase (AU en estandar RIS)
    au <- {
      h <- grep("^A1  - ", bloque, value=TRUE)
      if (length(h)>0) toupper(paste(sub("^A1  - ","",h), collapse=";")) else NA_character_
    }

    # AFILIACIONES: tag M1 (+ AD como alternativa)
    c1 <- {
      h1 <- sub("^M1  - ","",grep("^M1  - ", bloque, value=TRUE))
      h2 <- sub("^AD  - ","",grep("^AD  - ", bloque, value=TRUE))
      t  <- c(h1, h2)
      if (length(t)>0) paste(trimws(t), collapse=";") else NA_character_
    }

    # KEYWORDS: tag KW (EMTREE + keywords autor mezclados)
    kw <- getAll("KW", ";")

    # PAGINAS
    sp <- get1("SP"); ep <- get1("EP")
    pp <- if (!is.na(sp) && !is.na(ep)) paste0(sp,"-",ep) else sp

    # DOI: limpiar prefijo https://doi.org/ si viene incluido
    doi <- get1("DO")
    if (!is.na(doi)) {
      doi <- sub("^https?://(dx\\.)?doi\\.org/","",doi,ignore.case=TRUE)
      doi <- trimws(doi)
      if (doi=="") doi <- NA_character_
    }

    # ANO: solo 4 digitos
    anio <- {
      a <- get1("Y1")
      if (!is.na(a)) as.integer(substr(trimws(a),1,4)) else NA_integer_
    }

    # UT: numero de acceso Embase con prefijo
    ut <- { u <- get1("U2"); if (!is.na(u)) paste0("EMBASE:",u) else NA_character_ }

    # PubMed ID: campo C5 en Embase RIS
    pm <- trimws(get1("C5") %||% NA_character_)
    if (identical(pm, "NA") || identical(pm, "")) pm <- NA_character_

    tipo <- get1("M3")
    if (is.na(tipo)) tipo <- sub("^TY  - ","",bloque[1])

    pau <- if (!is.na(au)) strsplit(au,";")[[1]][1] else "UNKNOWN"
    j9  <- trimws(get1("JO") %||% get1("JF") %||% NA_character_)

    data.frame(
      TI=toupper(trimws(get1("T1") %||% NA_character_)),
      AU=au, AF=c1, C1=c1, RP=NA_character_,
      PY=anio, SO=trimws(get1("JF") %||% NA_character_),
      J9=j9, JI=j9, SN=trimws(get1("SN") %||% NA_character_),
      VL=trimws(get1("VL") %||% NA_character_),
      IS=trimws(get1("IS") %||% NA_character_),
      SP=sp, EP=ep, PP=pp, DI=doi,
      AB=trimws(get1("N2") %||% NA_character_),
      DE=kw, ID=kw,
      LA=trimws(get1("LA") %||% NA_character_),
      DT=tipo, UT=ut, PM=pm,
      TC=0L,  # Embase NO exporta citas en ningun formato
      DB="EMBASE",
      SR_FULL=paste0(pau,", ",anio,", ",j9),
      SR=substr(paste(pau,anio,j9),1,40),
      stringsAsFactors=FALSE
    )
  }

  registros <- vector("list", length(inicios))
  for (i in seq_along(inicios)) {
    fin_i <- if (i<=length(fines)) fines[i] else length(lineas)
    registros[[i]] <- tryCatch(parsear_uno(lineas[inicios[i]:fin_i]),
                               error=function(e){message("  [Advertencia] Registro #",i,": ",e$message); NULL})
  }

  df <- do.call(rbind, Filter(Negate(is.null), registros))
  df <- df[!is.na(df$TI) & !is.na(df$PY), ]
  df$SOURCE_DB <- "Embase"

  cat("  Importados:", nrow(df), "| Con DOI:", sum(!is.na(df$DI)&df$DI!=""),
      "| Con PMID:", sum(!is.na(df$PM)&df$PM!=""),
      "| Con C1:", sum(!is.na(df$C1)), "\n\n")
  return(df)
}


# ------------------------------------------------------------------------------
# 2B — FUSION DE 5 BASES + TRAZABILIDAD
# ------------------------------------------------------------------------------
# 1. Fusiona Scopus+WoS+SciELO+PubMed con mergeDbSources (deduplicacion automatica)
# 2. Anade Embase solo con registros no presentes en las otras 4
# 3. Crea columna "Bases_indexadoras" (ej: "Scopus | WoS | PubMed")

fusionar_5_bases <- function(scopus, wos, scielo, pubmed, embase) {

  cat("=== FUSION Y DEDUPLICACION ===\n")

  base_4 <- mergeDbSources(scopus, wos, scielo, pubmed, remove.duplicated=TRUE)
  cat("  Tras merge Scopus+WoS+SciELO+PubMed:", nrow(base_4), "\n")

  # Igualar columnas entre base_4 y embase para que bind_rows funcione
  for (col in names(base_4)) if (!(col %in% names(embase))) embase[[col]] <- NA
  for (col in names(embase))  if (!(col %in% names(base_4))) base_4[[col]] <- NA

  base_4$TI_n  <- toupper(trimws(base_4$TI))
  embase$TI_n  <- toupper(trimws(embase$TI))

  dois_ok    <- base_4$DI[!is.na(base_4$DI) & base_4$DI!=""]
  titulos_ok <- base_4$TI_n[!is.na(base_4$TI_n)]

  embase_unicos <- embase %>%
    filter((is.na(DI)|DI==""|!(DI %in% dois_ok)) & !(TI_n %in% titulos_ok))
  cat("  Registros unicos Embase:", nrow(embase_unicos), "\n")

  base_total <- bind_rows(base_4, embase_unicos) %>% select(-TI_n)
  cat("  TOTAL 5 bases:", nrow(base_total), "articulos unicos\n\n")

  # Trazabilidad
  inventario <- bind_rows(
    scopus %>% select(TI,DI,SOURCE_DB) %>% mutate(TI=toupper(trimws(TI))),
    wos    %>% select(TI,DI,SOURCE_DB) %>% mutate(TI=toupper(trimws(TI))),
    scielo %>% select(TI,DI,SOURCE_DB) %>% mutate(TI=toupper(trimws(TI))),
    pubmed %>% select(TI,DI,SOURCE_DB) %>% mutate(TI=toupper(trimws(TI))),
    embase %>% select(TI,DI,SOURCE_DB) %>% mutate(TI=toupper(trimws(TI)))
  )

  trazabilidad <- inventario %>%
    group_by(TI) %>%
    summarise(Bases_indexadoras=paste(unique(SOURCE_DB),collapse=" | "),
              N_bases=n_distinct(SOURCE_DB), .groups="drop") %>%
    rename(TI_n=TI)

  base_total <- base_total %>%
    mutate(TI_n=toupper(trimws(TI))) %>%
    left_join(trazabilidad, by="TI_n") %>%
    select(-TI_n)

  cat("  Cobertura por base:\n"); print(table(inventario$SOURCE_DB))
  cat("\n  Articulos en N bases:\n"); print(table(trazabilidad$N_bases)); cat("\n")

  list(base=base_total, inventario=inventario, trazabilidad=trazabilidad)
}


# ------------------------------------------------------------------------------
# 2C — CUARTILES SJR DESDE CSV LOCAL
# ------------------------------------------------------------------------------
# SCImago Journal Rankings: cuartil Q1-Q4 por ISSN de la revista.
# Bugs del CSV original corregidos:
#   - ISSNs sin guion ("15424863" -> "1542-4863")
#   - Columna Publisher duplicada (name_repair="unique")
#   - Coma decimal en SJR europeo
#   - ~291 filas con numero donde deberia ir Q

cargar_sjr <- function(ruta_csv, anio=2023) {
  if (!file.exists(ruta_csv)) {
    warning("CSV SJR no encontrado: ",ruta_csv,
            "\nDescargalo en: https://www.scimagojr.com/journalrank.php?year=",anio)
    return(NULL)
  }
  cat("  Cargando SJR", anio, "...\n")
  sjr_raw <- readr::read_delim(ruta_csv, delim=";",
                                locale=readr::locale(decimal_mark=",",encoding="UTF-8"),
                                show_col_types=FALSE, name_repair="unique")
  sjr <- sjr_raw %>%
    select(Title, Issn,
           SJR_raw=SJR, SJR_Q_raw=`SJR Best Quartile`,
           H_index=`H index`, OA=`Open Access`,
           Country, Areas, Categories) %>%
    mutate(
      SJR_Best_Quartile=case_when(SJR_Q_raw %in% c("Q1","Q2","Q3","Q4")~SJR_Q_raw,
                                   TRUE~NA_character_),
      SJR=as.numeric(str_replace(as.character(SJR_raw),",","."))
    ) %>% select(-SJR_Q_raw,-SJR_raw)

  norm_issn <- function(x) {
    d <- str_remove_all(str_trim(str_extract(x,"[0-9]{4}-?[0-9]{3}[0-9Xx]")),"-")
    if (is.na(d)||nchar(d)!=8) return(NA_character_)
    paste0(substr(d,1,4),"-",substr(d,5,8))
  }
  sjr <- sjr %>%
    mutate(ISSN1=sapply(str_trim(str_extract(Issn,"^[^,]+")),norm_issn,USE.NAMES=FALSE),
           ISSN2=sapply(str_trim(str_extract(Issn,"(?<=,)[^,]+")),norm_issn,USE.NAMES=FALSE))

  cat("  Revistas:", nrow(sjr), "| Con cuartil:", sum(!is.na(sjr$SJR_Best_Quartile)), "\n")
  return(sjr)
}

asignar_cuartil_sjr <- function(df, sjr_df, col_issn="SN") {
  if (is.null(sjr_df)) { cat("  SJR no disponible, omitido.\n"); return(df) }

  lookup <- bind_rows(
    sjr_df %>% filter(!is.na(ISSN1)) %>%
      select(ISSN=ISSN1, SJR_Q=SJR_Best_Quartile, SJR_score=SJR, SJR_H=H_index, SJR_OA=OA),
    sjr_df %>% filter(!is.na(ISSN2)) %>%
      select(ISSN=ISSN2, SJR_Q=SJR_Best_Quartile, SJR_score=SJR, SJR_H=H_index, SJR_OA=OA)
  ) %>% filter(!is.na(ISSN)) %>%
    arrange(ISSN, desc(SJR_score)) %>%
    distinct(ISSN, .keep_all=TRUE)

  df <- df %>%
    mutate(ISSN_tmp={
      sn <- trimws(.data[[col_issn]])
      i1 <- str_extract(sn,"[0-9]{4}-[0-9]{3}[0-9Xx]")
      d  <- str_extract(sn,"[0-9]{7}[0-9Xx]")
      i2 <- ifelse(!is.na(d),paste0(substr(d,1,4),"-",substr(d,5,8)),NA_character_)
      ifelse(!is.na(i1),i1,i2)
    }) %>%
    left_join(lookup, by=c("ISSN_tmp"="ISSN")) %>%
    select(-ISSN_tmp)

  cat("  SJR asignado:", sum(!is.na(df$SJR_Q)), "| Dist:"); print(table(df$SJR_Q,useNA="no")); cat("\n")
  return(df)
}


# ------------------------------------------------------------------------------
# 2D — API SCOPUS: CITAS ACTUALIZADAS
# ------------------------------------------------------------------------------
# Devuelve citedby-count desde Scopus en tiempo real.
# Requiere IP institucional o VPN + API key de Elsevier.
# Limite: 9 req/seg, 20.000/semana (plan basico).
# Documentacion: https://dev.elsevier.com/

get_citas_scopus_doi <- function(doi, api_key, pausa=0.4) {
  if (is.na(doi)||trimws(doi)=="") return(NA_integer_)
  url  <- paste0("https://api.elsevier.com/content/abstract/doi/",
                 utils::URLencode(doi, repeated=TRUE))
  resp <- tryCatch(
    httr::GET(url, httr::add_headers("X-ELS-APIKey"=api_key,"Accept"="application/json"),
              httr::timeout(12)),
    error=function(e) NULL)
  Sys.sleep(pausa)
  if (is.null(resp)||httr::status_code(resp)!=200) {
    if (!is.null(resp)&&httr::status_code(resp)==429){message("  Rate limit Scopus -> espera 15s..."); Sys.sleep(15)}
    return(NA_integer_)
  }
  d <- tryCatch(jsonlite::fromJSON(httr::content(resp,as="text",encoding="UTF-8"),simplifyVector=TRUE),error=function(e)NULL)
  tryCatch(as.integer(d[["abstracts-retrieval-response"]][["coredata"]][["citedby-count"]]),error=function(e)NA_integer_)
}

get_citas_scopus_titulo <- function(titulo, api_key, pausa=0.7) {
  if (is.na(titulo)||nchar(trimws(titulo))<15) return(NA_integer_)
  resp <- tryCatch(
    httr::GET("https://api.elsevier.com/content/search/scopus",
              query=list(query=paste0('TITLE("',substr(gsub('"',"",titulo),1,100),'")'),
                         field="citedby-count",count="1"),
              httr::add_headers("X-ELS-APIKey"=api_key,"Accept"="application/json"),
              httr::timeout(12)),
    error=function(e)NULL)
  Sys.sleep(pausa)
  if (is.null(resp)||httr::status_code(resp)!=200) return(NA_integer_)
  d <- tryCatch(jsonlite::fromJSON(httr::content(resp,as="text",encoding="UTF-8"),simplifyVector=TRUE),error=function(e)NULL)
  tryCatch(as.integer(d[["search-results"]][["entry"]][[1]][["citedby-count"]]),error=function(e)NA_integer_)
}

enriquecer_citas_scopus <- function(df, api_key) {
  n <- nrow(df)
  df$TC_Scopus    <- NA_integer_
  df$TC_Scopus_via <- NA_character_
  cat("=== CITAS VIA API SCOPUS ===\n  Articulos:", n,
      "| Tiempo estimado: ~", round(n*0.6/60,1), "min\n\n")
  for (i in seq_len(n)) {
    doi_i <- df$DI[i]
    if (!is.na(doi_i)&&doi_i!="") {
      df$TC_Scopus[i]    <- get_citas_scopus_doi(doi_i, api_key)
      df$TC_Scopus_via[i]<- "DOI"
    } else {
      df$TC_Scopus[i]    <- get_citas_scopus_titulo(df$TI[i], api_key)
      df$TC_Scopus_via[i]<- "Titulo"
    }
    if (i%%10==0||i==n)
      cat(sprintf("  [%d/%d] OK:%d NA:%d\n",i,n,sum(!is.na(df$TC_Scopus[1:i])),sum(is.na(df$TC_Scopus[1:i]))))
    if (i%%50==0) saveRDS(df,"data/backup_scopus.rds")
  }
  cat("  Cobertura:", round(mean(!is.na(df$TC_Scopus))*100,1),"%\n\n")
  return(df)
}


# ------------------------------------------------------------------------------
# 2E — API WoS CLARIVATE: CITAS EN WEB OF SCIENCE
# ------------------------------------------------------------------------------
# Requiere suscripcion institucional a WoS.
# Endpoint: WOS Starter API v1 (gratuita para instituciones suscritas).
# Key: https://developer.clarivate.com

get_citas_wos_doi <- function(doi, api_key=WOS_API_KEY, pausa=0.4) {
  if (is.na(doi)||trimws(doi)==""|nchar(api_key)==0) return(NULL)
  doi_l <- sub("^https?://(dx\\.)?doi\\.org/","",doi,ignore.case=TRUE)
  resp <- tryCatch(
    httr::GET("https://api.clarivate.com/apis/wos-starter/v1/documents",
              query=list(db="WOS",q=paste0("DO=(",doi_l,")"),limit=1,page=1),
              httr::add_headers("X-ApiKey"=api_key), httr::timeout(15)),
    error=function(e)NULL)
  Sys.sleep(pausa)
  if (is.null(resp)||httr::status_code(resp)!=200) return(NULL)
  d <- tryCatch(jsonlite::fromJSON(httr::content(resp,as="text",encoding="UTF-8"),simplifyVector=FALSE),error=function(e)NULL)
  if (is.null(d)||length(d$hits)==0) return(NULL)
  tibble::tibble(doi_input=doi,
                 WOS_citas=as.integer(d$hits[[1]]$times_cited %||% NA),
                 WOS_uid=as.character(d$hits[[1]]$uid %||% NA))
}

enriquecer_citas_wos <- function(df, api_key=WOS_API_KEY) {
  cat("=== CITAS VIA API WoS CLARIVATE ===\n")
  n <- nrow(df); res <- vector("list",n)
  for (i in seq_len(n)) {
    res[[i]] <- get_citas_wos_doi(df$DI[i], api_key)
    if (i%%20==0||i==n) cat(sprintf("  [%d/%d] OK:%d\n",i,n,sum(!sapply(res[1:i],is.null))))
    if (i%%50==0) saveRDS(df,"data/backup_wos.rds")
  }
  res_df <- bind_rows(res)
  if (nrow(res_df)>0) df <- left_join(df, res_df %>% rename(DI=doi_input), by="DI")
  cat("  WoS citas recuperadas:", sum(!is.na(df$WOS_citas)),"\n\n")
  return(df)
}


# ------------------------------------------------------------------------------
# 2F — OPENALEX: CITAS + OPEN ACCESS + NOMBRES COMPLETOS DE AUTORES
# ------------------------------------------------------------------------------
# GRATIS, sin key, sin limite diario.
# Solo necesita tu email en el User-Agent para acceso al pool premium (mas rapido).
# Endpoint: https://api.openalex.org/works/doi:{DOI}
#
# Que devuelve:
#   cited_by_count  -> citas totales (actualizado diariamente)
#   is_oa           -> Open Access si/no
#   oa_url          -> URL al PDF gratuito
#   authorships     -> autores con nombre COMPLETO, ORCID, institucion, pais
#                      <- CLAVE para recuperar nombres completos de Embase/WoS
#   concepts        -> areas tematicas asignadas automaticamente
#
# NOTA sobre el ejemplo que mencionaste:
#   curl "https://api.openalex.org/works/W2741809807"
#   W2741809807 es el ID interno de OpenAlex. Tambien funciona por DOI:
#   https://api.openalex.org/works/doi:10.1016/j.joi.2017.08.007

openalex_por_doi <- function(doi, email=MI_EMAIL, pausa=0.12) {
  if (is.na(doi)||trimws(doi)=="") return(NULL)
  doi_l <- sub("^https?://(dx\\.)?doi\\.org/","",doi,ignore.case=TRUE)
  resp  <- tryCatch(
    httr::GET(paste0("https://api.openalex.org/works/doi:",doi_l),
              httr::add_headers("User-Agent"=paste0("bibliometrix-peru/1.0 (",email,")")),
              httr::timeout(10)),
    error=function(e)NULL)
  Sys.sleep(pausa)
  if (is.null(resp)||httr::status_code(resp)!=200) return(NULL)
  d <- tryCatch(jsonlite::fromJSON(httr::content(resp,as="text",encoding="UTF-8"),simplifyVector=FALSE),error=function(e)NULL)
  if (is.null(d)) return(NULL)

  # Extraer autores con nombre completo, ORCID, institucion y pais
  autores_oa <- if (length(d$authorships)>0) {
    purrr::imap_dfr(d$authorships, function(a,pos) {
      tibble::tibble(
        doi_oa       = doi,
        posicion     = as.integer(pos),
        nombre_oa    = as.character(a$author$display_name %||% NA),
        orcid_oa     = as.character(a$author$orcid       %||% NA),
        inst_oa      = if (length(a$institutions)>0) as.character(a$institutions[[1]]$display_name %||% NA) else NA_character_,
        pais_oa      = if (length(a$institutions)>0) as.character(a$institutions[[1]]$country_code %||% NA) else NA_character_
      )
    })
  } else NULL

  list(
    resumen=tibble::tibble(
      doi_input  = doi,
      OA_citas   = as.integer(d$cited_by_count  %||% NA),
      OA_es_oa   = as.logical(d$open_access$is_oa %||% NA),
      OA_url_pdf = as.character(d$open_access$oa_url %||% NA),
      OA_tipo    = as.character(d$open_access$oa_status %||% NA),
      OA_areas   = {
        cs <- d$concepts
        if (length(cs)>0) paste(sapply(cs[1:min(3,length(cs))],`[[`,"display_name"),collapse="; ")
        else NA_character_
      }
    ),
    autores = autores_oa
  )
}

enriquecer_openalex <- function(df, email=MI_EMAIL) {
  cat("=== CITAS + OA + AUTORES VIA OPENALEX (gratis) ===\n")
  n <- nrow(df); res <- vector("list",n); autores <- vector("list",n)
  for (i in seq_len(n)) {
    r <- openalex_por_doi(df$DI[i], email)
    if (!is.null(r)) { res[[i]] <- r$resumen; autores[[i]] <- r$autores }
    if (i%%20==0||i==n) cat(sprintf("  [%d/%d] OK:%d\n",i,n,sum(!sapply(res[1:i],is.null))))
  }
  res_df <- bind_rows(res)
  if (nrow(res_df)>0) df <- left_join(df, res_df %>% rename(DI=doi_input), by="DI")
  cat("  OA citas:", sum(!is.na(df$OA_citas)),
      "| Open Access:", sum(df$OA_es_oa==TRUE,na.rm=TRUE),"\n\n")
  list(df=df, autores_oa=bind_rows(autores))
}


# ------------------------------------------------------------------------------
# 2G — SEMANTIC SCHOLAR: CITAS INFLUYENTES + REFERENCIAS COMPLETAS
# ------------------------------------------------------------------------------
# Allen Institute for AI — indexa >200 millones de papers.
# VENTAJA UNICA: devuelve las REFERENCIAS CITADAS completas.
# Esto permite analisis de co-citacion y acoplamiento bibliografico.
#
# API Key: XXsUDYZWbC29j6D15tSbe1lObzAJvb3m7Ir9xd35
# Con key: 100 req/seg | Sin key: 10 req/seg
# Registro: https://www.semanticscholar.org/product/api
#
# Campos que recuperamos:
#   citationCount           -> citas totales en Semantic Scholar
#   influentialCitationCount -> citas "influyentes" (papers que lo usan como base)
#   isOpenAccess            -> si es Open Access
#   openAccessPdf.url       -> URL al PDF
#   references              -> lista de referencias (DOIs para co-citacion)

get_ss_doi <- function(doi, api_key=SS_API_KEY, pausa=0.15) {
  if (is.na(doi)||trimws(doi)=="") return(NULL)
  doi_l  <- sub("^https?://(dx\\.)?doi\\.org/","",doi,ignore.case=TRUE)
  url    <- paste0("https://api.semanticscholar.org/graph/v1/paper/DOI:",
                   utils::URLencode(doi_l))
  campos <- "citationCount,influentialCitationCount,isOpenAccess,openAccessPdf,references.externalIds,authors.name,authors.authorId"

  headers <- if (nchar(api_key)>0)
    httr::add_headers("x-api-key"=api_key, "Accept"="application/json")
  else
    httr::add_headers("Accept"="application/json")

  resp <- tryCatch(
    httr::GET(url, query=list(fields=campos), headers, httr::timeout(12)),
    error=function(e)NULL)
  Sys.sleep(pausa)
  if (is.null(resp)||httr::status_code(resp)!=200) return(NULL)
  d <- tryCatch(jsonlite::fromJSON(httr::content(resp,as="text",encoding="UTF-8"),simplifyVector=FALSE),error=function(e)NULL)
  if (is.null(d)) return(NULL)

  # Referencias: extraer DOIs para analisis de co-citacion posterior
  refs_doi <- tryCatch({
    refs <- d$references %||% list()
    dois <- sapply(refs, function(r) r$externalIds$DOI %||% NA)
    paste(na.omit(dois), collapse="; ")
  }, error=function(e) NA_character_)
  if (identical(refs_doi,"")) refs_doi <- NA_character_

  tibble::tibble(
    doi_input      = doi,
    SS_citas       = as.integer(d$citationCount %||% NA),
    SS_citas_infl  = as.integer(d$influentialCitationCount %||% NA),
    SS_oa          = as.logical(d$isOpenAccess %||% NA),
    SS_pdf_url     = as.character(d$openAccessPdf$url %||% NA),
    SS_n_refs      = length(d$references %||% list()),
    SS_refs_dois   = refs_doi  # DOIs de todas las referencias (util para co-citacion)
  )
}

enriquecer_semantic_scholar <- function(df, api_key=SS_API_KEY) {
  cat("=== CITAS + REFERENCIAS VIA SEMANTIC SCHOLAR ===\n")
  cat("  API Key: presente (", nchar(api_key), "chars) -> 100 req/seg\n")
  n <- nrow(df); res <- vector("list",n)
  for (i in seq_len(n)) {
    res[[i]] <- get_ss_doi(df$DI[i], api_key)
    if (i%%20==0||i==n) cat(sprintf("  [%d/%d] OK:%d\n",i,n,sum(!sapply(res[1:i],is.null))))
    if (i%%50==0) saveRDS(df,"data/backup_ss.rds")
  }
  res_df <- bind_rows(res)
  if (nrow(res_df)>0) df <- left_join(df, res_df %>% rename(DI=doi_input), by="DI")
  cat("  SS citas:", sum(!is.na(df$SS_citas)),
      "| Influyentes:", sum(!is.na(df$SS_citas_infl)),
      "| Con referencias:", sum(!is.na(df$SS_refs_dois)),"\n\n")
  return(df)
}


# ------------------------------------------------------------------------------
# 2H — CROSSREF: DOIs FALTANTES + CITAS BASICAS
# ------------------------------------------------------------------------------
# GRATIS con email en User-Agent (Polite Pool: 50 req/seg).
# Busca el DOI por titulo + primer autor. Solo acepta si score > 80.
# Tambien recupera "is-referenced-by-count" (citas CrossRef — menos que Scopus).

buscar_doi_crossref <- function(titulo, au1=NULL, anio=NULL, email=MI_EMAIL) {
  if (is.na(titulo)||nchar(trimws(titulo))<15) return(list(doi=NA,cr_citas=NA))
  params <- list(`query.title`=substr(titulo,1,120), rows="1",
                 select="DOI,score,title,is-referenced-by-count")
  if (!is.null(au1)&&!is.na(au1)) params$`query.author` <- au1
  if (!is.null(anio)&&!is.na(anio))
    params$filter <- paste0("from-pub-date:",anio,",until-pub-date:",anio)

  resp <- tryCatch(
    httr::GET("https://api.crossref.org/works", query=params,
              httr::add_headers("User-Agent"=paste0("bibliometrix/1.0 (",email,")")),
              httr::timeout(10)),
    error=function(e)NULL)
  Sys.sleep(0.12)
  if (is.null(resp)||httr::status_code(resp)!=200) return(list(doi=NA,cr_citas=NA))
  d <- tryCatch(jsonlite::fromJSON(httr::content(resp,as="text",encoding="UTF-8"),simplifyVector=TRUE),error=function(e)NULL)
  if (is.null(d)) return(list(doi=NA,cr_citas=NA))
  items <- d$message$items
  if (length(items)==0||nrow(as.data.frame(items))==0) return(list(doi=NA,cr_citas=NA))
  if (!is.na(items$score[1])&&items$score[1]>80)
    return(list(doi=items$DOI[1], cr_citas=as.integer(items$`is-referenced-by-count`[1])))
  return(list(doi=NA,cr_citas=NA))
}

recuperar_dois_faltantes <- function(df, email=MI_EMAIL) {
  sin_doi <- which(is.na(df$DI)|df$DI=="")
  cat("=== RECUPERANDO DOIs FALTANTES VIA CROSSREF ===\n  Sin DOI:", length(sin_doi), "\n")
  if (!("CR_citas" %in% names(df))) df$CR_citas <- NA_integer_
  recuperados <- 0
  for (idx in sin_doi) {
    au1 <- if (!is.na(df$AU[idx])) strsplit(df$AU[idx],";")[[1]][1] else NULL
    r   <- buscar_doi_crossref(df$TI[idx], au1, df$PY[idx], email)
    if (!is.na(r$doi)) {
      df$DI[idx] <- r$doi
      if (!is.na(r$cr_citas)) df$CR_citas[idx] <- r$cr_citas
      recuperados <- recuperados+1
    }
  }
  cat("  DOIs recuperados:", recuperados, "de", length(sin_doi), "\n\n")
  return(df)
}


# ------------------------------------------------------------------------------
# 2I — UNPAYWALL: DETECCION DE OPEN ACCESS
# ------------------------------------------------------------------------------
# GRATIS con email. Devuelve si el articulo es OA y el URL del PDF legal.
# Complementa lo que devuelve OpenAlex (Unpaywall es la fuente original de OA).

get_unpaywall_doi <- function(doi, email=MI_EMAIL, pausa=0.1) {
  if (is.na(doi)||trimws(doi)=="") return(NULL)
  doi_l <- sub("^https?://(dx\\.)?doi\\.org/","",doi,ignore.case=TRUE)
  resp  <- tryCatch(
    httr::GET(paste0("https://api.unpaywall.org/v2/",utils::URLencode(doi_l)),
              query=list(email=email), httr::timeout(10)),
    error=function(e)NULL)
  Sys.sleep(pausa)
  if (is.null(resp)||httr::status_code(resp)!=200) return(NULL)
  d <- tryCatch(jsonlite::fromJSON(httr::content(resp,as="text",encoding="UTF-8"),simplifyVector=TRUE),error=function(e)NULL)
  if (is.null(d)) return(NULL)
  tibble::tibble(
    doi_input = doi,
    UPW_oa    = as.logical(d$is_oa %||% NA),
    UPW_tipo  = as.character(d$oa_status %||% NA),
    UPW_url   = as.character(d$best_oa_location$url %||% NA)
  )
}

enriquecer_unpaywall <- function(df, email=MI_EMAIL) {
  cat("=== OPEN ACCESS VIA UNPAYWALL ===\n")
  n <- nrow(df); res <- vector("list",n)
  for (i in seq_len(n)) {
    res[[i]] <- get_unpaywall_doi(df$DI[i], email)
    if (i%%30==0||i==n) cat(sprintf("  [%d/%d] OK:%d\n",i,n,sum(!sapply(res[1:i],is.null))))
  }
  res_df <- bind_rows(res)
  if (nrow(res_df)>0) df <- left_join(df, res_df %>% rename(DI=doi_input), by="DI")
  cat("  Open Access detectado:", sum(df$UPW_oa==TRUE,na.rm=TRUE),"\n\n")
  return(df)
}


# ------------------------------------------------------------------------------
# 2J — SEGREGACION DE AUTORES
# ------------------------------------------------------------------------------
# Genera tabla con una fila por ARTICULO y columnas:
#   AU1, F1, HOSP1, UNI1, G1, PAIS1  (primer autor)
#   AU2, F2, HOSP2, UNI2, G2, PAIS2  (segundo autor)
#   ... hasta AU_MAX
#   AU_CORRESPONSAL: posicion numerica del autor correspondiente
#
# FUENTES:
#   AU_n   -> campo AU (siempre disponible)
#   F_n    -> campo C1: "[Autor] Inst" (Scopus/WoS) o "(Autor) Inst" (Embase)
#   HOSP_n -> diccionario de los 50 hospitales peruanos
#   UNI_n  -> diccionario de universidades peruanas + deteccion generica
#   PAIS_n -> ultimo elemento separado por coma en la afiliacion
#   G_n    -> genderize.io (si USAR_GENDERIZE=TRUE) o vacio (llenar manualmente)
#   AU_CORRESPONSAL -> campo RP (Scopus/WoS) -- NO disponible en Embase

# Diccionario de 50 hospitales peruanos con aliases
HOSPITALES <- list(
  "Hospital Goyeneche (Arequipa)"       =c("Goyeneche"),
  "HNCASE - Seguin Escobedo"            =c("Seguin Escobedo","HNCASE","Carlos Alberto Seguin","Carlos Alberto Seg"),
  "HRHDE - Honorio Delgado"             =c("Honorio Delgado Espinoza","HRHDE"),
  "HNDAC - Daniel Alcides Carrion"      =c("Daniel Alcides Carr","HNDAC"),
  "Centro Medico Naval"                 =c("Centro Medico Naval","Naval","CMST","Santiago Tavara"),
  "HASS - Alberto Sabogal"              =c("Sabogal","HASS"),
  "HRCU - Regional Cusco"               =c("Regional.*Cusco","HRCU","Hospital.*Cusco"),
  "Antonio Lorena"                      =c("Antonio Lorena"),
  "EsSalud Cusco - Guevara Velasco"     =c("Adolfo Guevara","Guevara Velasco"),
  "HNRPP - Ramiro Priale"               =c("Ramiro Priale","HNRPP","Priale Perales"),
  "Hospital Belen Trujillo"             =c("Belen.*Trujillo","Hospital Belen"),
  "HVLE - Victor Lazarte"               =c("Victor Lazarte","Lazarte Echegaray","HVLE"),
  "HRDT - Regional Docente Trujillo"    =c("Regional Docente.*Trujillo","HRDT"),
  "HRL - Regional Lambayeque"           =c("Regional Lambayeque","HRL"),
  "HNAAA - Almanzor Aguinaga"           =c("Almanzor Aguinaga","HNAAA"),
  "HNHU - Hipolito Unanue"              =c("Hipolito Unanue","HNHU"),
  "HMA - Maria Auxiliadora"             =c("Maria Auxiliadora","HMA"),
  "HNDM - Dos de Mayo"                  =c("Dos de Mayo","HNDM"),
  "HNCH - Cayetano Heredia"             =c("Cayetano Heredia","HNCH"),
  "HNSEB - Sergio Bernales"             =c("Sergio Bernales","HNSEB"),
  "Hospital FAP"                        =c("Fuerza Aerea","FAP.*Peru","Hospital FAP"),
  "Hospital Militar Central"            =c("Hospital Militar","Luis Arias Schreiber"),
  "HEVES - Villa El Salvador"           =c("Villa El Salvador","HEVES"),
  "Clinica Delgado - AUNA"              =c("Clinica Delgado","AUNA"),
  "HNAL - Arzobispo Loayza"             =c("Arzobispo Loayza","Loayza","HNAL"),
  "HASR - Hospital Santa Rosa"          =c("Hospital Santa Rosa","HASR"),
  "Hospital PNP - Luis Saenz"           =c("Luis Saenz","PNP.*Peru","Policia Nacional"),
  "IREN Sur"                            =c("IREN Sur","IREN.*Arequipa","Neoplasicas.*Sur"),
  "IREN Norte - Pinillos Ganoza"        =c("IREN Norte","Pinillos Ganoza"),
  "IRO - Oncologia Regional"            =c("\\bIRO\\b","Instituto Regional de Oncolog"),
  "INO - Oftalmologia"                  =c("\\bINO\\b","Contreras Campos","Oftalmolog"),
  "INMP - Materno Perinatal"            =c("Materno Perinatal","\\bINMP\\b","Instituto Materno"),
  "INCN - Ciencias Neurologicas"        =c("Ciencias Neurologicas","\\bINCN\\b"),
  "INEN"                                =c("\\bINEN\\b","Enfermedades Neopl"),
  "HNGAI - Guillermo Almenara"          =c("Guillermo Almenara","Almenara","HNGAI"),
  "HNERM - Rebagliati Martins"          =c("Rebagliati","HNERM","Edgardo Rebagliati"),
  "INSNSB - INSN San Borja"             =c("INSN San Borja","INSNSB","Salud del Nino San Borja"),
  "INSN - Brena"                        =c("\\bINSN\\b","Instituto Nacional de Salud del Nino"),
  "INSM - Salud Mental"                 =c("Honorio Delgado Hideyo Noguchi","\\bINSM\\b","Hideyo Noguchi"),
  "IML - Medicina Legal"                =c("Medicina Legal","Avendano Ureta","Leonidas Avendano"),
  "Clinica Ricardo Palma"               =c("Ricardo Palma","Clinica Ricardo Palma"),
  "Clinica San Pablo"                   =c("\\bSan Pablo\\b","Clinica San Pablo"),
  "Clinica San Borja"                   =c("\\bSan Borja\\b"),
  "Clinica San Felipe"                  =c("\\bSan Felipe\\b"),
  "Hospital III Iquitos"                =c("\\bIquitos\\b"),
  "HRLI - Regional Loreto"              =c("Arriola Iglesias","Regional de Loreto"),
  "HNRP - Rebagliati"                   =c("Rebagliati Martins"),
  "Clinica del Inca"                    =c("Clinica del Inca"),
  "HEVES - Victor Larco Herrera"        =c("Victor Larco Herrera"),
  "Hospital Regional Piura"             =c("Regional.*Piura","Hospital.*Piura")
)

# Diccionario universidades peruanas
UNIVERSIDADES <- c(
  "Universidad Nacional Mayor de San Marcos"="UNMSM",
  "Universidad Peruana Cayetano Heredia"="UPCH",
  "Pontificia Universidad Catolica del Peru"="PUCP",
  "Universidad de Lima"="UdL",
  "Universidad Nacional de San Agustin"="UNSA",
  "Universidad Nacional de Trujillo"="UNT",
  "Universidad Privada Antenor Orrego"="UPAO",
  "Universidad Nacional de Piura"="UNP",
  "Universidad Nacional del Altiplano"="UNA",
  "Universidad Nacional de la Amazonia Peruana"="UNAP",
  "Universidad Nacional de Cajamarca"="UNC",
  "Universidad Ricardo Palma"="URP",
  "Universidad San Martin de Porres"="USMP",
  "Universidad Cientifica del Sur"="UCSUR",
  "Universidad Continental"="UC",
  "Universidad Andina del Cusco"="UAC",
  "Universidad Nacional de San Antonio Abad"="UNSAAC"
)

KW_UNIVERSIDAD <- c("University","Universidad","Universita","Universite",
                    "Universidade","Faculty","Facultad","School of Medicine",
                    "Medical School","College","Institute","Instituto")

detectar_hospital <- function(afil) {
  if (length(afil)==0) return(NA_character_)
  afil <- afil[1]
  if (is.na(afil)||trimws(afil)=="") return(NA_character_)
  afil_u <- toupper(afil)
  matches <- character(0)
  for (nombre in names(HOSPITALES)) {
    if (any(sapply(toupper(HOSPITALES[[nombre]]),function(p) grepl(p,afil_u,perl=TRUE))))
      matches <- c(matches,nombre)
  }
  if (length(matches)==0) return(NA_character_)
  paste(unique(matches),collapse=" | ")
}

detectar_universidad <- function(afil) {
  if (length(afil)==0) return(NA_character_)
  afil <- afil[1]
  if (is.na(afil)||trimws(afil)=="") return(NA_character_)
  afil_u <- toupper(afil)
  for (nombre in names(UNIVERSIDADES)) {
    if (grepl(toupper(nombre),afil_u,fixed=TRUE)||
        grepl(toupper(UNIVERSIDADES[[nombre]]),afil_u,fixed=TRUE)) return(nombre)
  }
  kw_u <- toupper(KW_UNIVERSIDAD)
  if (any(sapply(kw_u,function(k) grepl(k,afil_u,fixed=TRUE)))) {
    for (seg in str_split(afil,",")[[1]]) {
      if (any(sapply(toupper(KW_UNIVERSIDAD),function(k) grepl(k,toupper(seg),fixed=TRUE))))
        return(trimws(seg))
    }
  }
  return(NA_character_)
}

extraer_pais <- function(afil) {
  if (length(afil)==0) return(NA_character_)
  afil <- afil[1]
  if (is.na(afil)||trimws(afil)=="") return(NA_character_)
  paises <- c("Peru","Argentina","Brazil","Brasil","Chile","Colombia","Mexico",
              "Ecuador","Bolivia","Venezuela","Uruguay","Paraguay","Panama",
              "Costa Rica","Honduras","Guatemala","El Salvador","Nicaragua",
              "Cuba","Dominican Republic","Puerto Rico","United States","USA",
              "United Kingdom","UK","Spain","France","Germany","Italy",
              "Netherlands","Switzerland","Sweden","Australia","Canada",
              "Japan","China","South Korea","India","Portugal","Belgium",
              "Denmark","Norway","Finland","New Zealand","Turkey","Israel")
  afil_u <- toupper(afil)
  segs   <- rev(str_split(afil,",")[[1]])
  for (seg in segs[1:min(3,length(segs))])
    for (p in paises)
      if (grepl(paste0("\\b",toupper(p),"\\b"),toupper(seg))) return(p)
  for (p in paises)
    if (grepl(paste0("\\b",toupper(p),"\\b"),afil_u)) return(p)
  return(NA_character_)
}

# Parsear C1 para mapear autor -> afiliacion
# Scopus/WoS: "[Apellido, I.] Institucion, Ciudad, Pais"
# Embase:     "(Apellido N.) Institucion, Ciudad, Pais"
parsear_afiliaciones_c1 <- function(c1_raw) {
  if (is.na(c1_raw)||trimws(c1_raw)=="") return(NULL)
  if (str_detect(c1_raw,"\\[[^\\]]+\\]")) {
    segs <- str_split(c1_raw,"(?=\\[)")[[1]]
    segs <- segs[nchar(trimws(segs))>2]
    purrr::map_dfr(segs, function(seg) {
      au_str  <- str_extract(seg,"(?<=\\[)[^\\]]+")
      inst    <- str_trim(str_remove(seg,"\\[[^\\]]+\\]\\s*;?\\s*"))
      if (is.na(au_str)) return(NULL)
      aus <- trimws(str_split(au_str,";\\s*")[[1]])
      tibble::tibble(au_clave=aus, afiliacion=trimws(inst))
    })
  } else if (str_detect(c1_raw,"\\([^\\)]+\\)")) {
    segs <- str_split(c1_raw,";")[[1]]
    purrr::map_dfr(segs, function(seg) {
      au_str <- str_extract(seg,"(?<=\\()[^\\)]+")
      inst   <- str_trim(str_remove(seg,"\\([^\\)]+\\)\\s*"))
      if (is.na(au_str)) return(tibble::tibble(au_clave="TODOS",afiliacion=trimws(seg)))
      aus <- trimws(str_split(au_str,";\\s*")[[1]])
      tibble::tibble(au_clave=aus, afiliacion=trimws(inst))
    })
  } else {
    tibble::tibble(au_clave="TODOS", afiliacion=trimws(c1_raw))
  }
}

# CORREGIDO: columna se llama "afiliacion" no "afil"
buscar_afil_autor <- function(apellido, afil_map) {
  if (is.null(afil_map)||nrow(afil_map)==0) return(NA_character_)
  idx_todos <- which(afil_map$au_clave=="TODOS")
  if (length(idx_todos)>0) return(afil_map$afiliacion[idx_todos[1]])
  ape6 <- substr(toupper(trimws(apellido)),1,6)
  idx  <- which(sapply(afil_map$au_clave,function(k) grepl(ape6,toupper(k),fixed=TRUE)))
  if (length(idx)>0) return(afil_map$afiliacion[idx[1]])
  return(NA_character_)
}

segregar_autores <- function(df, n_max=AU_MAX) {
  cat("=== SEGREGACION DE AUTORES ===\n")
  cat("  Articulos:", nrow(df), "| Max autores por art:", n_max, "\n\n")

  result_lista <- vector("list", nrow(df))

  for (i in seq_len(nrow(df))) {
    au_raw <- df$AU[i]
    if (is.na(au_raw)||trimws(au_raw)=="") next

    autores <- trimws(str_split(au_raw,";")[[1]])
    autores <- autores[nchar(autores)>1]
    n_au    <- length(autores)

    afil_map <- tryCatch(parsear_afiliaciones_c1(if("C1" %in% names(df)) df$C1[i] else NA_character_),
                         error=function(e)NULL)

    # Autor correspondiente desde campo RP (Scopus/WoS)
    rp_raw <- if ("RP" %in% names(df)) df$RP[i] else NA_character_
    corr_pos <- NA_integer_
    if (!is.na(rp_raw)&&!is.null(rp_raw)&&nchar(trimws(rp_raw))>2) {
      ape_rp <- toupper(trimws(str_extract(rp_raw,"^[^,(]+")))
      for (pos in seq_along(autores)) {
        ape_pos <- toupper(str_split(autores[pos],",\\s*")[[1]][1])
        if (!is.na(ape_rp)&&nchar(ape_rp)>=4&&grepl(substr(ape_rp,1,5),ape_pos,fixed=TRUE)) {
          corr_pos <- as.integer(pos); break
        }
      }
    }

    fila <- list(
      ART_UT     = if ("UT"        %in% names(df)) df$UT[i]        else NA,
      ART_TI     = df$TI[i],
      ART_DOI    = if ("DI"        %in% names(df)) df$DI[i]        else NA,
      ART_PMID   = if ("PM"        %in% names(df)) df$PM[i]        else NA,
      ART_PY     = if ("PY"        %in% names(df)) df$PY[i]        else NA,
      ART_SO     = if ("SO"        %in% names(df)) df$SO[i]        else NA,
      ART_BASE   = if ("SOURCE_DB" %in% names(df)) df$SOURCE_DB[i] else NA,
      ART_TC     = if ("TC_FINAL"  %in% names(df)) df$TC_FINAL[i]  else NA,
      ART_SJR_Q  = if ("SJR_Q"    %in% names(df)) df$SJR_Q[i]     else NA,
      N_AUTORES  = n_au,
      AU_CORR    = corr_pos
    )

    for (pos in seq_len(n_max)) {
      sfx <- as.character(pos)
      if (pos<=n_au) {
        au_str  <- autores[pos]
        partes  <- str_split(au_str,",\\s*")[[1]]
        ape     <- trimws(partes[1])
        inicial <- if (length(partes)>1) trimws(partes[2]) else NA_character_

        afil <- tryCatch(buscar_afil_autor(ape,afil_map), error=function(e)NA_character_)
        hosp <- tryCatch(detectar_hospital(afil),          error=function(e)NA_character_)
        uni  <- tryCatch(detectar_universidad(afil),       error=function(e)NA_character_)
        pais <- tryCatch(extraer_pais(afil),               error=function(e)NA_character_)

        fila[[paste0("AU",      sfx)]] <- au_str
        fila[[paste0("F",       sfx)]] <- afil
        fila[[paste0("HOSP",    sfx)]] <- hosp
        fila[[paste0("UNI",     sfx)]] <- uni
        fila[[paste0("PAIS",    sfx)]] <- pais
        fila[[paste0("G",       sfx)]] <- NA_character_  # vacio: llenar manualmente o con genderize
        fila[[paste0("INICIAL", sfx)]] <- inicial
      } else {
        for (pfx in c("AU","F","HOSP","UNI","PAIS","G","INICIAL"))
          fila[[paste0(pfx,sfx)]] <- NA_character_
      }
    }

    result_lista[[i]] <- as.data.frame(fila, stringsAsFactors=FALSE)

    if (i%%50==0||i==nrow(df))
      cat(sprintf("  [%d/%d] procesados\n",i,nrow(df)))
  }

  tabla <- bind_rows(result_lista)
  cat("\n  Generadas:", nrow(tabla), "filas\n")
  cat("  Con AU1:", sum(!is.na(tabla$AU1)),
      "| HOSP1:", sum(!is.na(tabla$HOSP1)),
      "| UNI1:", sum(!is.na(tabla$UNI1)),
      "| Corresponsal:", sum(!is.na(tabla$AU_CORR)),"\n\n")
  return(tabla)
}


# ------------------------------------------------------------------------------
# 2K — GENDERIZE.IO: INFERENCIA DE GENERO
# ------------------------------------------------------------------------------
# Si USAR_GENDERIZE=FALSE (defecto): columnas G quedan vacias para Google Sheets.
# Si USAR_GENDERIZE=TRUE:
#   - Intenta obtener nombre completo de OpenAlex (mejor precision)
#   - Si solo hay inicial (Embase/WoS), usa el apellido como aproximacion
#   - Plan gratuito: 100 nombres/dia | Plan basico (~$9/mes): 1000/dia
#   - Registrarse en: https://store.genderize.io

.cache_gender <- new.env(hash=TRUE, parent=emptyenv())

genderize_nombre <- function(nombre, pais=PAIS_GENDERIZE, api_key=GENDERIZE_KEY, pausa=0.35) {
  if (is.na(nombre)||nchar(trimws(nombre))<2) return(list(G=NA,prob=NA))
  n <- trimws(str_extract(nombre,"^[A-Za-z]+"))
  if (is.na(n)||nchar(n)<2) return(list(G=NA,prob=NA))
  clave <- paste0(n,"_",pais)
  if (exists(clave,envir=.cache_gender)) return(get(clave,envir=.cache_gender))
  params <- list(name=n)
  if (!is.null(pais)&&!is.na(pais)) params$country_id <- toupper(pais)
  if (nchar(api_key)>0) params$apikey <- api_key
  resp <- tryCatch(httr::GET("https://api.genderize.io",query=params,httr::timeout(8)),error=function(e)NULL)
  Sys.sleep(pausa)
  if (is.null(resp)||httr::status_code(resp)!=200) { assign(clave,list(G=NA,prob=NA),envir=.cache_gender); return(list(G=NA,prob=NA)) }
  d <- tryCatch(jsonlite::fromJSON(httr::content(resp,as="text",encoding="UTF-8")),error=function(e)NULL)
  if (is.null(d)||is.null(d$gender)) { assign(clave,list(G=NA,prob=NA),envir=.cache_gender); return(list(G=NA,prob=NA)) }
  G    <- if (!is.na(d$gender)) ifelse(d$gender=="male","M","F") else NA_character_
  prob <- as.numeric(d$probability)
  res  <- list(G=G,prob=prob)
  assign(clave,res,envir=.cache_gender)
  return(res)
}

aplicar_genderize <- function(tabla_autores, n_max=AU_MAX, usar=USAR_GENDERIZE,
                               prob_min=PROB_MIN_GENDER, pais=PAIS_GENDERIZE,
                               api_key=GENDERIZE_KEY, autores_oa=NULL) {
  if (!usar) {
    cat("  Genderize desactivado -> columnas G vacias (llenar manualmente).\n\n")
    return(tabla_autores)
  }
  cat("=== INFERENCIA DE GENERO (genderize.io) ===\n  Prob. minima:", prob_min, "\n\n")

  nombres_completos <- NULL
  if (!is.null(autores_oa)&&nrow(autores_oa)>0) {
    nombres_completos <- autores_oa %>%
      mutate(primer_nombre_oa=str_extract(nombre_oa,"^[^\\s]+")) %>%
      select(doi_oa, posicion, primer_nombre_oa)
  }

  for (pos in seq_len(n_max)) {
    sfx <- as.character(pos)
    if (!paste0("AU",sfx) %in% names(tabla_autores)) next
    cat("  Posicion AU",pos,"...\n",sep="")
    for (i in seq_len(nrow(tabla_autores))) {
      if (is.na(tabla_autores[[paste0("AU",sfx)]][i])) next
      primer_nombre <- NA_character_
      if (!is.null(nombres_completos)) {
        doi_i <- tabla_autores$ART_DOI[i]
        m <- nombres_completos %>% filter(doi_oa==doi_i & posicion==pos)
        if (nrow(m)>0) primer_nombre <- m$primer_nombre_oa[1]
      }
      if (is.na(primer_nombre)) {
        inicial_i <- tabla_autores[[paste0("INICIAL",sfx)]][i]
        primer_nombre <- if (!is.na(inicial_i)&&nchar(inicial_i)>1) inicial_i
                         else str_split(tabla_autores[[paste0("AU",sfx)]][i],",")[[1]][1]
      }
      gen <- genderize_nombre(primer_nombre, pais, api_key)
      if (!is.null(gen$G)&&!is.na(gen$G)&&!is.null(gen$prob)&&!is.na(gen$prob)&&gen$prob>=prob_min)
        tabla_autores[[paste0("G",sfx)]][i] <- gen$G
    }
  }
  g_vals <- unlist(lapply(seq_len(n_max),function(p) tabla_autores[[paste0("G",p)]]))
  cat("  M:",sum(g_vals=="M",na.rm=TRUE),"| F:",sum(g_vals=="F",na.rm=TRUE),
      "| Vacio:",sum(is.na(g_vals)),"\n\n")
  return(tabla_autores)
}


# ==============================================================================
# SECCION 3 — PIPELINE DE EJECUCION
# ==============================================================================

cat("\n==============================\n  INICIO DEL PIPELINE\n==============================\n\n")


# -- PASO 1: IMPORTAR LAS 5 BASES ---------------------------------------------
cat("--- PASO 1: Importando bases ---\n\n")

cat("Scopus...\n")
scopus <- convert2df(RUTA_SCOPUS, dbsource="scopus", format="bibtex")
scopus$SOURCE_DB <- "Scopus"
cat("  Registros Scopus:", nrow(scopus), "\n\n")

cat("Web of Science...\n")
wos <- convert2df(RUTA_WOS, dbsource="wos", format="bibtex")
wos$SOURCE_DB <- "WoS"
cat("  Registros WoS:", nrow(wos), "\n\n")

cat("SciELO (via WoS plaintext)...\n")
scielo <- convert2df(RUTA_SCIELO, dbsource="wos", format="plaintext")
scielo$SOURCE_DB <- "SciELO"
cat("  Registros SciELO:", nrow(scielo), "\n\n")

cat("PubMed...\n")
pubmed <- convert2df(RUTA_PUBMED, dbsource="pubmed", format="pubmed")
pubmed$SOURCE_DB <- "PubMed"
cat("  Registros PubMed:", nrow(pubmed), "\n\n")

cat("Embase (parser personalizado RIS)...\n")
embase <- importar_embase_ris(RUTA_EMBASE)


# -- PASO 2: FUSION + TRAZABILIDAD --------------------------------------------
cat("--- PASO 2: Fusion y deduplicacion ---\n\n")

fusion         <- fusionar_5_bases(scopus, wos, scielo, pubmed, embase)
base_fusionada <- fusion$base
inventario     <- fusion$inventario
trazabilidad   <- fusion$trazabilidad

saveRDS(base_fusionada, file.path(CARPETA_RESULTADOS,"snap_tras_fusion.rds"))
cat("  Snapshot guardado.\n\n")


# -- PASO 3: DOIs FALTANTES ---------------------------------------------------
if (USAR_CROSSREF) {
  cat("--- PASO 3: Recuperando DOIs faltantes via CrossRef ---\n\n")
  base_fusionada <- recuperar_dois_faltantes(base_fusionada, MI_EMAIL)
} else {
  cat("--- PASO 3: CrossRef omitido ---\n\n")
}


# -- PASO 4: CUARTILES SJR ----------------------------------------------------
if (USAR_SJR) {
  cat("--- PASO 4: Cuartiles SJR ---\n\n")
  sjr_data       <- cargar_sjr(RUTA_SJR_CSV)
  base_fusionada <- asignar_cuartil_sjr(base_fusionada, sjr_data)
} else {
  cat("--- PASO 4: SJR omitido ---\n\n")
}


# -- PASO 5: CITAS SCOPUS -----------------------------------------------------
if (USAR_API_SCOPUS) {
  cat("--- PASO 5: Citas via API Scopus ---\n\n")
  test_sc <- get_citas_scopus_doi("10.1016/j.joi.2017.08.007", SCOPUS_API_KEY, pausa=0.5)
  if (!is.na(test_sc)) {
    cat("  Conexion Scopus OK (prueba:",test_sc,"citas)\n")
    base_fusionada <- enriquecer_citas_scopus(base_fusionada, SCOPUS_API_KEY)
  } else {
    cat("  Scopus API no disponible (verificar IP institucional / VPN).\n")
    cat("  Usando TC original de las exportaciones.\n\n")
    base_fusionada$TC_Scopus     <- as.integer(base_fusionada$TC)
    base_fusionada$TC_Scopus_via <- "Exportacion_original"
  }
} else {
  cat("--- PASO 5: Scopus API omitido ---\n\n")
}


# -- PASO 6: CITAS WoS --------------------------------------------------------
if (USAR_API_WOS) {
  cat("--- PASO 6: Citas via API WoS ---\n\n")
  test_wos <- get_citas_wos_doi("10.1016/j.joi.2017.08.007", WOS_API_KEY)
  if (!is.null(test_wos)) {
    cat("  Conexion WoS OK\n")
    base_fusionada <- enriquecer_citas_wos(base_fusionada, WOS_API_KEY)
  } else {
    cat("  WoS API no disponible (verificar suscripcion institucional).\n\n")
  }
} else {
  cat("--- PASO 6: WoS API omitido ---\n\n")
}


# -- PASO 7: OPENALEX ---------------------------------------------------------
autores_oa_global <- NULL
if (USAR_OPENALEX) {
  cat("--- PASO 7: OpenAlex (citas + OA + nombres autores) ---\n\n")
  res_oa            <- enriquecer_openalex(base_fusionada, MI_EMAIL)
  base_fusionada    <- res_oa$df
  autores_oa_global <- res_oa$autores_oa  # nombres completos para genderize
} else {
  cat("--- PASO 7: OpenAlex omitido ---\n\n")
}


# -- PASO 8: SEMANTIC SCHOLAR -------------------------------------------------
if (USAR_SEMSCHOLAR) {
  cat("--- PASO 8: Semantic Scholar (citas influyentes + referencias) ---\n\n")
  test_ss <- get_ss_doi("10.1016/j.joi.2017.08.007", SS_API_KEY)
  if (!is.null(test_ss)) {
    cat("  Conexion Semantic Scholar OK\n")
    base_fusionada <- enriquecer_semantic_scholar(base_fusionada, SS_API_KEY)
  } else {
    cat("  Semantic Scholar no disponible (verificar key).\n\n")
  }
} else {
  cat("--- PASO 8: Semantic Scholar omitido ---\n\n")
}


# -- PASO 9: UNPAYWALL --------------------------------------------------------
if (USAR_UNPAYWALL) {
  cat("--- PASO 9: Open Access via Unpaywall ---\n\n")
  base_fusionada <- enriquecer_unpaywall(base_fusionada, MI_EMAIL)
} else {
  cat("--- PASO 9: Unpaywall omitido ---\n\n")
}


# -- PASO 10: CONSOLIDAR CITAS ------------------------------------------------
# Prioridad: Scopus API > WoS API > Semantic Scholar > OpenAlex > CrossRef > TC original
cat("--- PASO 10: Consolidando citas de todas las fuentes ---\n\n")

base_fusionada <- base_fusionada %>%
  mutate(
    TC_FINAL = case_when(
      !is.na(TC_Scopus) & TC_Scopus>=0  ~ as.integer(TC_Scopus),
      !is.na(WOS_citas) & WOS_citas>=0  ~ as.integer(WOS_citas),
      !is.na(SS_citas)  & SS_citas>=0   ~ as.integer(SS_citas),
      !is.na(OA_citas)  & OA_citas>=0   ~ as.integer(OA_citas),
      !is.na(CR_citas)  & CR_citas>=0   ~ as.integer(CR_citas),
      !is.na(TC)                         ~ as.integer(TC),
      TRUE ~ 0L
    ),
    TC_FUENTE = case_when(
      !is.na(TC_Scopus) & TC_Scopus>=0  ~ "Scopus_API",
      !is.na(WOS_citas) & WOS_citas>=0  ~ "WoS_API",
      !is.na(SS_citas)  & SS_citas>=0   ~ "Semantic_Scholar",
      !is.na(OA_citas)  & OA_citas>=0   ~ "OpenAlex",
      !is.na(CR_citas)  & CR_citas>=0   ~ "CrossRef",
      !is.na(TC)                         ~ "Exportacion",
      TRUE ~ "Sin_citas"
    ),
    TC_FECHA = format(Sys.Date(),"%Y-%m-%d")
  )

cat("  Fuente de citas por articulo:\n")
print(table(base_fusionada$TC_FUENTE, useNA="no"))
cat("\n")


# -- PASO 11: ANALISIS BIBLIOMETRICO DESCRIPTIVO ------------------------------
cat("--- PASO 11: Analisis bibliometrico descriptivo ---\n\n")

analisis_bib <- biblioAnalysis(base_fusionada, sep=";")
summary(analisis_bib, k=15, pause=FALSE)

cat("\n  TOP 10 MAS CITADOS:\n")
top10 <- base_fusionada %>%
  filter(!is.na(TC_FINAL)) %>%
  arrange(desc(TC_FINAL)) %>%
  slice_head(n=10) %>%
  select(TI,AU,PY,SO,TC_FINAL,TC_FUENTE,SJR_Q)
print(top10 %>% mutate(TI=str_trunc(TI,60)))


# -- PASO 12: SEGREGACION DE AUTORES ------------------------------------------
cat("\n--- PASO 12: Segregacion de autores ---\n\n")
tabla_autores <- segregar_autores(base_fusionada, n_max=AU_MAX)


# -- PASO 13: GENERO ----------------------------------------------------------
cat("--- PASO 13: Inferencia de genero ---\n\n")
tabla_autores <- aplicar_genderize(tabla_autores, n_max=AU_MAX,
                                    usar=USAR_GENDERIZE, prob_min=PROB_MIN_GENDER,
                                    pais=PAIS_GENDERIZE, api_key=GENDERIZE_KEY,
                                    autores_oa=autores_oa_global)


# -- PASO 14: EXPORTAR --------------------------------------------------------
cat("--- PASO 14: Exportando resultados ---\n\n")

# ---- Columnas de la base completa para el Excel ----
cols_base <- c(
  # Identificadores
  "TI","AU","AF","PY","SO","DI","DT","LA","SN","VL","IS","PP",
  # Citas de todas las fuentes
  "TC","TC_Scopus","WOS_citas","SS_citas","SS_citas_infl","OA_citas","CR_citas",
  "TC_FINAL","TC_FUENTE","TC_FECHA",
  # Calidad de la revista
  "SJR_Q","SJR_score","SJR_H","SJR_OA",
  # Open Access (todas las fuentes)
  "OA_es_oa","OA_url_pdf","OA_tipo","UPW_oa","UPW_tipo","UPW_url","SS_oa","SS_pdf_url",
  # Referencias (para co-citacion futura)
  "SS_refs_dois","SS_n_refs",
  # Contenido
  "AB","DE","ID","CR",
  # Afiliaciones e instituciones
  "C1","RP","AU_UN","AU1_UN",
  # Trazabilidad
  "SOURCE_DB","Bases_indexadoras","N_bases",
  # Identificadores externos
  "UT","PM","J9","JI","DB"
)
cols_base_ok <- intersect(cols_base, names(base_fusionada))

# ---- Columnas de segregacion (sin las INICIAL_n que son auxiliares) ----
cols_seg <- names(tabla_autores)[!grepl("^INICIAL",names(tabla_autores))]

# ============================================================
# HOJA UNIFICADA: base completa + segregacion en UNA sola hoja
# ============================================================
# Se une por ART_DOI y ART_TI para que cada articulo tenga
# en la misma fila: todos sus campos bibliometricos + AU1/F1/HOSP1...
# Asi tu equipo puede revisar y completar todo desde un solo Excel.

base_para_union <- base_fusionada %>%
  select(all_of(cols_base_ok)) %>%
  mutate(
    MERGE_DOI = toupper(trimws(DI)),
    MERGE_TI  = toupper(trimws(TI))
  )

tabla_para_union <- tabla_autores %>%
  select(all_of(cols_seg)) %>%
  mutate(
    MERGE_DOI = toupper(trimws(ART_DOI)),
    MERGE_TI  = toupper(trimws(ART_TI))
  ) %>%
  # Eliminar columnas que ya estan en base_para_union para no duplicar
  select(-any_of(c("ART_TI","ART_PY","ART_SO","ART_BASE","ART_TC","ART_SJR_Q")))

# Union primaria por DOI; complementaria por titulo para los sin DOI
hoja_unificada <- base_para_union %>%
  left_join(tabla_para_union %>% filter(MERGE_DOI!=""|!is.na(MERGE_DOI)),
            by="MERGE_DOI", suffix=c("","_seg")) %>%
  # Para registros sin DOI que no hayan hecho match, unir por titulo
  mutate(AU_CORR_check = if ("AU_CORR" %in% names(.)) AU_CORR else NA) %>%
  # Limpiar columnas auxiliares de merge
  select(-MERGE_DOI, -MERGE_TI, -any_of(c("MERGE_DOI.y","MERGE_TI.y","MERGE_DOI.x","MERGE_TI.x")))

# Crear libro Excel
wb <- createWorkbook()

# ---- HOJA 1: BASE UNIFICADA (la mas importante) ----
# Contiene TODOS los campos bibliometricos + AU1/F1/HOSP1/UNI1/G1/PAIS1...
# Es la hoja que tu equipo va a revisar y completar manualmente.
addWorksheet(wb, "BASE_UNIFICADA")
writeData(wb, "BASE_UNIFICADA", hoja_unificada)

# Estilo: congelar primera fila y primera columna para navegacion comoda
freezePane(wb, "BASE_UNIFICADA", firstRow=TRUE, firstCol=FALSE)

# ---- HOJA 2: Solo base bibliometrica (sin segregacion) ----
addWorksheet(wb, "Base_bibliometrica")
writeData(wb, "Base_bibliometrica", base_fusionada[, cols_base_ok])

# ---- HOJA 3: Solo segregacion de autores ----
addWorksheet(wb, "Segregacion_autores")
writeData(wb, "Segregacion_autores", tabla_autores[, cols_seg])
freezePane(wb, "Segregacion_autores", firstRow=TRUE, firstCol=FALSE)

# ---- HOJA 4: Trazabilidad ----
addWorksheet(wb, "Trazabilidad")
writeData(wb, "Trazabilidad", trazabilidad %>% rename(Titulo=TI_n))

# ---- HOJA 5: Cobertura por base ----
addWorksheet(wb, "Cobertura_bases")
writeData(wb, "Cobertura_bases",
          inventario %>% count(SOURCE_DB, name="N_bruto") %>%
            mutate(N_dedup=nrow(base_fusionada), Fecha=format(Sys.Date(),"%Y-%m-%d")))

# ---- HOJA 6: Top citados ----
addWorksheet(wb, "Top_citados")
writeData(wb, "Top_citados",
          base_fusionada %>%
            filter(!is.na(TC_FINAL)&TC_FINAL>0) %>%
            arrange(desc(TC_FINAL)) %>%
            slice_head(n=100) %>%
            select(any_of(c("TI","AU","PY","SO","DI","TC_FINAL","SS_citas_infl",
                             "TC_FUENTE","SJR_Q","OA_es_oa","SOURCE_DB","Bases_indexadoras"))))

# ---- HOJA 7: Hospitales (conteo de autorias) ----
addWorksheet(wb, "Hospitales")
hosp_conteo <- purrr::map_dfr(seq_len(AU_MAX), function(pos) {
  col <- paste0("HOSP",pos)
  if (!col %in% names(tabla_autores)) return(NULL)
  tabla_autores %>%
    filter(!is.na(.data[[col]])) %>%
    mutate(Posicion=paste0("AU",pos)) %>%
    select(Posicion, Hospital=.data[[col]])
}) %>% count(Hospital, Posicion, sort=TRUE)
writeData(wb, "Hospitales", hosp_conteo)

# ---- HOJA 8: Cuartiles SJR ----
if ("SJR_Q" %in% names(base_fusionada)) {
  addWorksheet(wb, "Cuartiles_SJR")
  writeData(wb, "Cuartiles_SJR",
            base_fusionada %>%
              count(SO, SJR_Q, SJR_score) %>%
              arrange(SJR_Q, desc(SJR_score)) %>%
              rename(Revista=SO, Cuartil=SJR_Q, SJR=SJR_score, N=n))
}

# ---- Guardar ----
archivo_xlsx <- file.path(CARPETA_RESULTADOS, "resultados_biblio_hospitales.xlsx")
saveWorkbook(wb, archivo_xlsx, overwrite=TRUE)
cat("  Excel guardado:", archivo_xlsx, "\n")

# RDS para recarga rapida
saveRDS(base_fusionada, file.path(CARPETA_RESULTADOS,"base_fusionada_final.rds"))
saveRDS(tabla_autores,  file.path(CARPETA_RESULTADOS,"tabla_autores.rds"))
cat("  RDS guardados en:", CARPETA_RESULTADOS, "\n\n")


# -- PASO 15: BASE PARA BIBLIOSHINY -------------------------------------------
cat("--- PASO 15: Preparando base para Biblioshiny ---\n\n")

cols_shiny <- c("AU","DE","ID","C1","CR","AB","affiliations","AR","EM","BO",
                "da","DI","GA","eissn","earlyaccessdate","BE","FU","FX","BN",
                "SN","JI","SO","LA","meeting","month","note","NR","PN","oa",
                "orcid.numbers","PP","PU","SC","researcherid.numbers","SE","TC",
                "TI","DT","UT","U2","VL","web.of.science.categories.",
                "web.of.science.index","PY","AF","RP","DB","J9","AB_raw",
                "TI_raw","DE_raw","AU_UN","AU1_UN","AU_UN_NR","SR_FULL","SR")
for (col in cols_shiny) if (!(col %in% names(base_fusionada))) base_fusionada[[col]] <- NA
base_fusionada$TC <- base_fusionada$TC_FINAL  # usar citas actualizadas

base_shiny <- base_fusionada[, intersect(cols_shiny, names(base_fusionada))]
openxlsx::write.xlsx(base_shiny, file.path(CARPETA_RESULTADOS,"base_para_biblioshiny.xlsx"))
cat("  Base Biblioshiny guardada.\n\n")


# -- RESUMEN FINAL ------------------------------------------------------------
cat("=================================================================\n")
cat("  PIPELINE COMPLETADO\n")
cat("=================================================================\n")
cat(sprintf("  Articulos unicos finales  : %d\n", nrow(base_fusionada)))
cat(sprintf("  Con cuartil SJR           : %d\n", sum(!is.na(base_fusionada$SJR_Q))))
cat(sprintf("  Con citas (TC_FINAL > 0)  : %d\n", sum(base_fusionada$TC_FINAL>0, na.rm=TRUE)))
cat(sprintf("  Open Access               : %d\n", sum(base_fusionada$OA_es_oa==TRUE, na.rm=TRUE)))
cat(sprintf("  Con referencias (SS)      : %d\n", sum(!is.na(base_fusionada$SS_refs_dois))))
cat(sprintf("  Filas tabla autores       : %d\n", nrow(tabla_autores)))
cat(sprintf("  HOSP1 detectado           : %d\n", sum(!is.na(tabla_autores$HOSP1))))
cat(sprintf("  UNI1 detectada            : %d\n", sum(!is.na(tabla_autores$UNI1))))
cat(sprintf("  AU_CORR (corresponsal)    : %d\n", sum(!is.na(tabla_autores$AU_CORR))))
cat("  Fecha:", format(Sys.time(),"%d/%m/%Y %H:%M"), "\n")
cat("-----------------------------------------------------------------\n")
cat("  ARCHIVOS GENERADOS:\n")
cat("  resultados_biblio_hospitales.xlsx  <- Excel principal\n")
cat("  base_fusionada_final.rds           <- recarga rapida en R\n")
cat("  tabla_autores.rds                  <- recarga rapida en R\n")
cat("  base_para_biblioshiny.xlsx         <- para Biblioshiny\n")
cat("-----------------------------------------------------------------\n")
cat("  HOJAS DEL EXCEL:\n")
cat("  1. BASE_UNIFICADA    <- BASE + SEGREGACION JUNTAS (revisar aqui)\n")
cat("  2. Base_bibliometrica  <- solo campos bibliometricos\n")
cat("  3. Segregacion_autores <- AU1/F1/HOSP1/UNI1/G1/PAIS1...\n")
cat("  4. Trazabilidad         <- en que bases aparece cada art\n")
cat("  5. Cobertura_bases      <- registros brutos por base\n")
cat("  6. Top_citados          <- 100 mas citados\n")
cat("  7. Hospitales           <- autorias por hospital\n")
cat("  8. Cuartiles_SJR        <- distribucion Q1-Q4\n")
cat("=================================================================\n\n")
cat("  Para lanzar Biblioshiny:\n")
cat("  biblioshiny()\n\n")
cat("  Para recargar la base sin repetir el pipeline:\n")
cat("  base_fusionada <- readRDS('resultados/base_fusionada_final.rds')\n")
cat("  tabla_autores  <- readRDS('resultados/tabla_autores.rds')\n\n")

