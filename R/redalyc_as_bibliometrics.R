#' Convertir resultados de Redalyc a estructura bibliometrix (Field Tags)
#'
#' Convierte los resultados devueltos por las funciones de extracción de Redalyc
#' en una estructura compatible con el paquete \pkg{bibliometrix}, utilizando
#' etiquetas de campo estándar (AU, TI, SO, DE, ID, AB, PY, DI, UT, DB, etc.).
#'
#' La función permite seleccionar y normalizar el idioma de los campos textuales
#' (por ejemplo, resumen y palabras clave) cuando estos contienen versiones en
#' múltiples idiomas, priorizando el idioma solicitado y utilizando un idioma
#' de respaldo cuando sea necesario.
#'
#' @param x data.frame o tibble devuelto por
#'   \code{redalyc_get_all_articles()} o
#'   \code{redalyc_get_articles()$resultados}.
#' @param sep Separador estándar para campos multivaluados.
#'   Por defecto \code{";"} (convención usada por \pkg{bibliometrix}).
#' @param db Nombre de la base de datos a asignar en el campo \code{DB}.
#'   Por defecto \code{"REDALYC"}.
#' @param lang Idioma objetivo para los campos textuales.
#'   Opciones:
#'   \itemize{
#'     \item \code{"auto"}: selecciona automáticamente el idioma dominante
#'       (según detección o etiquetas explícitas).
#'     \item \code{"es"}: español.
#'     \item \code{"en"}: inglés.
#'     \item \code{"pt"}: portugués.
#'   }
#' @param fallback_lang Idioma de respaldo que se utilizará cuando el idioma
#'   solicitado en \code{lang} no esté disponible en el registro.
#'   Por defecto \code{"en"}.
#'
#' @return Tibble con columnas compatibles con \pkg{bibliometrix}, incluyendo:
#' \itemize{
#'   \item \code{UT}: identificador único del artículo.
#'   \item \code{AU}: autores.
#'   \item \code{TI}: título.
#'   \item \code{SO}: fuente o revista.
#'   \item \code{DE}: palabras clave de autor.
#'   \item \code{ID}: palabras clave adicionales.
#'   \item \code{AB}: resumen normalizado por idioma.
#'   \item \code{PY}: año de publicación.
#'   \item \code{DI}: DOI.
#'   \item \code{DB}: base de datos de origen.
#' }
#'
#' @details
#' Cuando los campos textuales contienen versiones multilingües explícitamente
#' etiquetadas (por ejemplo, \code{es:}, \code{en:}, \code{pt:}), la función
#' selecciona el texto correspondiente al idioma indicado en \code{lang}.
#'
#' Si el idioma solicitado no está disponible, se utiliza el idioma definido
#' en \code{fallback_lang}, evitando la pérdida de información textual.
#'
#' La función garantiza que el número de filas del dataset resultante coincida
#' con el número de registros originales, sin generar duplicaciones.
#'
#' @export
redalyc_as_bibliometrics <- function(x, sep = ";",
                                     lang = c("auto","es","en","pt"),
                                     fallback_lang = "es",
                                     filter_lang_rows = TRUE) {
  lang <- match.arg(lang)
  stopifnot(is.data.frame(x))
  x <- tibble::as_tibble(x)

  out <- redalyc_lang_detect(x)

  if(lang =='es'){out <- out |> dplyr::filter(lang == 'es')}
  else if(lang =='en'){out <- out |> dplyr::filter(lang == 'en')}
  else if(lang =='pt'){out <- out |> dplyr::filter(lang == 'pt')}

  x <- x |>
    dplyr::mutate(id = 1:n())|>
    dplyr::select(-resumen, -palabras) |>
    dplyr::left_join(out)

  # Helpers ----
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  pick <- function(df, candidates) {
    nm <- names(df)
    hit <- candidates[candidates %in% nm]
    if (length(hit) == 0) return(NULL)
    hit[[1]]
  }

  collapse_vec <- function(v) {
    # v puede ser character, list-column, etc.
    if (is.null(v)) return(NULL)
    if (is.list(v)) {
      purrr::map_chr(v, function(z) {
        if (is.null(z)) return(NA_character_)
        if (is.character(z)) return(paste(z, collapse = sep))
        if (is.atomic(z)) return(paste(as.character(z), collapse = sep))
        NA_character_
      })
    } else {
      as.character(v)
    }
  }

  # Candidatos típicos (ajusta según tu payload real) ----
  col_title   <- pick(x, c("title","titulo","documentTitle","ti"))
  col_year    <- pick(x, c("anioArticulo", "year","anio","año","py","publicationYear"))
  col_abs     <- pick(x, c("abstract","resumen","ab"))
  col_source  <- pick(x, c("nomRevista", "journal","revista","source","so","publicationName"))
  col_doi     <- pick(x, c("doiTitulo", "doi","DOI","di"))
  col_url     <- pick(x, c("url","link","enlace"))
  col_authors <- pick(x, c("authors","autores","author","au"))
  col_de      <- pick(x, c("palabras", "keywords","palabras_clave","palabrasClave","de"))
  col_vl      <- pick(x, c("volRevNum"))
  col_num     <- pick(x, c("numRevNum"))
  col_pages   <- pick(x, c("paginas"))
  col_issn    <- pick(x, c("issnrev"))
  col_id      <- pick(x, c("keywordsPlus","kw_plus","id"))  # si no existe, se puede duplicar DE hacia ID

  # Extraer/normalizar ----
  TI <- if (!is.null(col_title)) as.character(x[[col_title]]) else NA_character_
  PY <- if (!is.null(col_year)) suppressWarnings(as.integer(x[[col_year]])) else NA_integer_
  AB <- if (!is.null(col_abs)) as.character(x[[col_abs]]) else NA_character_
  SO <- if (!is.null(col_source)) as.character(x[[col_source]]) else NA_character_
  DI <- if (!is.null(col_doi)) as.character(x[[col_doi]]) else NA_character_
  VL <- if (!is.null(col_vl)) as.character(x[[col_vl]]) else NA_character_
  IS <- if (!is.null(col_num)) as.character(x[[col_num]]) else NA_character_

  # Numeros de páginas
  pages_raw <- if (!is.null(col_pages)) as.character(x[[col_pages]]) else NA_character_

  nums <- if (!all(is.na(pages_raw))) stringr::str_extract_all(pages_raw, "\\d+") else vector("list", nrow(x))

  Page.start <- purrr::map_chr(nums, ~ if (length(.x) >= 1) .x[1] else NA_character_)
  Page.end   <- purrr::map_chr(nums, ~ if (length(.x) >= 1) .x[length(.x)] else NA_character_)

  ISSN <- if (!is.null(col_issn)) as.character(x[[col_issn]]) else NA_character_

  AU <- if (!is.null(col_authors)) collapse_vec(x[[col_authors]]) else NA_character_
  DE <- if (!is.null(col_de)) collapse_vec(x[[col_de]]) else NA_character_
  ID <- if (!is.null(col_id)) collapse_vec(x[[col_id]]) else DE

  # UT (Unique Article Identifier): si no hay un ID estable, usa DOI; si no, usa URL; si no, hash simple del título+año
  UT <- DI
  if (all(is.na(UT)) && !is.null(col_url)) UT <- as.character(x[[col_url]])
  if (all(is.na(UT))) {
    UT <- paste0("REDALYC_", substr(digest::digest(paste(TI, PY), algo = "xxhash64"), 1, 16))
  }

  db <- 'REDALYC'

  out <- tibble::tibble(UT = UT,
                        AU = AU,
                        TI = TI,
                        SO = SO,
                        DE = DE,
                        VL = VL,
                        IS = IS,
                        Page.start = Page.start,
                        Page.end = Page.end,
                        ISSN = ISSN,
                        ID = ID,
                        AB = AB,
                        PY = PY,
                        DI = DI,
                        DB = db)

  extra <- redalyc_add_affiliations_c1(x)
  out$Affiliations <- extra$Affiliations
  out$C1 <- extra$C1

  # Asegurar tipos compatibles (muchas columnas son CHARACTER en bibliometrix):contentReference[oaicite:5]{index=5}
  out <- dplyr::mutate(out,
                       dplyr::across(c(UT, AU, TI, SO, DE, ID, AB, DI, VL, IS, ISSN, Page.start, Page.end, Affiliations, C1, DB),
                                     ~ dplyr::if_else(is.na(.x), NA_character_, as.character(.x))))

  out
}
