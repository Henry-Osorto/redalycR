#' Construye Affiliations y C1 estilo Scopus/bibliometrix desde Redalyc
#'
#' @param df data.frame/tibble con: autores, institucionAutores,
#'           institucionRevista, nomInstitucionRev
#' @param authors_delim separador de autores (en tu caso coma)
#' @param inst_delim separador de ids de institución del autor (en tu caso espacio)
#' @param aff_sep separador final entre afiliaciones/autores (bibliometrix usa ";")
#' @param upper logical: convertir a mayúsculas
#'
#' @return df con dos columnas nuevas: Affiliations y C1
redalyc_add_affiliations_c1 <- function(df,
                                        authors_delim = ",",
                                        inst_delim = " ",
                                        aff_sep = ";",
                                        upper = TRUE) {
  stopifnot(is.data.frame(df))

  suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
    library(stringr)
  })

  req <- c("autores", "institucionAutores", "institucionRevista", "nomInstitucionRev")
  miss <- setdiff(req, names(df))
  if (length(miss) > 0) {
    stop("Faltan columnas requeridas: ", paste(miss, collapse = ", "))
  }

  # id de fila para recomponer al final sin perder orden
  df0 <- df %>%
    mutate(.rid = row_number())

  # tabla de revista: institucionRevista -> nomInstitucionRev
  revista <- df0 %>%
    select(institucionRevista, nomInstitucionRev) %>%
    distinct() %>%
    mutate(institucionRevista = suppressWarnings(as.numeric(institucionRevista)),
           nomInstitucionRev  = as.character(nomInstitucionRev),
           nomInstitucionRev  = if (upper) str_to_upper(nomInstitucionRev) else nomInstitucionRev,
           nomInstitucionRev  = str_squish(nomInstitucionRev)) %>%
    arrange(institucionRevista)

  # instituciones de autores por documento (alineadas por posición)
  inst_long <- df0 %>%
    select(.rid, institucionAutores) %>%
    mutate(institucionAutores = as.character(institucionAutores)) %>%
    # separa "14805 14889" -> dos filas
    separate_longer_delim(institucionAutores, delim = inst_delim) %>%
    mutate(institucionAutores = str_squish(institucionAutores),
           institucionAutores = na_if(institucionAutores, ""),
           institucionAutores_num = suppressWarnings(as.numeric(institucionAutores))) %>%
    drop_na(institucionAutores) %>%
    group_by(.rid) %>%
    mutate(.pos = row_number()) %>%
    ungroup() %>%
    left_join(revista,
              by = c("institucionAutores_num" = "institucionRevista")) %>%
    mutate(# si no existe nombre, usa el id numérico (o el texto original)
           aff = if_else(is.na(nomInstitucionRev) | nomInstitucionRev == "",
                         as.character(institucionAutores),
                         nomInstitucionRev ),
           aff = str_squish(aff)) %>%
    select(.rid, .pos, aff)

  # autores por documento (alineados por posición)
  au_long <- df0 %>%
    select(.rid, autores) %>%
    mutate(autores = as.character(autores)) %>%
    separate_longer_delim(autores, delim = authors_delim) %>%
    mutate(autor = str_squish(autores),
           autor = na_if(autor, ""),
           autor = if (upper) str_to_upper(autor) else autor) %>%
    drop_na(autor) %>%
    group_by(.rid) %>%
    mutate(.pos = row_number()) %>%
    ungroup() %>%
    select(.rid, .pos, autor)

  # C1: autor + afiliación (si no hay afiliación, queda solo autor o "autor, <NA>" según prefieras)
  c1_long <- au_long %>%
    left_join(inst_long, by = c(".rid", ".pos")) %>%
    mutate(C1_item = case_when(is.na(aff) | aff == "" ~ autor, # si no hay afiliación, deja solo autor
                           TRUE ~ paste0(autor, ", ", aff))    )

  # Affiliations: lista única de afiliaciones por documento
  aff_by_doc <- inst_long %>%
    group_by(.rid) %>%
    summarise(Affiliations = paste(unique(aff), collapse = paste0(aff_sep, " ")),
              .groups = "drop")

  # C1: concatenación por documento
  c1_by_doc <- c1_long %>%
    group_by(.rid) %>%
    summarise(C1 = paste(C1_item, collapse = paste0(aff_sep, " ")),
              .groups = "drop")

  # devolver df original + nuevas columnas
  df0 %>%
    left_join(aff_by_doc, by = ".rid") %>%
    left_join(c1_by_doc,  by = ".rid") %>%
    select(-.rid)
}
