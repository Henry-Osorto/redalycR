#' Exportar a BibTeX (artículos) desde data frame bibliometrix-like
#'
#' @param M data.frame bibliometrix-like (idealmente salida de redalyc_as_bibliometrix()).
#' @param path Ruta destino .bib
#' @param key_strategy "ut" o "author_year_title"
#' @export
redalyc_write_bib <- function(M, path, key_strategy = c("author_year_title", "ut")) {
  stopifnot(is.data.frame(M), is.character(path), length(path) == 1)
  key_strategy <- match.arg(key_strategy)
  
  M <- tibble::as_tibble(M)
  
  # helper: null-coalesce
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  # helper: escape BibTeX básico
  esc <- function(s) {
    s <- ifelse(is.na(s), "", as.character(s))
    s <- gsub("\\\\", "\\\\textbackslash{}", s)
    s <- gsub("\\{", "\\\\{", s)
    s <- gsub("\\}", "\\\\}", s)
    s
  }
  
  # helper: limpia para key
  key_clean <- function(s) {
    s <- tolower(s %||% "")
    s <- iconv(s, from = "", to = "ASCII//TRANSLIT") # quita tildes
    s <- gsub("[^a-z0-9]+", "", s)
    s
  }
  
  make_key <- function(i) {
    if (key_strategy == "ut" && "UT" %in% names(M)) {
      ut <- as.character(M$UT[i])
      if (!is.na(ut) && nzchar(ut)) return(gsub("[^A-Za-z0-9_:\\-]", "_", ut))
    }
    
    # author_year_title
    au1 <- ""
    if ("AU" %in% names(M)) {
      # AU está tipo "Apellido, Nombre; Apellido2, Nombre2"
      first <- strsplit(as.character(M$AU[i] %||% ""), ";", fixed = TRUE)[[1]][1]
      # toma primera palabra antes de coma como apellido
      au1 <- trimws(strsplit(first %||% "", ",", fixed = TRUE)[[1]][1])
    }
    py <- if ("PY" %in% names(M)) as.character(M$PY[i] %||% "") else ""
    ti <- if ("TI" %in% names(M)) as.character(M$TI[i] %||% "") else ""
    ti_short <- substr(key_clean(ti), 1, 24)
    
    k <- paste0(key_clean(au1), py, "_", ti_short)
    if (!nzchar(k) && "UT" %in% names(M)) k <- paste0("redalyc_", key_clean(M$UT[i]))
    if (!nzchar(k)) k <- paste0("redalyc_", i)
    k
  }
  
  bib_entry <- function(i) {
    k  <- make_key(i)
    
    TI <- esc(M$TI[i] %||% "")
    AU <- esc(M$AU[i] %||% "")
    SO <- esc(M$SO[i] %||% "")
    PY <- esc(M$PY[i] %||% "")
    
    VL <- if ("VL" %in% names(M)) esc(M$VL[i] %||% "") else ""
    IS <- if ("IS" %in% names(M)) esc(M$IS[i] %||% "") else ""
    ISSN <- if ("ISSN" %in% names(M)) esc(M$ISSN[i] %||% "") else ""
    DI <- if ("DI" %in% names(M)) esc(M$DI[i] %||% "") else ""
    
    p1 <- if ("Page.start" %in% names(M)) esc(M$Page.start[i] %||% "") else ""
    p2 <- if ("Page.end" %in% names(M)) esc(M$Page.end[i] %||% "") else ""
    PAGES <- ""
    if (nzchar(p1) && nzchar(p2)) PAGES <- paste0(p1, "--", p2)
    if (nzchar(p1) && !nzchar(p2)) PAGES <- p1
    
    fields <- c(
      sprintf("  title = {%s}", TI),
      if (nzchar(AU)) sprintf("  author = {%s}", AU) else NULL,
      if (nzchar(SO)) sprintf("  journal = {%s}", SO) else NULL,
      if (nzchar(PY)) sprintf("  year = {%s}", PY) else NULL,
      if (nzchar(VL)) sprintf("  volume = {%s}", VL) else NULL,
      if (nzchar(IS)) sprintf("  number = {%s}", IS) else NULL,
      if (nzchar(PAGES)) sprintf("  pages = {%s}", PAGES) else NULL,
      if (nzchar(ISSN)) sprintf("  issn = {%s}", ISSN) else NULL,
      if (nzchar(DI)) sprintf("  doi = {%s}", DI) else NULL
    )
    
    paste0("@article{", k, ",\n", paste(fields, collapse = ",\n"), "\n}\n")
  }
  
  lines <- purrr::map_chr(seq_len(nrow(M)), bib_entry)
  writeLines(lines, con = path, useBytes = TRUE)
  invisible(path)
}
