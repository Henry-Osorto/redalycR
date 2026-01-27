`%||%` <- function(a, b) if (!is.null(a)) a else b

# Convierte cualquier cosa (factor/list) a character vector
redalyc_as_character <- function(x) {
  if (is.null(x)) return(character())
  if (is.factor(x)) return(as.character(x))
  if (is.list(x)) {
    return(purrr::map_chr(x, ~{
      if (is.null(.x)) return(NA_character_)
      if (is.character(.x)) return(paste(.x, collapse = ";"))
      if (is.atomic(.x)) return(paste(as.character(.x), collapse = ";"))
      NA_character_
    }))
  }
  as.character(x)
}

# Separar "es>>>en"
redalyc_split_lang <- function(x, lang = c("es","en","both"), sep = ">>>") {
  lang <- match.arg(lang)
  x <- redalyc_as_character(x)
  
  parts <- stringr::str_split_fixed(x, fixed(sep), n = 2)
  es <- stringr::str_trim(parts[,1])
  en <- stringr::str_trim(parts[,2])
  en[en == ""] <- NA_character_
  
  if (lang == "es") return(es)
  if (lang == "en") return(en)
  
  # both
  ifelse(is.na(en) | !nzchar(en), es, paste(es, en, sep = " ; "))
}

# Limpieza base (para texto libre: TI/AB)
redalyc_clean_text <- function(x,
                               lang = c("es","en"),
                               remove_numbers = TRUE,
                               remove_punct = TRUE,
                               tolower = TRUE,
                               strip_ws = TRUE,
                               remove_stopwords = TRUE,
                               extra_stopwords = character()) {
  lang <- match.arg(lang)
  x <- redalyc_as_character(x)
  x[is.na(x)] <- ""
  
  if (tolower) x <- stringr::str_to_lower(x)
  if (remove_numbers) x <- stringr::str_replace_all(x, "[0-9]+", " ")
  if (remove_punct)   x <- stringr::str_replace_all(x, "[[:punct:]]+", " ")
  if (strip_ws)      x <- stringr::str_squish(x)
  
  if (remove_stopwords) {
    sw <- switch(
      lang,
      es = stopwords::stopwords("es", source = "snowball"),
      en = stopwords::stopwords("en", source = "snowball")
    )
    sw <- unique(c(stringr::str_to_lower(sw), stringr::str_to_lower(extra_stopwords)))
    
    # remover stopwords tokenizando y reconstruyendo
    toks <- stringr::str_split(x, "\\s+")
    toks <- lapply(toks, function(tk) tk[!(tk %in% sw) & nzchar(tk)])
    x <- vapply(toks, paste, collapse = " ", FUN.VALUE = character(1))
    x <- stringr::str_squish(x)
  }
  
  x
}

# Tokenización por frases (keywords separadas por coma/; etc.)
redalyc_tokenize_phrases <- function(x, sep = ",", tolower = TRUE, trim = TRUE) {
  x <- redalyc_as_character(x)
  x[is.na(x)] <- ""
  if (tolower) x <- stringr::str_to_lower(x)
  
  out <- strsplit(x, split = sep, fixed = TRUE)
  out <- lapply(out, function(v) {
    if (trim) v <- trimws(v)
    v <- v[nzchar(v)]
    unique(v)
  })
  out
}

# Tokenización por palabras (títulos/resúmenes)
redalyc_tokenize_words <- function(x) {
  x <- redalyc_as_character(x)
  x[is.na(x)] <- ""
  toks <- stringr::str_split(x, "\\s+")
  toks <- lapply(toks, function(v) unique(v[nzchar(v)]))
  toks
}

# Parseo de sinónimos estilo bibliometrix:
# synonyms = c("human capital;capital humano;capital de humano", "sme;smE;MIPYMES")
# => mapea cada sinónimo a la 1ra etiqueta (canonical)
redalyc_synonym_map <- function(synonyms = NULL) {
  if (is.null(synonyms) || length(synonyms) == 0) return(NULL)
  
  # cada elemento "canon;sin1;sin2"
  pieces <- strsplit(synonyms, ";", fixed = TRUE)
  map <- list()
  for (p in pieces) {
    p <- trimws(p)
    p <- p[nzchar(p)]
    if (length(p) < 2) next
    canon <- stringr::str_to_lower(p[1])
    sins  <- stringr::str_to_lower(p)
    for (s in sins) map[[s]] <- canon
  }
  unlist(map, use.names = TRUE)
}

redalyc_apply_synonyms <- function(tokens, syn_map) {
  if (is.null(syn_map)) return(tokens)
  lapply(tokens, function(v) {
    v2 <- stringr::str_to_lower(v)
    v2 <- ifelse(v2 %in% names(syn_map), syn_map[v2], v2)
    unique(as.character(v2))
  })
}
