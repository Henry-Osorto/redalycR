lang_detect <- function(x, sep = ">>>") {

  stopifnot(is.data.frame(x))
  stopifnot(all(c("palabras", "resumen") %in% names(x)))

  if (!requireNamespace("cld2", quietly = TRUE)) {
    stop("Package 'cld2' is required.", call. = FALSE)
  }

  if (!requireNamespace("cld3", quietly = TRUE)) {
    stop("Package 'cld3' is required.", call. = FALSE)
  }

  allowed_langs <- c("es", "en", "pt", "fr")

  first_text <- function(z) {
    z <- z[!is.na(z) & nzchar(stringr::str_squish(z))]
    if (length(z) == 0) NA_character_ else z[[1]]
  }

  safe_detect_cld2 <- function(txt) {
    tryCatch(cld2::detect_language(txt), error = function(e) NA_character_)
  }

  safe_detect_cld3 <- function(txt) {
    tryCatch(cld3::detect_language(txt), error = function(e) NA_character_)
  }

  detect_lang_block <- function(txt, field = c("keywords", "abstract")) {

    field <- match.arg(field)

    txt <- as.character(txt)
    txt <- stringr::str_squish(txt)

    if (is.na(txt) || txt == "") return(NA_character_)

    # 1. Detectar etiqueta explícita: es:, en:, pt:, fr:
    explicit <- stringr::str_match(
      stringr::str_to_lower(txt),
      "^\\s*(es|en|pt|fr)\\s*:"
    )[, 2]

    if (!is.na(explicit) && explicit %in% allowed_langs) {
      return(explicit)
    }

    # 2. Quitar etiqueta si existe
    txt_clean <- stringr::str_remove(
      txt,
      "^\\s*(es|en|pt|fr)\\s*:\\s*"
    )

    txt_clean <- stringr::str_squish(txt_clean)
    txt_clean <- gsub("\\d+", " ", txt_clean)
    txt_clean <- stringr::str_squish(txt_clean)

    if (nchar(txt_clean) < 8) return(NA_character_)

    # 3. Detección sobre bloque completo
    candidates <- c(
      safe_detect_cld3(txt_clean),
      safe_detect_cld2(txt_clean)
    )

    # 4. En keywords, agregar votos por tokens separados
    if (field == "keywords") {

      tokens <- unlist(
        stringr::str_split(txt_clean, "\\s*[,;/]\\s*"),
        use.names = FALSE
      )

      tokens <- stringr::str_squish(tokens)
      tokens <- tokens[nchar(tokens) >= 5]

      if (length(tokens) > 0) {
        tokens <- tokens[seq_len(min(length(tokens), 6))]

        candidates <- c(
          candidates,
          purrr::map_chr(tokens, safe_detect_cld3),
          purrr::map_chr(tokens, safe_detect_cld2)
        )
      }
    }

    candidates <- candidates[candidates %in% allowed_langs]

    if (length(candidates) == 0) return(NA_character_)

    votes <- sort(table(candidates), decreasing = TRUE)

    # 5. Si hay ganador único, usarlo
    if (length(votes) == 1 || votes[[1]] > votes[[2]]) {
      return(names(votes)[[1]])
    }

    # 6. Si hay empate, usar detección del bloque completo como desempate
    full_cld3 <- safe_detect_cld3(txt_clean)
    full_cld2 <- safe_detect_cld2(txt_clean)

    if (full_cld3 %in% names(votes[votes == max(votes)])) {
      return(full_cld3)
    }

    if (full_cld2 %in% names(votes[votes == max(votes)])) {
      return(full_cld2)
    }

    # 7. Si sigue ambiguo, no asignar idioma
    NA_character_
  }

  split_field <- function(data, col_name, value_name, field_type) {

    data |>
      dplyr::select(id, value = dplyr::all_of(col_name)) |>
      dplyr::mutate(value = as.character(.data$value),
                    value = stringr::str_split(.data$value, sep)) |>
      tidyr::unnest_longer(value, indices_to = "pos") |>
      dplyr::mutate(value = stringr::str_squish(.data$value),
                    value = dplyr::na_if(.data$value, ""),
                    lang = purrr::map_chr(.data$value,
                                          detect_lang_block,
                                          field = field_type),

                    value = stringr::str_remove(.data$value,
                                                "^\\s*(es|en|pt|fr)\\s*:\\s*" ),

                    value = stringr::str_squish(.data$value)) |>
      tidyr::drop_na(value, lang) |>
      dplyr::select(id, pos, lang, !!value_name := value)
  }

  data <- tibble::as_tibble(x) |>
    dplyr::transmute(id = dplyr::row_number(),
                     palabras = as.character(.data$palabras),
                     resumen  = as.character(.data$resumen) )

  # Keywords por separado
  kw_long <- split_field(data = data,
                         col_name = "palabras",
                         value_name = "palabras",
                         field_type = "keywords")

  # Resúmenes por separado
  abs_long <- split_field(data = data,
                          col_name = "resumen",
                          value_name = "resumen",
                          field_type = "abstract")

  # Si algún keyword quedó sin resumen por idioma,
  # no se fuerza emparejamiento por posición.
  # La unión principal se hace por id + lang.
  kw_clean <- kw_long |>
    dplyr::group_by(.data$id, .data$lang) |>
    dplyr::summarise(palabras = first_text(.data$palabras),
                     .groups = "drop")

  abs_clean <- abs_long |>
    dplyr::group_by(.data$id, .data$lang) |>
    dplyr::summarise(resumen = first_text(.data$resumen),
                     .groups = "drop")

  out <- dplyr::full_join(kw_clean,
                          abs_clean,
                          by = c("id", "lang")) |>
    dplyr::arrange(.data$id, .data$lang) |>
    dplyr::select(id, palabras, lang, resumen)

  out
}
