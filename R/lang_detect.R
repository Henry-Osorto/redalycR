lang_detect <- function(x, sep = ">>>") {

  stopifnot(is.data.frame(x))
  stopifnot(all(c("palabras", "resumen") %in% names(x)))

  data <- tibble::as_tibble(x) |>
    dplyr::transmute(id = dplyr::row_number(),
                     palabras = trimws(as.character(.data$palabras)),
                     resumen  = trimws(as.character(.data$resumen)))

  # ---- Keywords por posición ----
  kw_long <- data |>
    dplyr::select(id, palabras) |>
    tidyr::separate(palabras,
                    into = c("kw1", "kw2", "kw3", "kw4"),
                    sep = sep,
                    fill = "right",
                    extra = "merge") |>
    tidyr::pivot_longer(cols = dplyr::starts_with("kw"),
                        names_to = "pos",
                        values_to = "palabras") |>
    dplyr::mutate(pos = readr::parse_number(pos),
                  palabras = stringr::str_squish(palabras),
                  palabras = dplyr::na_if(palabras, "")) |>
    tidyr::drop_na(palabras)

  # ---- Resúmenes por posición y etiqueta explícita ----
  abs_long <- data |>
    dplyr::select(id, resumen) |>
    tidyr::separate(resumen,
                    into = c("ab1", "ab2", "ab3", "ab4"),
                    sep = sep,
                    fill = "right",
                    extra = "merge") |>
    tidyr::pivot_longer(cols = dplyr::starts_with("ab"),
                        names_to = "pos",
                        values_to = "resumen") |>
    dplyr::mutate(pos = readr::parse_number(pos),
                  resumen = stringr::str_squish(resumen),
                  resumen = dplyr::na_if(resumen, ""),
                  lang_explicit = stringr::str_match(resumen, "^\\s*(es|en|pt|fr)\\s*:")[, 2],
                  resumen = stringr::str_remove(resumen, "^\\s*(es|en|pt|fr)\\s*:\\s*"),
                  resumen = stringr::str_squish(resumen)) |>
    tidyr::drop_na(resumen)

  # ---- Unir por id + posición, no solo por id ----
  out <- kw_long |>
    dplyr::left_join(abs_long, by = c("id", "pos")) |>
    dplyr::mutate(lang = lang_explicit)

  # ---- Si no hay etiqueta explícita, detectar idioma sobre bloque completo ----
  if (any(is.na(out$lang))) {
    idx <- is.na(out$lang)

    text_for_lang <- paste(out$palabras[idx],
                           out$resumen[idx],
                           sep = " ")

    out$lang[idx] <- cld3::detect_language(text_for_lang)
  }

  # ---- Normalizar idiomas permitidos ----
  out <- out |>
    dplyr::mutate(lang = dplyr::case_when(lang %in% c("es", "en", "pt", "fr") ~ lang,
                                          TRUE ~ NA_character_)) |>
    tidyr::drop_na(lang) |>
    dplyr::select(id, palabras, lang, resumen)

  # ---- Evitar duplicados por artículo e idioma ----
  out <- out |>
    dplyr::group_by(id, lang) |>
    dplyr::slice(1) |>
    dplyr::ungroup()

  out
}
