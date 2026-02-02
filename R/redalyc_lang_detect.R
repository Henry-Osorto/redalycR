redalyc_lang_detect <- function(x, sep = ">>>") {


  col <- c('palabras', 'resumen')
  data <- data.frame(x[, col], stringsAsFactors = FALSE)

  long <- data |>
    dplyr::select(-resumen) |>
    dplyr::mutate(id = dplyr::row_number()) |>
    tidyr::separate(palabras, into = c("lang1", "lang2", "lang3"), sep = sep, fill = "right") |>
    tidyr::pivot_longer(cols = -id, names_to = "cols", values_to = "text") |>
    tidyr::drop_na(text) |>
    dplyr::mutate(text = trimws(text),
                  text = dplyr::na_if(text, ""),
                  text.clean = gsub("\\d+", "", text),
                  text.clean = trimws(text.clean) ) |>
    tidyr::separate(text.clean, into = c("t1", "t2", "t3"), sep = ",", fill = "right") |>
    dplyr::mutate(lang.cld2.1 = cld2::detect_language(t1), #detectar idioma de tres keywords con cld2 y cld3
                  lang.cld2.2 = cld2::detect_language(t2),
                  lang.cld2.3 = cld2::detect_language(t3),
                  lang.cld3.1 = cld3::detect_language(t1),
                  lang.cld3.2 = cld3::detect_language(t2),
                  lang.cld3.3 = cld3::detect_language(t3),
                  es = rowSums(dplyr::across(lang.cld2.1:lang.cld3.3, ~ .x == "es"), na.rm = TRUE), # Identificar veces que se detectó el idioma
                  en = rowSums(dplyr::across(lang.cld2.1:lang.cld3.3, ~ .x == "en"), na.rm = TRUE),
                  pt = rowSums(dplyr::across(lang.cld2.1:lang.cld3.3, ~ .x == "pt"), na.rm = TRUE),
                  lang = dplyr::case_when( es >= en & es >= pt ~ "es", # votar por el idioma detectado
                                           en >= es & en >= pt ~ "en",
                                           pt >= es & pt >= en ~ "pt", TRUE ~ "en")) |>
    dplyr::select(id, palabras=text, lang) |>
    tidyr::drop_na(palabras)



  # Detectar idioma en el resumen (es:, en:, pt:)
  long.res <- data |>
    dplyr::select(-palabras) |>
    dplyr::mutate(id = dplyr::row_number()) |>
    tidyr::separate(resumen, into = c("lang1", "lang2", "lang3"), sep = sep, fill = "right") |>
    tidyr::pivot_longer(cols = -id, names_to = "cols", values_to = "text") |>
    tidyr::drop_na(text) |>
    dplyr::mutate(text = stringr::str_squish(text),   # normaliza espacios
                  lang = stringr::str_to_lower(stringr::str_sub(text, 1, 2)),# extrae 'es','en','pt'
                  text = stringr::str_remove(text, "^\\s*[a-z]{2}\\s*:\\s*"), # quita "es: " / "en:" / "pt:"
                  text = stringr::str_squish(text) ) |>
    dplyr::select(id, resumen=text, lang)


  out <- long |>
    dplyr::left_join(long.res)


}
