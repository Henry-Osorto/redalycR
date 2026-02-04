#' Clean a single keyword string
#'
#' Normalizes and cleans a vector of keyword terms (e.g., \code{DE} field) to
#' reduce noise in frequency and co-occurrence analyses. The function:
#' \itemize{
#'   \item squishes whitespace and replaces line breaks with spaces;
#'   \item removes edge quotes (single/double) and trailing punctuation;
#'   \item converts to uppercase;
#'   \item removes standalone \code{"JEL"} and isolated JEL classification codes
#'   (e.g., \code{C67}, \code{J21}, \code{O14}, \code{O1});
#'   \item removes tokens that are only numbers (e.g., \code{122}, \code{014}) or
#'   only punctuation (e.g., \code{"..."});
#'   \item converts empty strings to \code{NA}.
#' }
#'
#' @param x Character vector of keywords.
#'
#' @return A character vector with cleaned keywords; invalid/noise tokens are set
#'   to \code{NA}.
#'
#' @details
#' This helper is designed for bibliometric workflows where keyword fields may
#' contain inconsistent punctuation, line breaks, quotation marks, and JEL codes.
#' It aims to avoid destructive regex patterns that might remove valid leading
#' letters from words (e.g., turning \code{"DESARROLLO"} into \code{"ESARROLLO"}).
#'
#' @examples
#' clean_keyword(c(" economía.", "\"Capital humano\"", "JEL", "C67", "122", "..."))
#'
#' @importFrom stringr str_replace_all str_squish str_to_upper str_detect
#' @importFrom dplyr if_else na_if
#'
#' @keywords internal
clean_keyword <- function(x) {
  x <- as.character(x)

  # Normalizar espacios y saltos de línea internos
  x <- stringr::str_replace_all(x, "[\r\n\t]+", " ")
  x <- stringr::str_squish(x)

  # Quitar comillas/apóstrofes SOLO en bordes
  x <- stringr::str_replace_all(x, "^['\"]+|['\"]+$", "")

  # Quitar puntuación al final (economía. -> economía)
  x <- stringr::str_replace_all(x, "[[:punct:]]+$", "")
  x <- stringr::str_squish(x)

  # Pasar a mayúsculas
  x <- stringr::str_to_upper(x)

  # Quitar la palabra JEL como keyword
  x <- dplyr::if_else(x %in% c("JEL"), NA_character_, x)

  # Quitar códigos JEL aislados (C67, J21, O14, O1, etc.)
  x <- dplyr::if_else(stringr::str_detect(x, "^[A-Z]{1,2}\\d{1,2}$"),
                      NA_character_, x)

  # Quitar tokens solo números (122, 138, 01, 014...)
  x <- dplyr::if_else(stringr::str_detect(x, "^\\d+$"), NA_character_, x)

  # Quitar tokens solo símbolos/puntos (., .., ....)
  x <- dplyr::if_else(stringr::str_detect(x, "^[[:punct:][:space:]]+$"),
                      NA_character_, x)

  # Vacíos -> NA
  x <- dplyr::na_if(x, "")

  x
}
