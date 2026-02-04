#' Split keyword strings into individual terms
#'
#' Splits a character vector of keyword fields into lists of keyword tokens using
#' a regular-expression separator. Designed to handle mixed separators commonly
#' found in metadata exports (commas, semicolons, slashes).
#'
#' @param x Character vector with raw keyword fields.
#' @param sep_regex Regular expression used as a separator. Default splits on
#'   commas, semicolons, and slashes with optional surrounding whitespace.
#'
#' @return A list of character vectors, one per element of \code{x}.
#'
#' @examples
#' split_keywords("capital humano; educación / desarrollo", "\\s*[,;/]\\s*")
#'
#' @importFrom stringr str_replace_all str_squish
#'
#' @keywords internal
split_keywords <- function(x, sep_regex = "\\s*[,;/]\\s*") {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "[\r\n\t]+", " ")
  x <- stringr::str_squish(x)
  stringr::str_split(x, sep_regex)
}
