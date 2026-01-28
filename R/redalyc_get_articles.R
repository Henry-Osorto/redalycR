#' Obtiene metadatos de artículos desde Redalyc (getArticles)
#'
#' @param query Texto de búsqueda.
#' @param page Página a consultar.
#' @param page_size Registros por página.
#' @param ... Parámetros adicionales pasados a redalyc_build_url (collection, sort, base_url).
#'
#' @return Lista con totalResultados y resultados (tibble).
#' @export
redalyc_get_articles <- function(query, page = 1, page_size = 100, ...) {

  url <- redalyc_build_url(query = query, page = page, page_size = page_size, ...)

  # Request robusto
  req <- httr2::request(url) |>
    httr2::req_user_agent("redalycR (R package; contact: your-email)") |>
    httr2::req_timeout(60)

  resp <- httr2::req_perform(req)

  # Manejo de errores HTTP
  if (httr2::resp_status(resp) >= 400) {
    msg <- httr2::resp_body_string(resp)
    stop(sprintf("Error HTTP %s al consultar Redalyc: %s", httr2::resp_status(resp), msg))
  }

  txt <- httr2::resp_body_string(resp)

  # Parseo JSON
  obj <- jsonlite::fromJSON(txt, flatten = TRUE)

  total <- obj[["totalResultados"]]
  resultados <- obj[["resultados"]]

  # Asegurar tibble aunque venga vacío
  if (is.null(resultados)) {
    resultados <- tibble::tibble()
  } else {
    resultados <- tibble::as_tibble(resultados)
  }

  list(
    totalResultados = total,
    page = page,
    page_size = page_size,
    resultados = resultados
  )
}
