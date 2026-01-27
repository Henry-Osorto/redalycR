#' Descarga todos los resultados paginando automáticamente
#'
#' @param query Texto de búsqueda.
#' @param page_size Registros por página.
#' @param max_pages Máximo de páginas a recorrer (Inf para todas).
#' @param progress Mostrar progreso (TRUE/FALSE).
#' @param ... Parámetros adicionales para redalyc_get_articles.
#'
#' @return tibble con todos los resultados concatenados.
#' @export
redalyc_get_all_articles <- function(query,
                                     page_size = 100,
                                     max_pages = Inf,
                                     progress = TRUE,
                                     ...) {
  
  first <- redalyc_get_articles(query = query, page = 1, page_size = page_size, ...)
  total <- first$totalResultados
  
  if (is.null(total) || is.na(total)) {
    stop("No se pudo leer totalResultados desde la respuesta.")
  }
  
  total_pages <- ceiling(total / page_size)
  total_pages <- min(total_pages, max_pages)
  
  if (progress) {
    cli::cli_inform(c("i" = "Total resultados: {total}; páginas: {total_pages} (page_size={page_size})"))
  }
  
  pages <- seq_len(total_pages)
  
  out <- purrr::map_dfr(pages, function(p) {
    if (progress) cli::cli_inform(c("i" = "Descargando página {p}/{total_pages}..."))
    res <- if (p == 1) first else redalyc_get_articles(query = query, page = p, page_size = page_size, ...)
    res$resultados
  })
  
  out
}