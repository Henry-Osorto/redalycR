#' Construye la URL de búsqueda getArticles de Redalyc
#'
#' @param query Texto de búsqueda (puede incluir operadores AND/OR y comillas).
#' @param page Número de página (entero >= 1).
#' @param page_size Tamaño de página (recomendado 100).
#' @param collection Parámetro observado en endpoints (por defecto 1).
#' @param sort Parámetro observado (por defecto "default").
#' @param base_url Base del servicio.
#'
#' @return Una URL lista para consultar.
#' @export
redalyc_build_url <- function(query,
                              page = 1,
                              page_size = 100,
                              collection = 1,
                              sort = "default",
                              base_url = "https://www.redalyc.org/service/r2020/getArticles") {
  
  stopifnot(is.character(query), length(query) == 1, nchar(query) > 0)
  stopifnot(is.numeric(page), page >= 1)
  stopifnot(is.numeric(page_size), page_size >= 1)
  
  # La API suele requerir que ciertas partes vayan URL-encoded.
  # Usamos URLencode con reserved=TRUE para respetar caracteres reservados.
  q_enc <- utils::URLencode(query, reserved = TRUE)
  
  paste0(
    base_url, "/",
    q_enc, "/",
    as.integer(page), "/",
    as.integer(page_size), "/",
    as.integer(collection), "/",
    sort, "/"
  )
}