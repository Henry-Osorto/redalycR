#' Get redalycR output directory
#'
#' Returns the directory used by redalycR to save plots and tables. By default,
#' it uses \code{~/Documents/output_redalycR}. Users can override this path via
#' \code{options(redalycR.output_dir = "...")}.
#'
#' @param create Logical; if \code{TRUE}, the directory is created if it does not exist.
#'
#' @return A character string with the output directory path.
#'
#' @examples
#' redalycR_output_dir()
#'
#' @export
redalycR_output_dir <- function(create = FALSE) {
  out_dir <- getOption("redalycR.output_dir")
  if (is.null(out_dir) || !nzchar(out_dir)) {
    out_dir <- file.path(path.expand("~"), "Documents", "output_redalycR")
  }
  out_dir <- path.expand(out_dir)

  if (isTRUE(create) && !dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  out_dir
}
