#' Save an Excel workbook (optional dependency)
#'
#' Writes an .xlsx file using \pkg{writexl}. This is an optional dependency
#' (Suggests). If not installed, a clear error is thrown.
#'
#' @param data Named list of data frames to write as sheets.
#' @param filename File name (with .xlsx), saved inside \code{redalycR_output_dir()}.
#' @param save Logical; if \code{FALSE}, does not write files and returns \code{NULL}.
#'
#' @return The saved file path (or \code{NULL} if \code{save=FALSE}).
#'
#' @export
redalycR_save_xlsx <- function(data, filename, save = FALSE) {
  if (!isTRUE(save)) return(NULL)

  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop(
      "To save Excel output, the package 'writexl' is required.\n",
      "Please install it with install.packages('writexl').",
      call. = FALSE
    )
  }

  out_dir <- redalycR_output_dir(create = TRUE)
  path <- file.path(out_dir, filename)

  writexl::write_xlsx(data, path = path)
  path
}
