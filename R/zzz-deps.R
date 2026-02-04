#' @keywords internal
.redalycR_need <- function(pkg, purpose) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      sprintf("Package '%s' is required %s.", pkg, purpose),
      "\nPlease install it with install.packages('", pkg, "').",
      call. = FALSE
    )
  }
  invisible(TRUE)
}
