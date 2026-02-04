#' Save plots with a consistent redalycR convention
#'
#' Saves a plot object to \code{redalycR_output_dir()} using a consistent naming
#' convention and multiple extensions.
#'
#' @param plot A plot object compatible with \code{ggplot2::ggsave()}.
#' @param filename_base Base filename (without extension), e.g. \code{"keywords_top15"}.
#' @param ext Character vector of extensions, e.g. \code{c("png","pdf","svg")}.
#' @param width,height Dimensions passed to \code{ggplot2::ggsave()}.
#' @param save Logical; if \code{FALSE}, does not write files and returns \code{character(0)}.
#'
#' @return A character vector with the saved file paths (invisibly if \code{save=FALSE}).
#'
#' @examples
#' # res <- redalycR_save_plot(ggplot2::qplot(1:3, 1:3), "demo_plot", save = FALSE)
#'
#' @export
#'
#' @importFrom ggplot2 ggsave
redalycR_save_plot <- function(plot,
                               filename_base,
                               ext = c("png", "pdf", "svg"),
                               width = 8,
                               height = 5,
                               save = FALSE) {
  if (!isTRUE(save)) return(character(0))

  out_dir <- redalycR_output_dir(create = TRUE)

  # SVG: prefer svglite if installed (Suggests). Don't fail if missing.
  if ("svg" %in% ext && !requireNamespace("svglite", quietly = TRUE)) {
    warning(
      "Package 'svglite' is not installed. SVG output may use a fallback device.\n",
      "Install it with install.packages('svglite') for better SVG output.",
      call. = FALSE
    )
  }

  paths <- character(0)
  for (e in ext) {
    path <- file.path(out_dir, paste0(filename_base, ".", e))
    ggplot2::ggsave(
      filename = path,
      plot = plot,
      width = width,
      height = height
    )
    paths <- c(paths, path)
  }

  paths
}
