#' Keyword frequency plot (DE field)
#'
#' Computes keyword frequencies from a bibliometrix-like data frame (e.g., output
#' from \code{redalyc_as_bibliometrix()}), produces a horizontal bar chart for
#' the top \code{k} keywords, and optionally saves the plot and a full frequency
#' table to disk.
#'
#' @param M A data.frame containing at least an identifier column and a keyword
#'   column (by default \code{ID} and \code{DE}).
#' @param id_col Name of the identifier column. Default \code{"ID"}.
#' @param kw_col Name of the keyword column. Default \code{"DE"}.
#' @param sep_regex Regular expression used to split keyword strings into tokens.
#'   Default splits on commas, semicolons, and slashes.
#' @param k Number of top keywords to display in the plot.
#' @param color Fill color for bars.
#' @param stop_words Optional character vector of terms to exclude (e.g., country
#'   names such as \code{"MÉXICO"} or \code{"COLOMBIA"}). Matching is performed
#'   in uppercase after trimming.
#' @param save Logical; if \code{TRUE}, saves plot files and an Excel workbook
#'   to \code{~/Documents/output_redalycR}.
#' @param width,height Plot dimensions passed to \code{ggplot2::ggsave()}.
#' @param prefix Prefix used to build output file names when \code{save = TRUE}.
#' @param ext Character vector of plot extensions to save (e.g., \code{c("png","pdf","svg")}).
#'
#' @return A list with:
#' \describe{
#'   \item{freq_all}{data.frame with all keywords and their frequencies (no top-k filtering).}
#'   \item{top}{data.frame with the top \code{k} keywords used in the plot.}
#'   \item{plot}{ggplot object.}
#' }
#'
#' @details
#' The function relies on \code{clean_keyword()} to normalize tokens and remove
#' common noise sources such as standalone \code{"JEL"} and isolated JEL codes
#' (e.g., \code{C67}, \code{J21}, \code{O14}). This helps prevent misleading
#' frequencies and improves downstream co-occurrence analyses.
#'
#' When \code{save = TRUE}, the function:
#' \itemize{
#'   \item creates (if needed) \code{~/Documents/output_redalycR};
#'   \item saves one plot per extension in \code{ext} via \code{ggplot2::ggsave()};
#'   \item writes an Excel file with the full frequency table using \pkg{writexl}
#'   (no Java dependency).
#' }
#'
#' SVG output: \code{ggsave()} will use the best available SVG device; if the
#' \pkg{svglite} package is installed, it is typically preferred. Users do not
#' need to attach \pkg{svglite} with \code{library()} for \code{ggsave()} to work.
#'
#' @examples
#' df <- data.frame(
#'   ID = c(1,1,2,2),
#'   DE = c("Capital humano, educación", "desarrollo económico.", "JEL; C67; empleo", "capital humano"),
#'   stringsAsFactors = FALSE
#' )
#' res <- frequency_keywords(df, k = 10, stop_words = c("EMPLEO"), save = FALSE)
#' res$plot
#' head(res$freq_all)
#'
#' @export
#'
#' @importFrom dplyr select all_of mutate filter count slice_head if_else na_if
#' @importFrom tidyr unnest_longer
#' @importFrom ggplot2 ggplot aes geom_col coord_flip labs theme_minimal theme element_blank margin ggsave
#' @importFrom stats reorder
frequency_keywords <- function(M,
                               id_col = "ID",
                               kw_col = "DE",
                               sep_regex = "\\s*[,;/]\\s*",
                               k = 15,
                               color = "#032B47",
                               stop_words = NULL,
                               save = FALSE,
                               width = 8,
                               height = 5,
                               prefix = "keywords",
                               ext = c("png", "pdf", "svg")) {

  stopifnot(is.data.frame(M))
  stopifnot(id_col %in% names(M), kw_col %in% names(M))

  # Normalizar stop_words (en MAYÚSCULAS para compararlas con KW)
  if (!is.null(stop_words)) {
    stop_words <- stringr::str_to_upper(stringr::str_squish(as.character(stop_words)))
    stop_words <- stop_words[stop_words != ""]
  }

  # Construcción de frecuencias (TODAS, sin cortar por k)
  freq_all <- M |>
    dplyr::select(ID = dplyr::all_of(id_col),
                  DE = dplyr::all_of(kw_col)) |>
    dplyr::mutate(DE = as.character(DE)) |>
    dplyr::filter(!is.na(DE), stringr::str_squish(DE) != "") |>
    dplyr::mutate(DE_list = split_keywords(DE, sep_regex = sep_regex)) |>
    tidyr::unnest_longer(DE_list, values_to = "KW") |>
    dplyr::mutate(KW = clean_keyword(KW)) |>
    dplyr::filter(!is.na(KW)) |>
    { if (!is.null(stop_words)) dplyr::filter(., !KW %in% stop_words) else . } |>
    dplyr::count(KW, sort = TRUE)

  # Top-k para graficar
  top <- freq_all |>
    dplyr::slice_head(n = k)

  g <- top |>
    ggplot2::ggplot(ggplot2::aes(x = stats::reorder(KW, n), y = n)) +
    ggplot2::geom_col(fill = color) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "Keywords", y = "Frecuencia") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(10, 20, 10, 10)
    )

  # Guardado (gráficos + Excel)
  if (isTRUE(save)) {

    # Si quiere Excel, necesitamos writexl (Suggests)
    .redalycR_need("writexl", "to save Excel output")

    # Si pidió svg explícitamente, recomendamos svglite (Suggests)

    if ("svg" %in% ext && !requireNamespace("svglite", quietly = TRUE)) {
      warning(
        "Package 'svglite' is not installed. SVG output may use a fallback device.\n",
        "Install it with install.packages('svglite') for better SVG output.",
        call. = FALSE
      )
    }

    out_dir <- file.path(path.expand("~"), "Documents", "output_redalycR")
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

    base <- file.path(out_dir, sprintf("%s_%s_top%02d", prefix, kw_col, k))

    # Gráficos (ggplot2 es Imports)
    for (e in ext) {
      ggplot2::ggsave(
        filename = paste0(base, ".", e),
        plot = g,
        width = width,
        height = height
      )
    }

    # Excel con TODO (sin filtrar por k)
    writexl::write_xlsx(
      list(keywords = freq_all),
      path = paste0(base, "_ALL.xlsx")
    )
  }

  list(freq_all = freq_all, top = top, plot = g)
}
