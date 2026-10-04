#' Get a custom color palette
#'
#' @param n Number of colors needed.
#' @return A character vector of `n` hex colors.
#' @export
#' @examples
#' nice_palette(3)
nice_palette <- function(n = 5) {
  base <- c("#264653", "#2A9D8F", "#E9C46A", "#F4A261", "#E76F51")
  if (n <= length(base)) {
    base[seq_len(n)]
  } else {
    grDevices::colorRampPalette(base)(n)
  }
}


#' Histogram with a nicer look
#'
#' @param x A numeric vector.
#' @param bins Number of bins.
#' @param main Plot title.
#' @param xlab Label for the x axis. Defaults to the name of `x`.
#' @return A ggplot object.
#' @export
#' @examples
#' myhist(mtcars$mpg, main = "Miles per gallon")
myhist <- function(x, bins = 15, main = NULL, xlab = NULL) {
  if (is.null(xlab)) xlab <- deparse(substitute(x))
  ggplot2::ggplot(data.frame(x = x), ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(
      bins = bins,
      fill = nice_palette(2)[2],
      color = "white"
    ) +
    ggplot2::labs(title = main, x = xlab, y = "Count") +
    ggplot2::theme_minimal(base_size = 13)
}
