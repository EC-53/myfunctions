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
