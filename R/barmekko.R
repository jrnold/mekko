# this shouldn't be exposed
positions <- function(width) {
  0.5 * (cumsum(width) + cumsum(c(0, width[-length(width)])))
}

#' Create a bar mekko plot.
#'
#' A smarter bar chart.
#'
#' @param data A data frame.
#' @param x A categorical variable defining the width categories.
#' @param y A numeric variable defining the bar height.
#' @param width A numeric variable defining the bar widths
#' @param values A boolean indicating whether to show value labels in bars
#' @return A bar mekko constructed with ggplot2.
#' @export
#' @example examples/barmekko.R
#' @importFrom ggplot2 ggplot geom_col aes
#' @importFrom rlang enquo UQ eval_tidy
barmekko <- function(data, x, y, width = 1, values = FALSE, fill = TRUE) {
  x <- enquo(x)
  y <- enquo(y)
  width <- enquo(width)
  if (fill) {
    fill <- x
  } else {
    fill <- quo(NULL)
  }
  pos <- eval_tidy(quo(positions(UQ(width))), data = data)
  xlab <- eval_tidy(x, data = data)
  ggplot(data) +
    geom_barmekko(aes(x = UQ(width), y = UQ(y), fill = UQ(fill))) +
    scale_x_discrete(labels = xlab, breaks = pos)
}
