#' @export
#' @rdname geom_barmekko
#' @importFrom ggplot2 layer GeomCol
geom_barmekko <- function(mapping = NULL, data = NULL,
                     position = "identity",
                     x = 1,
                     ...,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = "identity",
    geom = GeomBarMekko,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      x = x,
      ...
    )
  )
}

#' GeomBarMekko
#'
#' @export
#' @importFrom ggplot2 GeomRect resolution ggproto_parent
#' @importFrom rlang %||%
ggplot2::ggproto("GeomBarMekko", GeomRect,
   required_aes = c("x", "y"),

   # These aes columns are created by setup_data(). They need to be listed here so
   # that GeomRect$handle_na() properly removes any bars that fall outside the defined
   # limits, not just those for which x and y are outside the limits
   non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),


   setup_data = function(data, params) {
     data$x <- data$x %||% params$x

     xboundaries <- cumsum(c(0, data$x))

     data$ymin <- pmin(data$y, 0)
     data$ymax <- pmax(data$y, 0)
     data$xmin <- xboundaries[-1]
     data$xmax <- xboundaries[-length(xboundaries)]
     data$x <- (data$xmin + data$xmax) * 0.5

     data
   },

   draw_panel = function(self, data, panel_params, coord) {
     # Hack to ensure that width is detected as a parameter
     ggproto_parent(GeomRect, self)$draw_panel(data, panel_params, coord)
   }
) -> GeomBarMekko
