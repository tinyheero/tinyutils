#' Get the Default ggplot2 Color Palette
#' 
#' \code{gg_color_hue} returns the default ggplot color palette for a given
#' number of colors
#'
#' @author John Colby \url{http://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette}
#
#' @param n Number of Coolors
#' @export
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}
