#' Get the Legend for ggplot
#'
#' Retrieve the legend of a ggplot so that it can be used as a plot itself. 
#' This is useful in situations where you are arrange multiple ggplots and want
#' to share a common legend.
#'
#' @author \url{http://www.sthda.com/english/wiki/ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page-r-software-and-data-visualization}
#' @param in.ggplot Input ggplot with legend
#' @return ggplot legend
#' @export
get_legend <- function(in.ggplot) {
  tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(in.ggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  tmp$grobs[[leg]]
}
