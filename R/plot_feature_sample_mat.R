#' Plot a Feature-Sample Matrix
#'
#' Generates a ggplot2::geom_tile plot of features by sample.
#' It is able to deal with multiple types affecting the same sample.
#'
#' @param in.dt 3 column (feature, sampleID, type) data.table object
#' @param feature.order character vector indicating the order of the features 
#'   in the final plot on the y-axis
#' @param sample.order character vector indicating the order of the samples 
#'   in the final plot on the x-axis
#' @export
#' @examples
#' v1 <- c("RCOR1", "NCOR1", "LCOR1", "RCOR1", "RCOR1")
#' v2 <- c("sampleA", "sampleB", "sampleA", "sampleC", "sampleA")
#' v3 <- c("Deletion", "Deletion", "SNV", "Rearrangement", "SNV")
#' in.dt <- data.table::data.table(feature = v1, sampleID = v2, type = v3)
#' plot_feature_sample_mat(in.dt)
plot_feature_sample_mat <- function(in.dt, feature.order, sample.order) {
  
  if (is.null(feature.order)) {
    feature.order <- unique(in.dt[, feature])
  }
  if (is.null(sample.order)) {
    sample.id.order <- unique(in.dt[, sampleID])
  }

  in.dt <- in.dt[, feature := as.numeric(factor(feature, levels = feature.order)) ]
  in.dt <- in.dt[, sampleID := as.numeric(factor(sampleID, levels = sample.id.order)) ]
  in.dt <- in.dt[, shift := (1:(.N))/.N - 1/(2 * .N) - 1/2, 
                 by = list(sampleID, feature)]
  in.dt <- in.dt[, height := 1/.N, by = list(sampleID, feature)]

  p1 <- ggplot2::ggplot(in.dt, ggplot2::aes(x = sampleID, 
                                      y = feature + shift, 
                                      height = height,
                                      fill = type)) +
    ggplot2::geom_tile(color = "black", size = 1) +
    ggplot2::scale_y_discrete(limits = 1:3, labels = feature.order)

  p1
}
