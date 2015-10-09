#' Plot a Feature-Sample Matrix
#'
#' Generates a ggplot2::geom_tile plot of features by sample.
#' It is able to deal with multiple types affecting the same sample.
#'
#' @param in.dt A 3 column (feature, sampleID, type) data.table object
#' @param feature.order character vector indicating the order of the features 
#'   in the final plot on the y-axis
#' @param sample.id.order character vector indicating the order of the samples 
#'   in the final plot on the x-axis
#' @param fill.colors character vector indicating the colors of the different 
#'   "types". The names should be the types with the value being the color
#' @param type.display.mode Specify whether multiple or a single type can appear 
#'   in the same feature/sample cell
#' @param type.order Specify the "priority" of the types. This has an effect when
#'   type.display.mode is set to single. 
#' @export
#' @examples
#' v1 <- c("RCOR1", "NCOR1", "LCOR", "RCOR1", "RCOR1", "RCOR1", "RCOR1")
#' v2 <- c("sampleA", "sampleC", "sampleB", "sampleC", "sampleA", "sampleC", "sampleC")
#' v3 <- c("Deletion", "Deletion", "SNV", "Rearrangement", "SNV", "Rearrangement", "SNV")
#' feature.order <- c("RCOR1", "NCOR1", "LCOR")
#' sample.id.order <- c("sampleA", "sampleB", "sampleC")
#' in.dt <- data.table::data.table(feature = v1, sampleID = v2, type = v3)
#' fill.colors <- c("Deletion" = "Blue", "Rearrangement" = "Green", "SNV" = "Red")
#'
#' plot_feature_sample_mat(in.dt, feature.order, sample.id.order, 
#'   fill.colors = fill.colors)
#'
#' plot_feature_sample_mat(in.dt, feature.order, sample.id.order, fill.colors = fill.colors,
#'   type.display.mode = "single")
#'
#' plot_feature_sample_mat(in.dt, feature.order, sample.id.order, fill.colors = fill.colors,
#'   type.display.mode = "single", type.order = c("Rearrangement", "SNV", "Deletion"))
plot_feature_sample_mat <- function(in.dt, feature.order, sample.id.order, fill.colors,
                             type.display.mode = c("multiple", "single"), 
                             type.order) {

  type.display.mode <- match.arg(type.display.mode)

  # Copy so that it doesn't change the in.dt from the pass-in
  tmp.dt <- data.table::copy(in.dt)
  
  if (missing(feature.order)) {
    message("Detected no feature.order. Setting feature.order")
    feature.order <- unique(tmp.dt[, feature])
  } 

  if (length(unique(feature.order)) != length(feature.order)) {
    warning("There may be duplicates in your feature.order. This may cause issues")
  }
  
  feature.order <- rev(feature.order)

  if (missing(sample.id.order)) {
    message("Detected no sample.id.order. Setting sample.id.order")
    sample.id.order <- unique(tmp.dt[, sampleID])
  }

  if (missing(type.order)) {
    message("Detected no type.order. Setting type.order")
    type.order <- unique(tmp.dt[, type])
  }

  if (type.display.mode == "single") {
    tmp.df <- unique(tmp.dt)
    tmp.df <- dplyr::as_data_frame(tmp.df)
    tmp.df <- dplyr::mutate(tmp.df, 
                            type = factor(type, levels = rev(type.order)))
    tmp.df <- dplyr::group_by(tmp.df, sampleID, feature)
    tmp.df <- dplyr::arrange(tmp.df, type)
    tmp.df <- dplyr::top_n(tmp.df, n = 1)
    tmp.dt <- data.table(tmp.df)
  }

  tmp.dt <- tmp.dt[, feature := as.numeric(factor(feature, 
                                           levels = feature.order))]

  tmp.dt <- tmp.dt[, sampleID := factor(sampleID, 
                                 levels = sample.id.order)]

  tmp.dt <- tmp.dt[, shift := (1:(.N))/.N - 1/(2 * .N) - 1/2, 
                 by = list(sampleID, feature)]

  tmp.dt <- tmp.dt[, height := 1/.N, by = list(sampleID, feature)]

  p1 <- ggplot2::ggplot(tmp.dt, ggplot2::aes(x = sampleID, 
                                      y = feature + shift, 
                                      height = height,
                                      fill = type)) +
    ggplot2::geom_tile(color = "black", size = 1) +
    ggplot2::scale_y_discrete(limits = 1:length(feature.order), 
                              labels = feature.order) +
    ggplot2::ylab("Feature") +
    ggplot2::xlab("Sample ID")

  if (!missing(fill.colors)) {
    p1 <- p1 +
      ggplot2::scale_fill_manual(values = fill.colors)
  }
  p1
}
