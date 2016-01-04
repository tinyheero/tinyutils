#' Plot a Feature-Sample Matrix
#'
#' Generates a ggplot2::geom_tile plot of features by sample.
#' It is able to deal with multiple types affecting the same sample.
#'
#' @param in.df A 3 column (feature, sampleID, type) data.frame object
#' @param feature.order character vector indicating the order of the features 
#'   in the final plot on the y-axis. If not set, then function will set it 
#'   automatically
#' @param sample.id.order character vector indicating the order of the samples 
#'   in the final plot on the x-axis. If not set, then function will set it 
#'   automatically
#' @param fill.colors character vector indicating the colors of the different 
#'   "types". The names should be the types with the value being the color
#' @param type.display.mode Specify whether multiple or a single feature type 
#'   can appear in the same feature/sample cell
#' @param type.order Specify the "priority" of the feature types. This only 
#'   has an effect when type.display.mode is set to single
#' @param tile.col Border color of each cell. If not yet, no border color is 
#'   used
#' @param rotate.x.labels Rotate the x-axes labels by a certain degree
#' @param missing.fill.col Color of the cell that has missing values
#' @export
#' @examples
#' v1 <- c("RCOR1", "NCOR1", "LCOR", "RCOR1", "RCOR1", "RCOR1", "RCOR1")
#' v2 <- c("sampleA", "sampleC", "sampleB", "sampleC", "sampleA", "sampleC", "sampleC")
#' v3 <- c("Deletion", "Deletion", "SNV", "Rearrangement", "SNV", "Rearrangement", "SNV")
#' feature.order <- c("RCOR1", "NCOR1", "LCOR")
#' sample.id.order <- c("sampleA", "sampleB", "sampleC")
#' in.df <- dplyr::data_frame(feature = v1, sampleID = v2, type = v3)
#' fill.colors <- c("Deletion" = "Blue", "Rearrangement" = "Green", "SNV" = "Red")
#' 
#' plot_feature_sample_mat(in.df)
#' 
#' # With black tile color
#' plot_feature_sample_mat(in.df, tile.col = "black")
#' 
#' # Fill in missing values with a lightgrey color
#' plot_feature_sample_mat(in.df, tile.col = "black", 
#'   missing.fill.col = "lightgrey")
#' 
#' # Rotate x-axes labels by 90 degrees
#' plot_feature_sample_mat(in.df, rotate.x.labels = 90)
#' 
#' # Specify order of features, samples, and colors
#' plot_feature_sample_mat(in.df, feature.order, sample.id.order, 
#'   fill.colors = fill.colors)
#' 
#' # Specify each cell can only have one "feature type"
#' plot_feature_sample_mat(in.df, feature.order, sample.id.order, fill.colors = fill.colors,
#'   type.display.mode = "single")
#' 
#' # Specify the specific priority of the "feature type" for cells with
#' # multiple features
#' plot_feature_sample_mat(in.df, feature.order, sample.id.order, fill.colors = fill.colors,
#'   type.display.mode = "single", type.order = c("Rearrangement", "SNV", "Deletion"))
plot_feature_sample_mat <- function(in.df, feature.order, sample.id.order, fill.colors,
                             type.display.mode = c("multiple", "single"), 
                             type.order, tile.col = NA, rotate.x.labels, 
                             missing.fill.col) {

  .Deprecated("plot_cofeature_mat", "cofeatureR", 
              paste("This function has been moved to the cofeatureR package and",
                    "renamed to plot_cofeature_mat."))

  if (!missing(missing.fill.col)) {
    message("Detected missing.fill.col parameter")
    in.df.grid <- expand.grid(feature = unique(in.df[["feature"]]),
                              sampleID = unique(in.df[["sampleID"]]),
                              stringsAsFactors = FALSE)

    in.df <- dplyr::right_join(in.df, in.df.grid)
  }

  type.display.mode <- match.arg(type.display.mode)

  # arg parameter cannot be a numeric vector
  if (missing(rotate.x.labels)) {
    rotate.x.labels <- 0
  } else {
    if (!rotate.x.labels %in% c(45, 90)) {
      stop("rotate.x.labels must be either 45 or 90")
    }
  }

  # Copy so that it doesn't change the in.df from the pass-in
  in.dt <- data.table::data.table(in.df)
  tmp.dt <- data.table::copy(in.dt)
  
  if (missing(feature.order)) {
    message("Detected no feature.order. Setting feature.order")
    feature.order <- unique(tmp.dt[["feature"]])
  } 

  if (length(unique(feature.order)) != length(feature.order)) {
    warning("There may be duplicates in your feature.order. This may cause issues. ")
  }
  
  feature.order <- rev(feature.order)

  if (missing(sample.id.order)) {
    message("Detected no sample.id.order. Setting sample.id.order")
    sample.id.order <- unique(tmp.dt[["sampleID"]])
  } else {
    missing.samples <- setdiff(unique(tmp.dt[["sampleID"]]), sample.id.order)
    if (length(missing.samples) > 0) {
      warning(paste("sampleID in in.df not found in sample.id.order:", 
                    paste(missing.samples, collapse = ", ")))
    }
  }

  if (missing(type.order)) {
    message("Detected no type.order. Setting type.order")
    type.order <- unique(tmp.dt[["type"]])
  }

  if (type.display.mode == "single") {
    tmp.df <- unique(tmp.dt)
    tmp.df <- dplyr::as_data_frame(tmp.df)
    tmp.df <- dplyr::mutate(tmp.df, 
                            type = factor(type, levels = rev(type.order)))
    tmp.df <- dplyr::group_by(tmp.df, sampleID, feature)
    tmp.df <- dplyr::arrange(tmp.df, type)
    tmp.df <- dplyr::top_n(tmp.df, n = 1)
    tmp.dt <- tmp.df
  }

  tmp.df <- data.table::data.table(tmp.dt)
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
    ggplot2::scale_y_discrete(limits = 1:length(feature.order), 
                              labels = feature.order) +
    ggplot2::ylab("Feature") +
    ggplot2::xlab("Sample ID")

  if (!missing(fill.colors)) {
    p1 <- p1 +
      ggplot2::scale_fill_manual(values = fill.colors)
  }

  if (missing(missing.fill.col)) {
    p1 <- p1 +
      ggplot2::geom_tile(color = tile.col, size = 1)
  } else {

    # Plot two geom_tile. 1 for data present and 1 for data missing
    filter.crit.1 <- lazyeval::interp(~ !is.na(type), .values = list(type = as.name("type")))
    filter.crit.2 <- lazyeval::interp(~ is.na(type), .values = list(type = as.name("type")))

    p1 <- p1 +
      ggplot2::geom_tile(data = dplyr::filter_(tmp.dt, filter.crit.1), 
                         color = tile.col, size = 1) +
      ggplot2::geom_tile(data = dplyr::filter_(tmp.dt, filter.crit.2), 
                         fill = missing.fill.col, color = tile.col, size = 1)
  }

  if (rotate.x.labels == 90) {
    p1 <- p1 +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, 
                                                         hjust = 1, 
                                                         vjust = 0.5))
  } else if (rotate.x.labels == 45) {
    p1 <- p1 +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, 
                                                         hjust = 1, 
                                                         vjust = 1))
  }
  p1
}
