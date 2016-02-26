#' Format Matrix
#'
#' This function provides the ability to format your matrix in various ways.
#' For instance, you can log transform, row normalize, and quantile normalize.
#' It is best to use the log.transform and norm.method separately (i.e. make to
#' calls of format_mat)
#'
#' @param mat The input matrix.
#' @param log.transform Boolean to indicate whether the log transform the
#'        matrix.
#' @param log.base The base to use for the log transformation.
#' @param correct.inf.flag Booelean to set whether.
#' @param min.val Minimum value of the matrix. Any values in the matrix below
#'   this value will be set to this value. This is useful if trying to log
#'   transform a matrix. For instance, if log2 transforming then any values
#'   less than 1 will produce negative values. Setting min.val = 1 will ensure
#'   that any values less than 1 be log transformed to 0.
#' @return A formatted matrix.
#' @export
format_mat <- function(mat, log.transform = FALSE, log.base = 2,
                       norm.method = c("none", "quantile", "z", "max"),
                       min.val) {

  norm.method <- match.arg(norm.method)

  # Save the row- and col- names as some normalization methods drop the names
  mat.row.names <- rownames(mat)
  mat.col.names <- colnames(mat)

  if (!missing(min.val)) {
    message(paste("Setting minimum value of matrix to be", min.val))
    mat[mat < min.val] <- min.val
  }

  if (log.transform) {
    message(paste("Log", log.base, "transforming"))
    mat <- log(mat, log.base)

    if (any(mat < 0)) {
      warning("Values < 0 found in matrix. Please ensure this is what you want.")
    }
  }

  if (norm.method == "quantile") {
    if (!requireNamespace("preprocessCore", quietly = TRUE)) {
      stop("preprocessCore needed for quantile normalization.
           Please install first.",
           call. = FALSE)
    }
    message('Quantile normalizing')
    mat <- preprocessCore::normalize.quantiles(mat)
    rownames(mat) <- mat.row.names
    colnames(mat) <- mat.col.names

  } else if (norm.method == "z") {
    if (!requireNamespace("som", quietly = TRUE)) {
      stop("som needed for Z-score normalization. Please install first.",
           call. = FALSE)
    }
    message('Normalizing by Z-scores')
    mat <- som::normalize(mat)
    colnames(mat) <- mat.col.names

  } else if (norm.method == "max") {
    message('Normalizing by maximum value')
    maxVals <- apply(mat, 1, max)
    mat <- mat/maxVals
  }

  mat
}
