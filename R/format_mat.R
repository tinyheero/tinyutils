#' Format Matrix 
#'
#' This function provides the ability to format your matrix in various ways.
#' For instance, you can log transform, row normalize, and quantile normalize.
#'
#' @param mat The input matrix
#' @param log.transform Boolean to indicate whether the log transform the 
#'        matrix
#' @param log.base The base to use for the log transformation
#' @param correct.inf.flag Booelean to set whether 
#' @return A formatted matrix
#' @export
format_mat <- function(mat, log.transform = FALSE, log.base = 2,
                       norm.method = c("none", "quantile", "z", "max")) {

  norm.method <- match.arg(norm.method)

	# Save the row- and col- names as some normalization methods drop the names
	mat.row.names <- rownames(mat)
  mat.col.names <- colnames(mat)

	if (log.transform) {
		message('Log2 transforming')
    mat <- log(mat, log.base)
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

	} else if (norm.method == "z" ) {
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
