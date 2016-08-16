#' Get Triangle of the Correlation Matrix
#'
#' Gets a triangle (lower or upper) of a correlation matrix.
#' 
#' @param cor.mat Correlation matrix.
#' @param tri.type Retrieve values below or above the matrix. If 'none' is 
#'   specified, then the same input matrix is returned.
#' @return matrix with values above or below the diagonal depending on the 
#'   tri.type.
get_lower_upper_tri <- function(cor.mat, tri.type = c("none", "lower", 
                                                      "upper")) {

  tri.type <- match.arg(tri.type)
  
  if (tri.type == "upper") {
    cor.mat[upper.tri(cor.mat)] <- NA
  } else if (tri.type == "lower") {
    cor.mat[lower.tri(cor.mat)]<- NA
  }

  cor.mat
}

