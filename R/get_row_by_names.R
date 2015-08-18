#' Get Row of Matrix by Rowname
#'
#' This function provides a pipe friendly way of retrieving rows of a matrix
#' by rownames.
#'
#' @param in.mat The input matrix
#' @param in.row.names The row names to subset by
#' @return A subset of the matrix
#' @export
#' @examples 
#' mat <- matrix(1:20, 5, 4, dimnames = list(LETTERS[1:5], NULL))
#' get_row_by_name(mat, c("A", "D", "E"))
get_row_by_names <- function(in.mat, in.row.names) {
  in.mat[in.row.names, ]
}
