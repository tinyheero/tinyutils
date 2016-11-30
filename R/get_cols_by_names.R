#' Get Columns of Matrix by Column Name
#'
#' This function provides a pipe friendly way of retrieving columns of a matrix
#' by column anmes
#'
#' @param in.mat The input matrix
#' @param in.col.names The column names to subset by
#' @return A subset of the matrix
#' @export
#' @examples 
#' mat <- matrix(1:20, 5, 4, dimnames = list(NULL, LETTERS[1:5]))
#' get_cols_by_names(mat, c("A", "D", "E"))
get_cols_by_names <- function(in.mat, in.col.names) {
  in.mat[, in.col.names]
}
