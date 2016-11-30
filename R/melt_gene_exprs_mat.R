#' Melt Gene Expression Matrix
#'
#' \code{melt_gene_exprs_mat} melts the input gene expression matrix to 
#' produce a tidy data.frame
#'
#' @param exprs.mat Gene expression matrix with genes as rows and samples as 
#'   columns.
#' @return data.frame of gene_name, sample_id and gene_exprs.
#' @export
melt_gene_exprs_mat <- function(exprs.mat) {
  message("Melt gene expression matrix")
  exprs.df <- exprs.mat %>%
    reshape2::melt(value.name = "gene_exprs") %>%
    dplyr::rename_(.dots = c("gene_name" = "Var1", "sample_id" = "Var2"))

  dplyr::tbl_df(exprs.df)
}
