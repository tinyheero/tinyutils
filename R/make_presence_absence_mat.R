#' Create a presence/asbence (PA) matrix 
#' 
#' This function given a list of vectors containing various elements 
#' (e.g. gene names) will create a matrix with the elements as rows and 
#' each vector as a column. TRUE/FALSE values wil indicate whether the 
#' element was found part of the vector
#' 
#' @param inList List of vectors containing elements. 
#' @return A matrix containing various vector elements as rows and the 
#'  names in the list as columns
#' @export
#' @examples
#' v1 <- c("EZH2", "B2M", "RCOR1", "CREBBP")
#' v2 <- c("LCOR", "NCOR1", "MLL2", "CREBBP", "CD58")
#' v3 <- c("TP53", "UNC80", "CREBBP")
#' inList <- list("groupA" = v1, "groupB" = v2, "groupC" = v3)
#' MakePAMatrix(inList)
MakePAMatrix <- function(inList) {
  geneList <- unique(unlist(inList))

  outMat <- matrix(NA, length(geneList), length(inList), dimnames = list(geneList, names(inList)))
  for (i in 1:ncol(outMat)) {
    curGenes <- inList[[i]]
    outMat[, i] <- rownames(outMat) %in% curGenes
  }
  outMat
}
