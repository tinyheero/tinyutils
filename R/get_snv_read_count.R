#' Get Nucleotide Read Counts
#' 
#' Wrapper function over Rsamtools::pileup to retrieve nucleotide read count 
#' values over a set of positions. This function serves more as a way to 
#' document how to use the pileup function.
#' 
#' @param bamfile Path to the input bam file.
#' @param scan.bam.params Rsamtools::ScanBamParam object which specifies 
#'   positions to lookup and additional read filtering. See the ScanBamParam 
#'   documentation for specific details.  
#' @param pileup.param Rsamtools::PileupParam object which specifies specific
#'   pileup parameters for filtering.
#' @return data.frame containing the nucleotide and read count information for each
#'   position specified.
#' @export
#' @examples
#' library("GenomicRanges")
#' library("Rsamtools")
#' 
#' bamfile <- system.file("extdata", "ex1.bam", package = "Rsamtools")
#' 
#' # Setup Lookup Positions 
#' positions.df <- data.frame(chr = c("seq1", "seq1", "seq1"),
#'                            start = c(50, 891, 1000),
#'                            stop = c(50, 891, 1000))
#' positions.gr <- makeGRangesFromDataFrame(positions.df)
#' scan.bam.param <- ScanBamParam(which = positions.gr)
#' 
#' # Setup Read Filters
#' pileup.param <- PileupParam(max_depth = 1000, min_mapq = 13, 
#'                             min_base_quality = 20, 
#'                             distinguish_strands = FALSE, 
#'                             min_nucleotide_depth = 0)
#'
#' get_snv_read_count(bamfile, scan.bam.param)
get_snv_read_count <- function(bamfile, scan.bam.param, pileup.param) {

  bam.file <- Rsamtools::BamFile(bamfile)

  if (missing(bamfile)) {
    stop("bamfile must be specified")
  }

  if (missing(scan.bam.param)) {
    stop("scan.bam.param must be specified")
  }

  if (missing(pileup.param)) {
    message(paste("Detected no pileup.param. Using default values from",
                  "Rsamtools::PileupParam()"))
    pileup.param <- Rsamtools::PileupParam()
  }

  pileup(bamfile, scanBamParam = scan.bam.param, pileupParam = pileup.param)
}
