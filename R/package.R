################################################################################
#' phyloseqExtra
#'
#' This package allows you to store, handle, and analyz high-throughput p
#' hylogenic sequencing data in different phylogenic level. The the core
#' component of this package includes two S4 classes.
#'
#' The SummarizedPhyloseq was built based on the phyloseq class from the
#' phyloseq package. It inherits from the phyloseq class. Not only it contains
#' all the slots in phyloseq, but also it has 7 new slots as the summarized otu
#' table on the level of from species to kingdom. Differential abundance test
#' can then be applied to the SummarizedPhyloseq on each phyloseq level, and
#' the results are stored in the SummarizedPhyloStats class.
#'
#' @import methods
#' @name phyloseqExtra-package
#' @author Chenghao Zhu \email{chhzhu@@ucdavis.edu}
#' @docType package
#' @keywords package
NA
################################################################################
