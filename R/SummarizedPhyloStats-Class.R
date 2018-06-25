################################################################################
setClassUnion("dataframeOrNULL", c("data.frame", "NULL"))
################################################################################
#' @name SummarizedPhyloStats-class
#' @aliases SummarizedPhyloStats-class
#' @author Chenghao Zhu
#' @title S4 class SummarizedPhyloStats
#'
#' @description
#' A S4 object to store hypothesis test statistical results of phylogenic
#' sequencing experiment data, at different phylogenic level
#'
#' @details
#' This class has 8 slots and each stores the hypothesis test statistical
#' results of a designated phylogenic level, from otu to kingdom.There are
#' currently two statistical packages supported, limma and DESeq2. There are
#' 2 construction methods \code{\link{spy_to_deseq2}} and
#' \code{\link{spy_to_limma}}. The purpose of this class is to allow users to
#' conduct hypothesis test using DESeq2 or limma at all phylogenic levels and
#' view the results quickly.
#'
#' \describe{
#'     This class has 8 slots and each slot is a data.frame with 5 variables as
#' below:
#'     \item{baseMean}{The mean value of the baseline group. This corresponds
#'     to the AveExpr from the limma result or the baseMean
#'     from the DESeq2 result}
#'     \item{logFC}{Log transformed fold change from baseline. This corresponds
#'     to the logFC from the limma result or the log2FoldChange from the DESeq2
#'     result}
#'     \item{stat}{The statistic value. This corresponds to the t from the limma
#'     result or the stat from the DESeq2 result}
#'     \item{pvalue}{The raw p-value. This corresponds to the P.Value from the
#'     limma result or the pvalue from the DESeq2 result}
#'     \item{padj}{The adjusted p-value. This corresponds to the adj.P.Value
#'     from the limma result or the padj from the DESeq2 result}
#' }
#'
#' @slot kingdom_table Summarized statistic result at kingdom level.
#' @slot phylum_table Summarized statistic result at phylum level.
#' @slot class_table Summarized statistic result at class level.
#' @slot order_table Summarized statistic result at order level.
#' @slot family_table Summarized statistic result at family level.
#' @slot genus_table Summarized statistic result at genus level.
#' @slot species_table Summarized statistic result at species level.
#' @slot otu_table Summarized statistic result at otu level.
#'
#' @seealso
#' \code{\link{spy_to_deseq2}}
#' \code{\link{spy_to_limma}}
#' \code{\link{summarizeFromPhyloseq}}
#' \code{\link{SummarizedPhyloseq-accessors}}
#' \code{\link{SummarizedPhyloseq-assign}}
#' \code{\link{phyloseq-class}}
#'
#' @exportClass SummarizedPhyloStats
setClass(
    Class = "SummarizedPhyloStats",
    representation = representation(
        kingdom_table = "dataframeOrNULL",
        phylum_table  = "dataframeOrNULL",
        class_table   = "dataframeOrNULL",
        order_table   = "dataframeOrNULL",
        family_table  = "dataframeOrNULL",
        genus_table   = "dataframeOrNULL",
        species_table = "dataframeOrNULL",
        otu_table     = "dataframeOrNULL"
    ),
    prototype = prototype(
        kingdom_table = "NULL",
        phylum_table  = "NULL",
        class_table   = "NULL",
        order_table   = "NULL",
        family_table  = "NULL",
        genus_table   = "NULL",
        species_table = "NULL",
        otu_table     = "NULL"
    )
)
################################################################################
setValidity(
    "SummarizedPhyloStats",
    function(object){
        splat_list = splat_phyloseq_objects(object)
        for(slot in names(splat_list)){
            if(is.na(splat_list[slot])) next
            colNames = colnames(splat_list[[slot]])
            shared_column = intersect(colNames,
                                      c("baseMean", "logFC", "stat", "pvalue", "padj"))
            if(length(shared_column) < 5){
                return("Slot columns must be baseMean, logFC, stat, pvalue, padj")
            }
        }
    }
)

################################################################################
#' @export
#' @inheritParams methods::show
setMethod(
    "show", signature = "SummarizedPhyloStats",
    definition = function(object){
        cat(">>>>>>>>>>>>>>>>   SummarizedPhyloStats   <<<<<<<<<<<<<<<<<<<\n\n")
        splat_list = splat_phyloseq_objects(object)
        for(slot in names(splat_list)){
            if(!is.na(splat_list[slot])){
                cat(paste0(
                    str_pad(paste0(slot, "()"), width = 18, side = "right"),
                    "    [ ",
                    str_pad(nrow(splat_list[[slot]]), width = 4, side = "left"),
                    " taxa, ",
                    str_pad(sum(splat_list[[slot]]$pvalue < 0.05),
                            width = 3, side = "left"),
                    " p<0.05, ",
                    str_pad(sum(splat_list[[slot]]$padj < 0.05),
                            width = 2, side = "left"),
                    " padj<0.05 ]\n"
                ))
            }else{
                cat(paste0(
                    str_pad(paste0(slot, "()"), width = 18, side = "right"),
                    "    [",
                    paste(rep(" ",16), collapse=""),
                    "empty",
                    paste(rep(" ",16), collapse=""), "]\n"
                ))
            }
        }
    }
)

