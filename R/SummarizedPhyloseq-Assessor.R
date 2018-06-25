## Slot assessor
################################################################################
setGeneric("kingdom_table", function(object) standardGeneric("kingdom_table"))
#' @name kingdom_table
#' @rdname SummarizedPhyloseq-accessors
#' @title Accessors for SummarizedPhyloseq class and SummarizedPhyloStats class
#'
#' @description Accessor methods for the \code{\link{SummarizedPhyloseq-class}}
#' and the \code{\link{SummarizedPhyloStats-class}}
#'
#' @details These methods retrieve slots from either a SummarizedPhyloseq or a SummarizedPhyloStats object.
#' \describe{
#'     \item{kingdom_table()}{Retrieve the kingdom_table slot}
#'     \item{phylum_table()}{Retrieve the phylum_table slot}
#'     \item{class_table()}{Retrieve the class_table slot}
#'     \item{order_table()}{Retrieve the order_table slot}
#'     \item{family_table()}{Retrieve the family_table slot}
#'     \item{genus_table()}{Retrieve the genus_table slot}
#'     \item{species_table()}{Retrieve the species_table slot}
#'     \item{otu_table()}{Retrieve the otu_table slot}
#' }
#'
#' @param object Either a SummarizedPhyloseq or SummarziedPhyloStats object.
#' @return A otu_table-class object.
#' @author Chenghao Zhu
#'
#' @seealso
#' \code{\link{SummarizedPhyloseq-class}}
#' \code{\link{SummarizedPhyloStats-class}}
#' \code{\link{summarizeFromPhyloseq}}
#'
#' @export
setMethod(
    f = "kingdom_table",
    signature = "SummarizedPhyloseq",
    definition = function(object){
        return(object@kingdom_table)
    }
)
################################################################################
setGeneric("phylum_table", function(object) standardGeneric("phylum_table"))
#' @rdname SummarizedPhyloseq-accessors
#' @aliases phylum_table
#' @export
setMethod(
    f = "phylum_table",
    signature = "SummarizedPhyloseq",
    definition = function(object){
        return(object@phylum_table)
    }
)
################################################################################
setGeneric("class_table", function(object) standardGeneric("class_table"))
#' @rdname SummarizedPhyloseq-accessors
#' @aliases class_table
#' @export
setMethod(
    f = "class_table",
    signature = "SummarizedPhyloseq",
    definition = function(object){
        return(object@class_table)
    }
)
################################################################################
setGeneric("order_table", function(object) standardGeneric("order_table"))
#' @rdname SummarizedPhyloseq-accessors
#' @aliases order_table
#' @export
setMethod(
    f = "order_table",
    signature = "SummarizedPhyloseq",
    definition = function(object){
        return(object@order_table)
    }
)
################################################################################
setGeneric("family_table", function(object) standardGeneric("family_table"))
#' @rdname SummarizedPhyloseq-accessors
#' @aliases family_table
#' @export
setMethod(
    f = "family_table",
    signature = "SummarizedPhyloseq",
    definition = function(object){
        return(object@family_table)
    }
)
################################################################################
setGeneric("genus_table", function(object) standardGeneric("genus_table"))
#' @rdname SummarizedPhyloseq-accessors
#' @aliases genus_table
#' @export
setMethod(
    f = "genus_table",
    signature = "SummarizedPhyloseq",
    definition = function(object){
        return(object@genus_table)
    }
)
################################################################################
setGeneric("species_table", function(object) standardGeneric("species_table"))
#' @rdname SummarizedPhyloseq-accessors
#' @aliases species_table
#' @export
setMethod(
    f = "species_table",
    signature = "SummarizedPhyloseq",
    definition = function(object){
        return(object@species_table)
    }
)
################################################################################
#' @export
setMethod(
    f = "[[",
    signature = "SummarizedPhyloseq",
    definition = function(x, i, j, drop){
        if(i %in% slotNames(x)){
            return(eval(parse(text=paste0("x@", i))))
        }else{}
    }
)
