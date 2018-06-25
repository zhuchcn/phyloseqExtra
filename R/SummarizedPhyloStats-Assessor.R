## Slot assessor
## ------------- getter -------------
################################################################################
#' @rdname SummarizedPhyloseq-accessors
#' @export
setMethod(
    f = "kingdom_table",
    signature = "SummarizedPhyloStats",
    definition = function(object){
        return(object@kingdom_table)
    }
)
################################################################################
#' @rdname SummarizedPhyloseq-accessors
#' @export
setMethod(
    f = "phylum_table",
    signature = "SummarizedPhyloStats",
    definition = function(object){
        return(object@phylum_table)
    }
)
################################################################################
#' @rdname SummarizedPhyloseq-accessors
#' @export
setMethod(
    f = "class_table",
    signature = "SummarizedPhyloStats",
    definition = function(object){
        return(object@class_table)
    }
)
################################################################################
#' @rdname SummarizedPhyloseq-accessors
#' @export
setMethod(
    f = "order_table",
    signature = "SummarizedPhyloStats",
    definition = function(object){
        return(object@order_table)
    }
)
################################################################################
#' @rdname SummarizedPhyloseq-accessors
#' @export
setMethod(
    f = "family_table",
    signature = "SummarizedPhyloStats",
    definition = function(object){
        return(object@family_table)
    }
)
################################################################################
#' @rdname SummarizedPhyloseq-accessors
#' @export
setMethod(
    f = "genus_table",
    signature = "SummarizedPhyloStats",
    definition = function(object){
        return(object@genus_table)
    }
)
################################################################################
#' @rdname SummarizedPhyloseq-accessors
#' @export
setMethod(
    f = "species_table",
    signature = "SummarizedPhyloStats",
    definition = function(object){
        return(object@species_table)
    }
)
################################################################################
#' @rdname SummarizedPhyloseq-accessors
#' @export
setMethod(
    f = "otu_table",
    signature = "SummarizedPhyloStats",
    definition = function(object){
        return(object@otu_table)
    }
)
