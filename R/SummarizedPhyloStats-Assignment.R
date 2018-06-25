################################################################################
#' @rdname SummarizedPhyloseq-assign
#' @aliases kingdom_table<-
#' @export
setReplaceMethod(
    f = "kingdom_table",
    signature = "SummarizedPhyloStats",
    definition = function(object, value){
        object@kingdom_table <- value
        validObject(object)
        return(object)
    }
)
################################################################################
#' @rdname SummarizedPhyloseq-assign
#' @aliases phylum_table<-
#' @export
setReplaceMethod(
    f = "phylum_table",
    signature = "SummarizedPhyloStats",
    definition = function(object, value){
        object@phylum_table <- value
        validObject(object)
        return(object)
    }
)
################################################################################
#' @rdname SummarizedPhyloseq-assign
#' @aliases calss_table<-
#' @export
setReplaceMethod(
    f = "class_table",
    signature = "SummarizedPhyloStats",
    definition = function(object, value){
        object@class_table <- value
        validObject(object)
        return(object)
    }
)
################################################################################
#' @rdname SummarizedPhyloseq-assign
#' @aliases order_table<-
#' @export
setReplaceMethod(
    f = "order_table",
    signature = "SummarizedPhyloStats",
    definition = function(object, value){
        object@order_table <- value
        validObject(object)
        return(object)
    }
)
################################################################################
#' @rdname SummarizedPhyloseq-assign
#' @aliases family_table<-
#' @export
setReplaceMethod(
    f = "family_table",
    signature = "SummarizedPhyloStats",
    definition = function(object, value){
        object@family_table <- value
        validObject(object)
        return(object)
    }
)
################################################################################
#' @rdname SummarizedPhyloseq-assign
#' @aliases genus_table<-
#' @export
setReplaceMethod(
    f = "genus_table",
    signature = "SummarizedPhyloStats",
    definition = function(object, value){
        object@genus_table <- value
        validObject(object)
        return(object)
    }
)
################################################################################
#' @rdname SummarizedPhyloseq-assign
#' @aliases species_table<-
#' @export
setReplaceMethod(
    f = "species_table",
    signature = "SummarizedPhyloStats",
    definition = function(object, value){
        object@species_table <- value
        validObject(object)
        return(object)
    }
)
################################################################################
#' @rdname SummarizedPhyloseq-assign
#' @export
setReplaceMethod(
    f = "otu_table",
    signature = "SummarizedPhyloStats",
    definition = function(x, value){
        x@otu_table <- value
        validObject(x)
        return(x)
    }
)

