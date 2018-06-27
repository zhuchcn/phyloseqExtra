################################################################################
setGeneric("kingdom_table<-", function(object, value) standardGeneric("kingdom_table<-"))
#' @rdname SummarizedPhyloseq-assign
#' @name kingdom_table<-
#' @title Assignment methods for SummarizedPhyloseq and SummarizedPhyloStats
#'
#' @description Assignment methods for the \code{\link{SummarizedPhyloseq-class}}
#' class and the \code{\link{SummarizedPhyloStats-class}} class
#'
#' @details These methods assign an new \code{\link{otu_table-class}} object to either a SummarizedPhyloseq or a SummarizedPhyloStats object.
#'
#' @param object (Required) \code{\link{SummarizedPhyloseq-class}} or \code{\link{SummarizedPhyloStats-class}}
#' @param value (Required) \code{\link{otu_table-class}}
#'
#' @seealso
#' \code{\link{SummarizedPhyloseq-class}}
#' \code{\link{SummarizedPhyloStats-class}}
#' \code{\link{summarizeFromPhyloseq}}
#'
#' @export
setReplaceMethod(
    f = "kingdom_table",
    signature = "SummarizedPhyloseq",
    definition = function(object, value){
        object@kingdom_table <- value
        validObject(object)
        return(object)
    }
)
################################################################################
setGeneric("phylum_table<-", function(object, value) standardGeneric("phylum_table<-"))
#' @rdname SummarizedPhyloseq-assign
#' @aliases phylum_table<-
#' @export
setReplaceMethod(
    f = "phylum_table",
    signature = "SummarizedPhyloseq",
    definition = function(object, value){
        object@phylum_table <- value
        validObject(object)
        return(object)
    }
)
################################################################################
setGeneric("class_table<-", function(object, value) standardGeneric("class_table<-"))
#' @rdname SummarizedPhyloseq-assign
#' @aliases class_table<-
#' @export
setReplaceMethod(
    f = "class_table",
    signature = "SummarizedPhyloseq",
    definition = function(object, value){
        object@class_table <- value
        validObject(object)
        return(object)
    }
)
################################################################################
setGeneric("order_table<-", function(object, value) standardGeneric("order_table<-"))
#' @rdname SummarizedPhyloseq-assign
#' @aliases order_table<-
#' @export
setReplaceMethod(
    f = "order_table",
    signature = "SummarizedPhyloseq",
    definition = function(object, value){
        object@order_table <- value
        validObject(object)
        return(object)
    }
)
################################################################################
setGeneric("family_table<-", function(object, value) standardGeneric("family_table<-"))
#' @rdname SummarizedPhyloseq-assign
#' @aliases family_table<-
#' @export
setReplaceMethod(
    f = "family_table",
    signature = "SummarizedPhyloseq",
    definition = function(object, value){
        object@family_table <- value
        validObject(object)
        return(object)
    }
)
################################################################################
setGeneric("genus_table<-", function(object, value) standardGeneric("genus_table<-"))
#' @rdname SummarizedPhyloseq-assign
#' @aliases genus_table<-
#' @export
setReplaceMethod(
    f = "genus_table",
    signature = "SummarizedPhyloseq",
    definition = function(object, value){
        object@genus_table <- value
        validObject(object)
        return(object)
    }
)
################################################################################
setGeneric("species_table<-", function(object, value) standardGeneric("species_table<-"))
#' @rdname SummarizedPhyloseq-assign
#' @aliases species_table<-
#' @export
setReplaceMethod(
    f = "species_table",
    signature = "SummarizedPhyloseq",
    definition = function(object, value){
        object@species_table <- value
        validObject(object)
        return(object)
    }
)
################################################################################
# Doesn't work
setReplaceMethod(
    f = "[[",
    signature = "SummarizedPhyloseq",
    definition = function(x, i, value){
        if(! i %in% slotNames(x)){
            stop(str_c(i,"isn't a valid slot name"))
        }
        x[[i]] <- value
        valueObject(x)
        return(x)
    }
)
################################################################################
setReplaceMethod(
    "otu_table",
    signature = "SummarizedPhyloseq",
    definition = function(x, value){
        x@otu_table <- value
        validObject(x)
        return(x)
    }
)
################################################################################
setGeneric("sample_data<-", function(x, value) standardGeneric("sample_data<-"))
setReplaceMethod(
    "sample_data",
    signature = "SummarizedPhyloseq",
    definition = function(x, value){
        x@sam_data <- value
        validObject(x)
        return(x)
    }
)
################################################################################
setReplaceMethod(
    "tax_table",
    signature = "SummarizedPhyloseq",
    definition = function(x, value){
        x@tax_table <- value
        validObject(x)
        return(x)
    }
)

