################################################################################
##########                           Getter                           ##########
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
################################################################################
##########                           Setter                           ##########
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
################################################################################
##########                          Extract                           ##########
################################################################################
#' @rdname Extract
#' @aliases [[
#' @export
setMethod(
    f = "[[",
    signature = "SummarizedPhyloStats",
    definition = function(x, i, j, drop){
        if(!is.character(i) | ! i %in% slotNames(x))
            stop("[ phylox ] Slot not found", call. = FALSE)
        slot(x, i)
    }
)
#' @rdname Extract
#' @aliases [[<-
#' @export
setReplaceMethod(
    "[[", "SummarizedPhyloStats",
    function(x, i, j, ..., value){
        if(!is.character(i) | ! i %in% slotNames(x))
            stop("[ phylox ] Slot not found", call. = FALSE)
        slot(x, i) = value
        validObject(x)
        return(x)
    }
)
#' @rdname Extract
#' @aliases $
#' @export
setMethod(
    "$", "SummarizedPhyloStats",
    function(x, name){
        if(! name %in% slotNames(x))
            stop("[ phylox ] Slot not found", call. = FALSE)
        slot(x, name)
    }
)
#' @rdname Extract
#' @aliases $<-
#' @export
setReplaceMethod(
    "$", "SummarizedPhyloStats",
    function(x, name, value){
        if(! name %in% slotNames(name))
            stop("[ phylox ] Slot not found", call. = FALSE)
        slot(x, name) = value
        validObject(x)
        return(x)
    }
)

