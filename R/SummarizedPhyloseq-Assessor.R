################################################################################
##########                       Slot Assessor                        ##########
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
##########                      Slot Assignment                       ##########
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
################################################################################
##########                          Extract                           ##########
################################################################################
#' @rdname Extract
#' @aliases [[
#' @title Extract or replace a slot of an SummarizedPhyloseq or SummarizedPhyloStats object
#' @description Extract or replace the content of a slot of an
#' \code{\link{SummarizedPhyloseq-class}} or an \code{\link{SummarizedPhyloStats-class}} object.
#' @param x An \code{\link{SummarizedPhyloseq-class}} or \code{\link{SummarizedPhyloStats-class}} object.
#' @param name The name of the slot.
#' @export
setMethod(
    f = "[[",
    signature = "SummarizedPhyloseq",
    definition = function(x, i, j, drop){
        if(!is.character(i) | ! i %in% slotNames(x))
            stop("[ phylox ] Slot not found", call. = FALSE)
        slot(x, i)
    }
)
#' rdname Extract
#' @aliases [[<-
#' @export
setReplaceMethod(
    "[[", "SummarizedPhyloseq",
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
    "$", "SummarizedPhyloseq",
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
    "$", "SummarizedPhyloseq",
    function(x, name, value){
        if(! name %in% slotNames(name))
            stop("[ phylox ] Slot not found", call. = FALSE)
        slot(x, name) = value
        validObject(x)
        return(x)
    }
)
