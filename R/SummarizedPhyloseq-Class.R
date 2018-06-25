################################################################################
#' S4 class SummarizedPhyloseq
#'
#' A S4 Object for storing phyloseq data at all phylogenic levels
#'
#' This class inherit from the
#' \code{\link{phyloseq-class}}
#' from the phyloseq package. It contains three data classes:
#' \code{\link{otu_table-class}} ("otu_table" slot),
#' \code{\link{sample_data-class}} ("sample_data" slot), and
#' \code{\link{taxonomyTable-class}} ("tax_table" slot).
#' Besides the three slots inherit from phyloseq-class, the SummarizedPhyloseq
#' also has 7 more slots, "kingdom_table", "phylum_table", "class_table",
#' "order_table", "family_table", "genus_table", and "species_table", all in
#' the class of otu_table-class. This allows you to view the data in different
#' phylogenic levels.
#' \code{\link{summarizeFromPhyloseq}}
#' is the main constructor for this class, directly from a phyloseq object.
#' The phyloseq needs to have the otu_table, tax_table, and sample_data to
#' successfully construct this class.
#'
#' slots:
#' \describe{
#'     \item{otu_table}{ a tabular object of the OTU table, class of otu_table-class}
#'     \item{sam_data}{ a tabular object of the sample meta-data, class of sample_data.}
#'     \item{tax_table}{ a tabular object of the taxonomy table, class of taxonomyTable.}
#'     \item{kingdom_table}{ a tabular object of the summarized OTU table into the kingdom level, class of otu_table}
#'     \item{phylum_table}{ a tabular object of the summarized OTU table into the phylum level, class of otu_table}
#'     \item{class_table}{ a tabular object of the summarized OTU table into the class level, class of otu_table}
#'     \item{order_table}{ a tabular object of the summarized OTU table into the order level, class of otu_table}
#'     \item{family_table}{ a tabular object of the summarized OTU table into the family level, class of otu_table}
#'     \item{genus_table}{ a tabular object of the summarized OTU table into the genus level, class of otu_table}
#'     \item{species_table}{ a tabular object of the summarized OTU table into the species level, class of otu_table}
#' }
#'
#' @seealso
#' \code{\link{phyloseq-class}}
#' \code{\link{summarizeFromPhyloseq}}
#' \code{\link{SummarizedPhyloStats-class}}
#' \code{\link{SummarizedPhyloseq-accessors}}
#' \code{\link{SummarizedPhyloseq-assign}}
#'
#' @importClassesFrom phyloseq phyloseq
#' @name SummarizedPhyloseq-class
#' @aliases SummarizedPhyloseq-class
#' @exportClass SummarizedPhyloseq
#' @rdname SummarizedPhyloseq-class
#' @author Chenghao Zhu
setClass(
    Class = "SummarizedPhyloseq",
    representation = representation(
        kingdom_table = "otu_tableOrNULL",
        phylum_table  = "otu_tableOrNULL",
        class_table   = "otu_tableOrNULL",
        order_table   = "otu_tableOrNULL",
        family_table  = "otu_tableOrNULL",
        genus_table   = "otu_tableOrNULL",
        species_table = "otu_tableOrNULL"
    ),
    contains = "phyloseq",
    prototype = prototype(
        kingdom_table = "NULL",
        phylum_table  = "NULL",
        class_table   = "NULL",
        order_table   = "NULL",
        family_table  = "NULL",
        genus_table   = "NULL",
        species_table = "NULL"
    )
)
################################################################################
#' summarizeFromPhyloseq
#'
#' Construct a
#' \code{\link{SummarizedPhyloseq-class}} object from a
#' \code{\link{phyloseq-class}} object
#'
#' This is the main constructor of the SummarizedPhyloseq class. It takes a
#' phyloseq object and calls the
#' \code{link{summarize_taxa}}() function for each phylogenic level and return
#' a SummarizedPhyloseq object.
#'
#' @usage summarizeFromPhyloseq(physeq, level = "all", keep_full_tax = FALSE)
#'
#' @param physeq An phyloseq object from the phyloseq package. It must contain
#' an otu_table slot, a sam_data slot, and a tax_table slot. The phy_tree slot
#' and refseq slots are not required.
#' @param level The taxonomy level to summarize. It can be one ore more from
#' Kingdom, Phylum, Class, Order, Family, Genus, and Species. If mutiple
#' phylogenic levels are specified, use a string vector. Default is "all" that
#' parses all levels. The slots that are not specified will be NULL.
#'
#' @return A SummarizedPhyloseq object
#'
#' @seealso \code{\link{SummarizedPhyloseq-class}}
#'
#' @author Chenghao Zhu
#' @import phyloseq
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tibble
#' @export
#' @examples
#' library(phyloseq)
#' data(GlobalPatterns)
#' spy = summarizedFromPhyloseq(GlobalPatterns)
summarizeFromPhyloseq <- function(physeq, level = "all"){
    if( length(level) == 1 & "all" %in% level){
        level = c("Kingdom", "Phylum", "Class", "Order",
                  "Family", "Genus", "Species")
    }
    all_levels = c("Kingdom", "Phylum", "Class", "Order",
                   "Family", "Genus", "Species")

    summarizedPhyseq = lapply(level, function(lvl){
        otu_table = summarize_taxa(physeq, level = lvl,
                                   keep_full_tax = FALSE) %>%
            column_to_rownames("taxonomy")
        otu_table(otu_table, taxa_are_rows = TRUE)
    })
    names(summarizedPhyseq) = level

    for(lvl in all_levels){
        if(lvl %in% level){
            assign(lvl, summarizedPhyseq[[lvl]])
        }else{
            assign(lvl, NULL)
        }
    }

    new("SummarizedPhyloseq",
        kingdom_table = Kingdom,
        phylum_table  = Phylum,
        class_table   = Class,
        order_table   = Order,
        family_table  = Family,
        genus_table   = Genus,
        species_table = Species,
        otu_table     = otu_table(physeq),
        tax_table     = tax_table(physeq),
        sam_data      = sample_data(physeq))
}
################################################################################
#' @inheritParams methods::show
#' @import stringr
#' @export
setMethod(
    "show",
    signature = "SummarizedPhyloseq",
    definition = function(object){
        cat(">>>>>>>>>>>>>>>> summarized phyloseq object <<<<<<<<<<<<<<<<\n\n")
        show(as(object, "phyloseq"))
        levels = c("kingdom_table", "phylum_table", "class_table", "order_table",
                   "family_table", "genus_table", "species_table")
        cat("\nphyloseq extra slots:\n")
        for(lvl in levels){
            slt = eval(parse(text = paste0("object@", lvl)))
            if(!is.null(slt)){
                cat(paste(str_pad(paste0(lvl, "()"), width=18, side="right"),
                          str_pad(paste0(str_to_title(
                              gsub("\\(\\)","",gsub("\\_"," ", lvl))),":"),
                              width = 15, side = "right"),
                          "[ ",
                          str_pad(ntaxa(slt), width = 3, side="right"),
                          " taxa and ",
                          nsamples(slt), " samples ]\n", sep = ""))
            }
        }
        cat("\n>>>>>>>>>>>>>>>> SummarizedPhyloseq-Class <<<<<<<<<<<<<<<<")
    }
)
################################################################################
## This validity method checks if the summarized slots have the same sample
## names as the sam_data
setValidity(
    "SummarizedPhyloseq",
    function(object){
        sp_list = splat_phyloseq_objects(object)
        otu_tables = c("kingdom_table", "phylum_table", "class_table",
                       "order_table", "family_table", "genus_table",
                       "species_table")
        object_sample_names = sample_names(object@sam_data)
        for(tbl in otu_tables){
            table = sp_list[[tbl]]
            if(!is.null(table)){
                if((length(sample_names(table)) != length(object_sample_names)) |
                   all.equal(sample_names(table), object_sample_names) != T){
                    return(paste("in ", tbl, " sample names don't match"))
                }
            }
        }
        validObject(as(object, "phyloseq"))
    }
)

################################################################################
### This function converts a phyloseq object to an unclassed list, with the
### name of each element being the name of each slot. It actually works for
### other classes not only phyloseq.
#' @keywords internal
splat_phyloseq_objects = function(object){
    slot_names = slotNames(object)
    slot_list = lapply(slot_names, function(slt){
        eval(parse(text = paste0("object@", slt)))
    })
    names(slot_list) = slot_names
    return(slot_list[!is.na(slot_list)])
}

################################################################################
# this funciton converts sample_data to a data.frame
#' @export
setMethod(
    f = "as.data.frame",
    signature = "sample_data",
    definition = function(x){
        as(x, "data.frame")
    }
)
