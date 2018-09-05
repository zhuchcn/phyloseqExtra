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
                          str_pad(ntaxa(slt), width = 3, side="left"),
                          " taxa and ",
                          nsamples(slt), " samples ]\n", sep = ""))
            }
        }
        cat("\n>>>>>>>>>>>>>>>> SummarizedPhyloseq-Class <<<<<<<<<<<<<<<<")
    }
)
################################################################################
#' @inheritParams methods::show
#' @export
setMethod(
    "show",
    signature = "otu_table",
    definition = function(object){
        cat(paste0("OTU Table:          [",
                   ntaxa(object),
                   " taxa and ",
                   nsamples(object),
                   " samples]\n"))
        if( taxa_are_rows(object) ){
            cat("                     taxa are rows", fill=TRUE)
        } else {
            cat("                     taxa are columns", fill=TRUE)
        }
        nrow2show = min(8, ntaxa(object))
        ncol2show = min(8, nsamples(object))
        show(object@.Data[1:nrow2show,1:ncol2show])
        nrow_left = ntaxa(object) - nrow2show
        ncol_left = nsamples(object) - ncol2show
        if( nrow_left > 0 ){
            cat(paste0("\n... with ",
                       str_pad(nrow_left, width=4, side="right"),
                       " more taxa"))
        }
        if( ncol2show > 0){
            cat(paste0("\n... with ",
                       str_pad(ncol_left, width=4, side="right"),
                       " more samples"))
        }
    }
)
################################################################################
#' @inheritParams methods::show
#' @export
setMethod(
    "show",
    signature = "sample_data",
    definition = function(object){
        cat(paste0("Sample Data:        [",
                   dim(sample_data(object))[1],
                   " samples by ",
                   dim(sample_data(object))[2],
                   " sample variables]:\n", sep = ""))
        nrow2show = min(10, nrow(object))
        ncol2show = min(5, ncol(object))
        show(as(object[1:nrow2show, 1:ncol2show], "data.frame"))
        nrow_left = nrow(object) - nrow2show
        ncol_left = ncol(object) - ncol2show
        if( nrow_left > 0 ){
            cat(paste0("\n... with ",
                       str_pad(nrow_left, width=4, side="right"),
                       " more samples"))
        }
        if( ncol2show > 0){
            col_classes = sapply(object, class)
            short_names = c("<chr>", "<int>", "<fct>", "<num>")
            names(short_names) = c("character", "integer", "factor", "numeric")
            col_classes = short_names[col_classes]
            col_left = tail(paste(colnames(object), col_classes), ncol_left)
            cat(paste0("\n... with ",
                       str_pad(ncol_left, width=4, side="right"),
                       " more variables:\n",
                       paste(col_left, collapse = ", ")))
        }
    }
)
################################################################################
#' @inheritParams methods::show
#' @export
setMethod(
    "show",
    signature = "taxonomyTable",
    definition = function(object){
        cat(paste0("Taxonomy Table:     [", dim(object)[1], " taxa by ",
                  dim(object)[2],
                  " taxonomic ranks]:\n"))
        nrow2show = min(8, nrow(object))
        nrow_left = nrow(object) - nrow2show
        show(as(object[1:nrow2show,], "matrix"))
        if( nrow_left > 0 ){
            cat(paste0("\n... with ",
                       str_pad(nrow_left, width=4, side="right"),
                       " more taxa"))
        }
    }
)
