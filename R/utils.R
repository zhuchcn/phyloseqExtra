# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

################################################################################
##' @title summarize_taxa
##'
##' @param physeq a phyloseq object
##' @param level the taxonomy level to summarize. Level must be one from 1 to 7, while 1 is Kingdom and 7 is Species.
##' @param keep_full_tax logistical value whether to use the full tax path. Default is FALSE.
##' @import dplyr
##' @import reshape2
##' @import phyloseq
##' @author Chenghao Zhu
##' @description summarize phyloseq object on different taxonomy level.
##'
##' @examples
##' library(phyloseq)
##' data("GlobalPatterns")
##' GP_family = summarize_taxa(GlobalPatterns, level=5)
summarize_taxa = function(physeq, level = "Family", keep_full_tax = TRUE){
    all_levels = c("Kingdom", "Phylum", "Class", "Order",
                   "Family", "Genus", "Species", "Feature")
    if(!is.character(level) | !level %in% all_levels){
        message(paste(level))
        stop("Level must be one of 'Kingdom', 'Phylum', 'Class', 'Order',
             'Family', 'Genus', 'Species', 'Feature'")
    }

    level = which(level == all_levels)

    otutab = otu_table(physeq)
    taxtab = tax_table(physeq)

    if(keep_full_tax){
        taxonomy = apply(taxtab[,1:level], 1, function(x)
            paste(c("r__Root", x), collapse="|"))
    }else{
        taxonomy = as.character(taxtab[,level])
    }

    taxonomy[is.na(taxonomy)] = "NA"

    otutab %>%
        as.data.frame %>%
        mutate(taxonomy = taxonomy) %>%
        #filter(!is.na(taxonomy) & !grepl("\\|NA", taxonomy)) %>%
        melt(id.var = "taxonomy",
             variable.name = "sample_id") %>%
        group_by(taxonomy, sample_id) %>%
        summarize(value = sum(value)) %>%
        dcast(taxonomy~sample_id)
    }

##' @title fix_duplicate_tax
##'
##' @param physeq a phyloseq object
##' @import phyloseq
##' @author Chenghao Zhu
##' @export
##' @description fix the duplicatae taxonomy names of a phyloseq object

fix_duplicate_tax = function(physeq){
    taxtab <- tax_table(physeq)
    for(i in 3:ncol(taxtab)){
        uniqs = unique(taxtab[,i])
        for(j in 1:length(uniqs)){
            if(is.na(uniqs[j])) next
            ind = which(taxtab[,i]== as.character(uniqs[j]))
            if(length(unique(taxtab[ind,i-1]))>1){
                taxtab[ind,i] = paste(taxtab[ind,i-1], taxtab[ind,i], sep="_")
            }
        }
    }
    tax_table(physeq) = taxtab
    return(physeq)
}

################################################################################
complete_phylo_levels = function(){
    return(c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"))
}
