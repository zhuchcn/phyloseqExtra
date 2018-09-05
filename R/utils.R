################################################################################
#' @keywords internal
'%+%' = function(a, b) paste0(a, b)
################################################################################
#' @keywords internal
complete_phylo_levels = function(){
    return(c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species", "OTU"))
}
