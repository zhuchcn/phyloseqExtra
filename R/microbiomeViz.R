## microbiomeViz annotation
#' @title Construct annotation table for microbiomeViz cladogram annotation
#' @description
#' It creats a annotation table base on a SummarizedPhyloStats object with
#' node name and the color to highlight. This table can then be parsed to
#' the \code{\link{clade.anno}} function from the microbiomeViz package. It
#' goes through each slot in thee SummarizedPhyloStats object, and selects the
#' taxa that matches the provided criteria. For example, the default setting
#' selects all the taxa with a unadjusted pvalue small or equal to 0.05, and
#' label the ones with a positive logFC as red, and negative as blue.
#' @param spys (required) A \code{\link{SummarizedPhyloseq-class}} object.
#' @param coef Must be pvalue or padj
#' @param cutoff The coef cutoff for selection. Default is 0.05
#' @param colors It takes a string vector of 2 colors. The first will be
#' positive logFC and the second will be negativ.
#' @param levels The phylogenic levels to annotate, with 1 being kingdom and
#' 7 being species. Default is all levels.
#' @seealso
#' \code{\link{SummarizedPhyloStats}}
#' \code{\link{clade.anno}}
#'
#' @export
create_annodata = function(spys, coef = c("pvalue", "padj"), cutoff = 0.05, colors = c("#00468BFF", "#ED0000FF"), levels = 1:7){
    levels = slotNames(spys)[levels]
    coef = match.arg(coef)
    anno.data = data.frame()
    splat_list = splat_phyloseq_objects(spys)
    has_prefix = grepl("^[kpcofgs]{1}__",rownames(splat_list[[1]])[1])
    for(slot in names(splat_list)){
        if(!slot %in% levels) next
        if(is.na(splat_list[slot])) next
        slot_sub = splat_list[[slot]]%>%
            rownames_to_column("node") %>%
            filter(eval(parse(text = paste0("!is.na(", coef, ") &",
                                            coef, "<=", cutoff))))
        if(!has_prefix){
            slot_sub = mutate(
                slot_sub,
                node = paste0(str_split(slot, "",simplify = T)[1],"__", node))
        }
        anno.data = rbind(
            anno.data,
            data.frame(
                node = slot_sub$node,
                color = ifelse(slot_sub$logFC > 0, colors[1], colors[2]),
                stringsAsFactors = FALSE
            )
        )
    }
    return(anno.data)
}
