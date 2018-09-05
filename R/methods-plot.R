################################################################################
#' @title Stacked bar plot for SummarizedPhyloseq data
#' @description
#' This function uses the ggplot2 package to generate barplot from a given
#' \code{\link{SummarizedPhyloseq-class}} object on a given phylogenic level.
#' The output is a ggplot object so it can be easily styled using additional
#' ggplot functions.
#' @param spy \code{\link{SummarizedPhyloseq-class}}
#' @param level Character. The phyloseq level to use. Must be one of
#' Kingdom, Phylum, Class, Order, Family, Genus, or Species.
#' @param by Character. The sample meta-data variable to plot the
#' abundance data againt to. It must be from the sample data's column names.
#' If multiple variables are givin, facets will be used. The default is
#' sample name. See examples.
#' @param legend.show Logical variable whether to show the legends.
#' @param plotly Logical value. If TRUE, a plotly variable will be returned.
#' @import ggplot2
#' @author Chenghao Zhu
#' @export
#' @examples
#' data(fatigue)
#' fatigue = transform_sample_counts(physeq, function(x) x/sum(x))
#' spy = summarizeFromPhyloseq(fatigue)
#' plot_bar(spy)
#' plot_bar(spy, "Phylum", by = "Subject")
#' plot_bar(spy, "Genus", by = c("Subject", "Sex"))
#' plot_bar(spy, "Genus", by = c("Subject", "Sex"), plotly = T)
#' plot_bar(spy, "Phylum", by = c("Subject", "age_range", "Sex"))
plot_bar = function(spy, level="Phylum", by = NULL, show.legend = TRUE){

    # check auguments
    if(is.null(by)) by = "sample_id"

    if(length(by) > 3) stop("Group variable number must be <= 3")
    if(!level %in% complete_phylo_levels())
        stop(paste0("Invalid level. Must be one from ",
                    paste(complete_phylo_levels(), collapse = ", ")))

    # retrieve data
    slot = eval(parse(text = paste0(tolower(level), "_table(spy)"))) %>%
        as.data.frame %>%
        t %>% as.data.frame %>%
        rownames_to_column("sample_id")

    sam_data = as(sample_data(spy), "data.frame") %>%
        rownames_to_column("sample_id")

    # clean data
    mdf = merge(slot, sam_data, by = "sample_id") %>%
        melt(c(colnames(sam_data)),
             variable.name = "OTU", value.name = "Abundance") %>%
        group_by_(.dots = c("OTU", by)) %>%
        summarize(Abundance = mean(Abundance)) %>%
        ungroup() %>%
        as.data.frame

    colnames(mdf) = c("OTU",
                      paste0(rep("V", ncol(mdf)-2), 1:(ncol(mdf)-2)),
                      "Abundance")

    # make plot
    p = ggplot(mdf) +
        geom_bar(aes(x = V1, y = Abundance, fill = OTU),
                 stat = "identity", position = "stack") +
        labs(x = "") +
        theme_bw() +
        guides(fill = guide_legend(title = level))
    if(length(by) == 2){
        p = p + facet_grid(.~V2)
    }else if(length(by) == 3){
        p = p + facet_grid(V2 ~ V3)
    }
    if(!show.legend){
        p = p + theme(legend.position = "none")
    }
    p
}
################################################################################
#' @title Boxplot for SummarizedPhyloseq data
#' @description
#' This function uses the ggplot2 package to generate a boxplot from a givin
#' \code{\link{SummarizedPhyloseq-class}} object by specifying a taxon name.
#' @param spy \code{\link{SummarizedPhyloseq-class}}
#' @param level character. The phylogenic level.
#' @param taxon character. The taxon name to plot.
#' @param by character. The sample meta-data variable to plot the
#' abundance data againt to. It must be from the sample data's column names.
#' If multiple variables are givin, facets will be used. The default is
#' sample name. See examples.
#' @param line character. A sample meta-data variable. If specified, geom_line
#' will be called to draw lines between 2 points. This is particually usful to
#' deal with repeated measures.
#' @param color.by character. A sample meta-data variable. If specified, points
#' with different levels will be colored diffently. See examples.
#' @param jitter numeric. If specified, points will be jittered. Recommanded
#' value: 0.15. the \code{line} and \code{jitter} can not be specified at the
#' same time.
#' @param point.size numeric. The size of points. Default is 3
#' @param point.alpha numeric. The transparency of points.
#' @param point.color character. If the \code{color.by} is not specified, this
#' value will be given the to the points color.
#' @param whisker.width numeirc. The width of boxplot whisker. Default is 0.5.
#' @param color.pal character. The color panel to use.
#' @param show.legend logical. Whether to show legend. Default is TRUE.
#' @param syle character. The pre-defined style to apply on the plot. "bw" is a
#' empty default style using the \code{\link{theme_bw}}. "academic" is a classic
#' style based on the \code{\link{theme_classic}}.
#' @param plotly logical. If TRUE, a plotly variable will be returned.
#' @author Chenghao Zhu
#' @export
#' @examples
#' data(fatigue)
#' fatigue = transform_sample_counts(fatigue, function(x) x/sum(x))
#' spy = summarizeFromPhyloseq(fatigue)
#' plot_box(spy, level = "Genus", taxon = "g__Ruminococcus", by = "Subject", jitter = 0.15)
#' plot_box(spy, level = "Genus", taxon = "g__Ruminococcus", by = c("Subject","Sex"), box.size = 1, whisker.size = 1, show.points = F, style = "academic")
#' plot_box(spy, level = "Genus", taxon = "g__Ruminococcus", by = c("Subject","Sex"), jitter = 0.15, box.size = 1, whisker.size = 1, point.alpha = 0.75, point.color = "steelblue", style = "academic")
plot_boxplot = function(spy, level, taxon, x, rows, cols, line, color, ...){

    if(!requireNamespace("ggmetaplots")){
        stop("[ phylox: PackageNotFound ] Can't find the ggmetaplot package. Please install it using:\n\n    devtools::install_github('zhuchcn/ggmetaplot')",
             call. = FALSE)
    }

    level_list = complete_phylo_levels() %>% tolower()
    level = tolower(level)

    if(!level %in% level_list)
        stop(paste0("[ phylox ]Invalid level. Must be one from ",
                    paste(level_list, collapse = ", ")))

    args = as.list(match.call())[-c(1:3)]
    names(args)[names(args) == "taxon"] = "y"

    sample_vars = unique(c(args$x, args$rows, args$cols, args$color, args$line))

    level_list = paste0(level_list, "_table")
    names(level_list) = tolower(complete_phylo_levels())

    df = data.frame(
        abundance = as.numeric(spy[[level_list[level]]][taxon,])
    ) %>%
        cbind(spy@sam_data[,sample_vars])
    colnames(df)[-1] = sample_vars

    args$data = df
    args$y = "abundance"

    do.call(ggmetaplots::ggboxplot, args)
}
################################################################################
#' @title Generate a heatmap with all the taxa the match a given crateria from
#' a given SummarizedPhyloseq Object.
#' @description
#' By specifying the phylogenic level, coeficient, and cutoff, all the taxa with
#'  a adjusted or unadjusted p value that is smaller than the cutoff will be
#'  selected. Then the summarized OTU table at the specifed level is extracted
#'  to generate a heatmap.
#'
#' This function depends on the \code{\link{zheatmap}} package. The package can
#' be installed from the github using \code{devtools::install_github("zhuchcn/zheatmap")}
#' @author Chenghao Zhu
#' @param spy SummarizedPhyloseq object
#' @param spys SummarizedPhyloStats object
#' @param level character variable indicates the targeted phylogenic level
#' @param coef character, either "pvalue" or "padj"
#' @param cutoff numeric, the cutoff of coef to use
#' @param anno.var character, indicates the sample metadata variabel to use for
#' annotation as a side bar. It must be one from the colnames of the sample data.
#' @param ... other parameters supported by the \code{\link{zheatmap}} function.
#' @seealso \code{\link{SummarizedPhyloseq-class}}, \code{\link{SummarizedPhyloStats-class}}, \code{\link{zheatmap}}
#' @examples
#' data(fatigue)
#' fatigue = transform_sample_counts(fatigue, function(x) x/sum(x))
#' spy = summarizeFromPhyloseq(fatigue)
#' design = model.matrix(data = as(sample_data(fatigue), "data.frame"), ~Subject + 1)
#' spys_lm = spy_to_limma(spy_prop, design, transform = "log", p.value = 2, coef = 2)
#' plot_heatmap(spy,
#'              spys_lm,
#'              coef = "pvalue",
#'              cutoff = 0.1,
#'              anno.var = "Subject")
#' @export
plot_heatmap = function(spy,
                        spys,
                        level = "Genus",
                        coef = "pvalue",
                        cutoff = 0.05,
                        anno.var,
                        ...
){
    if (!requireNamespace("zheatmap", quietly = TRUE)) {
        stop("[ phylox: PackageNotFound ] The \"zheatmap\" package is required for this funciton. Please install it.")
    }

    if(!level %in% complete_phylo_levels())
        stop(paste0("Invalid level. Must be one from ",
                    paste(complete_phylo_levels(), collapse = ", ")))

    otutab = eval(parse(text = paste0(tolower(level), "_table(spy)"))) %>%
        as.data.frame
    statab = eval(parse(text = paste0(tolower(level), "_table(spys)"))) %>%
        as.data.frame
    samtab = as(sample_data(spy), "data.frame")

    otu.list = statab %>%
        rownames_to_column("OTU") %>%
        filter_at(vars(coef), any_vars( . <= cutoff)) %>%
        filter(OTU != "NA") %>%
        select("OTU")

    data = as.data.frame(otutab[otu.list$OTU,])

    colSideBar = if(!missing(anno.var)) samtab[,anno.var] else NULL

    zheatmap::zheatmap(data = data,
                       colSideBar = colSideBar,
                       ...)
}
