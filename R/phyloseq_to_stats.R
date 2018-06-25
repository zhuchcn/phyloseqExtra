################################################################################
#' @name spy_to_deseq2
#' @title Conduct DESeq2 analysis on different phylogenic levels
#' @description
#' Conduct differential expression using the DESeq2 package to a
#' SummarizedPhyloseq object. See the \code{\link{DESeq}} for more detail.
#' @note The DESeq2 package needs to be installed in order to successfully run
#' this function.
#' @param spy (required) \code{\link{SummarizedPhyloseq-class}}
#' @param design (required) The design matrix of the experiment, with rows
#' corresponding to sample IDs and columns to coefficients to be estimated.
#' This can be constructed using \code{\link{model.matrix}} function
#' @param resultsName The name of the coefficient (variable) for building the
#' results tables. The value provided must be one of the colnames of the design
#' matrix. This argument is corresponding to the \code{name} in the
#' \code{\link{results}} from the DESeq2 package.
#' @param ... (optional) Other parameters to be parsed to the
#' \code{\link{DESeq}} function
#' @author Chenghao Zhu
#' @export
spy_to_deseq2 = function(spy, design, resultsName, ...){
    if (!requireNamespace("DESeq2", quietly = TRUE)) {
        stop("Package \"DESeq2\" needed for this function to work. Please install it.",
             call. = FALSE)
    }
    sample_data = sample_data(spy)
    splat_list = splat_phyloseq_objects(spy)
    splat_list = splat_list[sapply(splat_list, class) == "otu_table" & !is.na(splat_list)]
    result_list = lapply(splat_list, function(table){
        ps = phyloseq(table, sample_data)
        de = phyloseq_to_deseq2(ps, design)
        de = DESeq2::DESeq(de, ...)
        res = as.data.frame(DESeq2::results(de, name = resultsName))
        res = res[,c("baseMean", "log2FoldChange", "stat", "pvalue", "padj")]
        names(res) = c("baseMean", "logFC", "stat", "pvalue", "padj")
        return(res)
    })
    do.call(new, c(list(Class="SummarizedPhyloStats"), result_list))
}

################################################################################
#' @name spy_to_limma
#' @title Linear model using limma on different phylogenic levels
#' @description
#' Fit linear models using the limma package to a SummarizedPhyloseq
#' object. See the \code{\link{lmFit}} for more detail.
#' @note the limma package needs to be installed in order to successfully run
#' this function
#' @param spy (required) \code{\link{SummarizedPhyloseq-class}}
#' @param design (required) The design matrix of the experiment, with rows
#' corresponding to sample IDs and columns to coefficients to be estimated.
#' This can be constructed using \code{\link{model.matrix}} function
#' @param transform (required) The method to use to transform the data before fitting
#' linear model. "log" will apply a log transform to the data while "none" won't
#' do anything.
#' @param coef (required) The index number or name of the coefficient (variable) for
#' building the results tables. The value provided must be one of the colnames
#' or indices of the design matrix. This argument is corresponding to the
#' \code{coef} in the \code{\link{topTable}} from the limma package.
#' @param p.value (required) Same as \code{coef}
#' @param ... (optional) Other parameters to be parsed to the
#' \code{\link{topTable}} function
#' @author Chenghao Zhu
#' @export

spy_to_limma = function(spy, design, transform = c("log", "none"), coef, p.value, ...){
    if (!requireNamespace("limma", quietly = TRUE)) {
        stop("Package \"limma\" needed for this function to work. Please install it.",
             call. = FALSE)
    }
    transform = match.arg(transform)
    if(transform == "log"){
        transform = function(x) log(x + 1)
    }else{
        transform = I
    }

    sample_data = sample_data(spy)
    splat_list = splat_phyloseq_objects(spy)
    splat_list = splat_list[sapply(splat_list, class) == "otu_table" & !is.na(splat_list)]
    result_list = lapply(splat_list, function(table){
        otu = as.data.frame(table)
        fit = limma::lmFit(otu, design)
        fit_ebayes = limma::eBayes(fit)
        res = limma::topTable(fit_ebayes, sort.by = "none", number = nrow(otu),
                       coef = coef, p.value = p.value,...)
        res = res[,c("AveExpr", "logFC", "t", "P.Value", "adj.P.Val")]
        names(res) = c("baseMean", "logFC", "stat", "pvalue", "padj")
        return(res)
    })
    do.call(new, c(list(Class="SummarizedPhyloStats"), result_list))
}
