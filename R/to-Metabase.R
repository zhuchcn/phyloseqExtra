################################################################################
#' @title Convert a phyloseq object to a MicrobiomeSet object
#' @param physeq \code{\link{phyloseq-class}} object
#' @return \code{\link{MicrobiomeSet-class}} object
#' @export
#' @author Chenghao Zhu
as_MicrobiomeSet = function(physeq){

    if(!requireNamespace("Metabase"))
        stop("[ phylox: PackageNotFound ] The Metabase package is not found")
    if(!isClass(physeq, Class = "phyloseq"))
        stop("[ phylox ] Input object must be phyloseq class")

    otu_table = as(physeq@otu_table, "matrix")
    otu_table = Metabase::conc_table(otu_table)

    sample_data = physeq@sam_data
    if(is.null(sample_data)) {
        warning("[ phylox ] phyloseq object does not have a sample data")
    } else {
        sample_data = as(sample_data, "data.frame")
        sample_data = Metabase::sample_table(sample_data)
    }

    tax_table = physeq@tax_table
    if(!is.null(tax_table)){
        tax_table = as.data.frame(as(tax_table, "matrix"),
                                  stringsAsFactors = FALSE)
        tax_table = Metabase::feature_data(tax_table)
    }

    Metabase::MicrobiomeSet(
        conc_table = otu_table,
        sample_table = sample_data,
        feature_data = tax_table
    )
}
################################################################################
#' @title Convert a SummarizedPhyloseq object to a list of MicrobiomeSet
#' @param spy a \code{\link{SummarizedPhyloseq-class}} object
#' @export
#' @seealso \code{\link{phyloseq_to_mSet}} \code{\link{SummarizedPhyloseq-class}}
#' @author Chenghao Zhu
as_MicrobiomeSetList = function(spy){

    if(!requireNamespace("Metabase"))
        stop("[ phylox ] [ Package Not Found ] The Metabase package is not found")
    if(!isClass(spy, Class = "SummarizedPhyloseq"))
        stop("[ phylox ] Input object must be SummarizedPhyloseq class")

    mSet_li = lapply(levels(spy), function(lvl){
        ps = phyloseq(spy[[lvl]], spy@sam_data)
        if(lvl == "otu_table")
            ps@tax_table = spy@tax_table
        as_MicrobiomeSet(ps)
    })

    names(mSet_li) = gsub("_table", "", levels(spy))

    return(mSet_li)
}
