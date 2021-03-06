% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods-plot.R
\name{plot_heatmap}
\alias{plot_heatmap}
\title{Generate a heatmap with all the taxa the match a given crateria from
a given SummarizedPhyloseq Object.}
\usage{
plot_heatmap(spy, spys, level = "Genus", coef = "pvalue",
  cutoff = 0.05, anno.var, ...)
}
\arguments{
\item{spy}{SummarizedPhyloseq object}

\item{spys}{SummarizedPhyloStats object}

\item{level}{character variable indicates the targeted phylogenic level}

\item{coef}{character, either "pvalue" or "padj"}

\item{cutoff}{numeric, the cutoff of coef to use}

\item{anno.var}{character, indicates the sample metadata variabel to use for
annotation as a side bar. It must be one from the colnames of the sample data.}

\item{...}{other parameters supported by the \code{\link{zheatmap}} function.}
}
\description{
By specifying the phylogenic level, coeficient, and cutoff, all the taxa with
 a adjusted or unadjusted p value that is smaller than the cutoff will be
 selected. Then the summarized OTU table at the specifed level is extracted
 to generate a heatmap.

This function depends on the \code{\link{zheatmap}} package. The package can
be installed from the github using \code{devtools::install_github("zhuchcn/zheatmap")}
}
\examples{
data(fatigue)
fatigue = transform_sample_counts(fatigue, function(x) x/sum(x))
spy = summarizeFromPhyloseq(fatigue)
design = model.matrix(data = as(sample_data(fatigue), "data.frame"), ~Subject + 1)
spys_lm = spy_to_limma(spy_prop, design, transform = "log", p.value = 2, coef = 2)
plot_heatmap(spy,
             spys_lm,
             coef = "pvalue",
             cutoff = 0.1,
             anno.var = "Subject")
}
\seealso{
\code{\link{SummarizedPhyloseq-class}}, \code{\link{SummarizedPhyloStats-class}}, \code{\link{zheatmap}}
}
\author{
Chenghao Zhu
}
