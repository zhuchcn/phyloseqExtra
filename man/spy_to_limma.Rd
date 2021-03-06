% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phyloseq_to_stats.R
\name{spy_to_limma}
\alias{spy_to_limma}
\title{Linear model using limma on different phylogenic levels}
\usage{
spy_to_limma(spy, design, transform = function(x) {     log2(x + 1) },
  coef, p.value, ...)
}
\arguments{
\item{spy}{(required) \code{\link{SummarizedPhyloseq-class}}}

\item{design}{(required) The design matrix of the experiment, with rows
corresponding to sample IDs and columns to coefficients to be estimated.
This can be constructed using \code{\link{model.matrix}} function}

\item{transform}{(required) The method to use to transform the data before fitting
linear model. "log" will apply a log transform to the data while "none" won't
do anything.}

\item{coef}{(required) The index number or name of the coefficient (variable) for
building the results tables. The value provided must be one of the colnames
or indices of the design matrix. This argument is corresponding to the
\code{coef} in the \code{\link{topTable}} from the limma package.}

\item{p.value}{(required) Same as \code{coef}}

\item{...}{(optional) Other parameters to be parsed to the
\code{\link{topTable}} function}
}
\description{
Fit linear models using the limma package to a SummarizedPhyloseq
object. See the \code{\link{lmFit}} for more detail.
}
\note{
the limma package needs to be installed in order to successfully run
this function
}
\author{
Chenghao Zhu
}
