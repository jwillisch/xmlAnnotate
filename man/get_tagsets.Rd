% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funcs.R
\name{get_tagsets}
\alias{get_tagsets}
\title{Apply get_tagset to each XML file in a folder}
\usage{
get_tagsets(x, nodes = c("hedge"), mae = "2.1")
}
\arguments{
\item{x}{folder}

\item{nodes}{vector of node types that should be extracted (default: just 'hedge')}
}
\value{
a \code{data.frame} containing the \code{rbind}-ed output of \code{get_tagsets}
        for each XML file in the folder
}
\description{
Apply get_tagset to each XML file in a folder
}

