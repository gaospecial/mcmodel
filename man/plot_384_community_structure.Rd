% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_384.R
\name{plot_384_community_structure}
\alias{plot_384_community_structure}
\title{Plot 384 well community structure with pie chart}
\usage{
plot_384_community_structure(data, cols = dplyr::starts_with("label_"))
}
\arguments{
\item{data}{a wider data.frame giving the (relative) quantity of each species, the key column is \strong{well_position}}

\item{cols}{specify the columns, which give the quantity of two or more species}
}
\value{
a ggplot object
}
\description{
Plot 384 well community structure with pie chart
}
