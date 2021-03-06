% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare.R
\name{compare}
\alias{compare}
\title{Comparing vrmatch Outputs}
\usage{
compare(m1, m2, row = "row", id = "lVoterUniqueID", vars = NULL)
}
\arguments{
\item{m1}{The first vrmatch output.}

\item{m2}{The second vrmatch output.}

\item{row}{Temporary row field name.}

\item{id}{Internal ID variable.}

\item{vars}{Variables to show in set difference dataframes.
Defaults to all common variable names.}
}
\value{
List of set difference dataframes and their number of rows.
}
\description{
This function takes two `vrmatch` outputs and compares their differences
by returning the set difference of matched outputs between them excluding
ID matches. The assumption is that these are matches on the same datasets
with different parameter selection, match-variable selection, or random
sample selection.
}
