% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/changes_extract.R
\name{changes_extract}
\alias{changes_extract}
\title{Changes by Field}
\usage{
changes_extract(match, varnames = NULL, nrow = "nrow")
}
\arguments{
\item{match}{List of matched output from vrmatch.}

\item{varnames}{Variables to track changes.
Defaults to all variables.}

\item{nrow}{Name of list element which will contain the number of rows
of the input list dataframes.}
}
\value{
List of dataframes that contain changes in each specified field.
}
\description{
This function takes a matched output and outputs dataframes that have
changed values in user-specified fields.
}
