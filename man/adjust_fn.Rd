% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adjust_fn.R
\name{adjust_fn}
\alias{adjust_fn}
\title{Correct False Negatives in a vrmatch Output for All Snapshots}
\usage{
adjust_fn(match, fn_ids = c("lVoterUniqueID", "sAffNumber"))
}
\arguments{
\item{match}{The vrmatch output to correct false negatives.}

\item{fn_ids}{Voter IDs used in detecting and correcting false negatives.}
}
\value{
Corrected vrmatch output.
}
\description{
This function takes the `vrmatch` output and using a voter ID variable,
re-matches the nonmatches that have the same ID back as a match---i.e.,
correcting the false negatives. This is sometimes necessary for real
applications, especially when you do not wish to exclude ID matches
from probabilistic record linkage.
}
\details{
Note that this function is only relevant when using `vrmatch`
when the reference ID was not used to exclude exact matches. In addition,
after the correction, the EM object and et cetera from fastLink has
not been corrected accordingly and should not be used in inference.

Also note that this assumes that if a voter ID matches, it is never a false
positive, and that there are no duplicates by `fn_ids`.
}
