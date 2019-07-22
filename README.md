# voterdiffR

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/sysilviakim/voterdiffR.svg?branch=master)](https://travis-ci.org/sysilviakim/voterdiffR)<!-- badges: end -->

## Introduction

This package contains replication functions and materials for Kim, Schneider, and Alvarez (2018) *Evaluating the Quality of Changes in Voter Registration Databases*. 

The administration of elections depends crucially upon the quality and integrity of voter registration databases. In addition, political scientists  are increasingly using these databases in their research. However, these databases are dynamic, and may be subject to external manipulation and unintentional errors. In this paper, using data from Orange County, California, we develop two methods for evaluating the quality of voter registration data as it changes over time: (1) generating audit data by repeated record linkage across periodic snapshots of a given database, and monitoring it for sudden anomalous changes; and (2) identifying duplicates via an efficient, automated duplicate detection, and tracking new duplicates and deduplication efforts over time. We show that the generated data can serve not only to evaluate voter file quality, but also as a novel source of data on election administration practices, such as implementation of election laws and administrators' interactions with other state-level agencies.

## Installation

```
devtools::install_github("sysilviakim/voterdiffR")
```

We use CRAN package `fastLink` for probabilistic record linkage. For more information on how it operates, visit https://github.com/kosukeimai/fastLink. 

## Example

(Forthcoming.)

--------------------------------------------

## OCROV application

Although the data itself cannot be released to the public for privacy concerns, the following is the series of the code using this package that we run, excluding figures/tables creation and performance assessment.

--------------------------------------------

### Setup 

The following is the minimal setup necessary for the application.

```
library(voterdiffR)
options(digits = 4, scipen = 999)

vl <- list(
  name = c("szNameLast", "szNameMiddle", "szNameFirst", "sNameSuffix"),
  addr1 = c("sHouseNum", "szStreetName", "sUnitNum"),
  addr2 = c("szSitusAddress", "szSitusCity", "sSitusZip"),
  date = c("dtBirthDate", "dtRegDate", "dtOrigRegDate", "dtLastUpdate_dt"),
  num = c("sHouseNum", "sSitusZip"),
  id = c("lVoterUniqueID", "sAffNumber"),
  mat = c("szNameLast", "szNameFirst", "dtBirthDate", "sHouseNum", "sSitusZip")
)
vl$all <- Reduce(union, vl[1:4])
```

--------------------------------------------

### Clean 

First, we clean all dataframes to be used.

```
date_df <- snapshot_list()
data(oc_colclasses)
clean_snapshot(
  date_df = date_df,
  col_classes = oc_colclasses,
  varnames = vl$all,
  date = vl$date,
  num = vl$num
)
```

--------------------------------------------

### Match

We generate the audit trail by matching periodic voter snapshots. 

```
report <- vrmatch(
  date_df,
  varnames = vl$mat,
  varnames_str = setdiff(vl$mat, union(vl$num, vl$date)),
  varnames_num = intersect(vl$mat, union(vl$num, vl$date)),
  file_type = ".fst",
  vars_change = c(vl$mat[1:3], vl$addr2[1], vl$id[1], "szPartyName"),
  exist_files = TRUE
)
```

--------------------------------------------

### Anomaly Detection

--------------------------------------------

### Duplicate Detection


