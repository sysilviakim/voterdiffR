## Introduction

This package contains replication functions and materials for Kim, Schneider, and Alvarez (2018) *Evaluating the Quality of Changes in Voter Registration Databases*. 

The administration of elections depends crucially upon the quality and integrity of voter registration databases. In addition, political scientists  are increasingly using these databases in their research. However, these databases are dynamic, and may be subject to external manipulation and unintentional errors. In this paper, using data from Orange County, California, we develop two methods for evaluating the quality of voter registration data as it changes over time: (1) generating audit data by repeated record linkage across periodic snapshots of a given database, and monitoring it for sudden anomalous changes; and (2) identifying duplicates via an efficient, automated duplicate detection, and tracking new duplicates and deduplication efforts over time. We show that the generated data can serve not only to evaluate voter file quality, but also as a novel source of data on election administration practices, such as implementation of election laws and administrators' interactions with other state-level agencies.

## Installation

```
devtools::install_github("sysilviakim/voterdiffR")
```

We use CRAN package `fastLink` for record linkage. For more information, visit https://github.com/kosukeimai/fastLink. 

## Example

(Forthcoming.)

## OCROV application

Although the data itself cannot be released to the public for privacy concerns, the following is the series of the code using this package that we run.

```
## Setup
library(voterdiffR)
options(digits = 4, scipen = 999)

non_addr <- c("szNameLast", "szNameMiddle", "szNameFirst", "sNameSuffix")
addr <- c("sHouseNum", "szStreetName", "sUnitNum", 
          "szSitusCity", "sSitusZip", "szSitusAddress")
vars_date <- c("dtBirthDate", "dtRegDate", "dtOrigRegDate", "dtLastUpdate_dt")
vars_num <- c("sHouseNum", "sSitusZip")
num_match <- c(vars_num, vars_date)
vars_all <- unique(c(non_addr, addr, vars_date))
vars_id <- c("lVoterUniqueID", "sAffNumber")

## Clean
date_df <- snapshot_list()
data(oc_colclasses)
clean_snapshot(
  date_df = date_df, col_classes = oc_colclasses, 
  varnames = vars_all, varnames_date = vars_date, varnames_num = vars_num
)

## Match

## Anomaly Detection

## Duplicate Detection


```
