## Introduction

This package contains replication functions and materials for Kim, Schneider, and Alvarez (2018) *Evaluating the Quality of Changes in Voter Registration Databases*. 

The administration of elections depends crucially upon the quality and integrity of voter registration databases. In addition, political scientists  are increasingly using these databases in their research. However, these databases are dynamic, and may be subject to external manipulation and unintentional errors. In this paper, using data from Orange County, California, we develop two methods for evaluating the quality of voter registration data as it changes over time: (1) generating audit data by repeated record linkage across periodic snapshots of a given database, and monitoring it for sudden anomalous changes; and (2) identifying duplicates via an efficient, automated duplicate detection, and tracking new duplicates and deduplication efforts over time. We show that the generated data can serve not only to evaluate voter file quality, but also as a novel source of data on election administration practices, such as implementation of election laws and administrators' interactions with other state-level agencies.

Installation can be done by following:

```
devtools::install_github("sysilviakim/voterdiffR")
```

We use CRAN package `fastLink` for record linkage. For more information, visit https://github.com/kosukeimai/fastLink. 
