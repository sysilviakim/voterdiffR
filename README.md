# voterdiffR

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/sysilviakim/voterdiffR.svg?branch=master)](https://travis-ci.org/sysilviakim/voterdiffR)<!-- badges: end -->

## Introduction

This package contains replication functions and materials for Kim,  Seo-young  Silvia,  Spencer  Schneider,  and  R.  Michael  Alvarez.  2019.  "Evaluating  the Quality of Changes in Voter Registration Databases." American Politics Research. Article first published online: Sep 9, 2019. DOI: 10.1177/1532673X19.

> The administration of elections depends crucially upon the quality and integrity of voter registration databases. In addition, political scientists are increasingly using these databases in their research. However, these databases are dynamic and may be subject to external manipulation and unintentional errors. In this article, using data from Orange County, California, we develop two methods for evaluating the quality of voter registration data as it changes over time: (a) generating audit data by repeated record linkage across periodic snapshots of a given database and monitoring it for sudden anomalous changes and (b) identifying duplicates via an efficient, automated duplicate detection, and tracking new duplicates and deduplication efforts over time. We show that the generated data can serve not only to evaluate voter file quality and election integrity but also as a novel source of data on election administration practices.

## Installation

```
devtools::install_github("sysilviakim/voterdiffR")
```

We use CRAN package `fastLink` for probabilistic record linkage. For more information on how it operates, visit https://github.com/kosukeimai/fastLink.

--------------------------------------------

## OCROV application

Although the raw voter files cannot be released to the public for privacy concerns, the code used to generate the results of the paper is available on the project repository at [monitoringtheelection/auditingVR](https://github.com/monitoringtheelection/auditingVR), as well as the intermediate outputs such as the audit data. 

Please stay tuned for examples, vignettes, and detailed instructions on the usage! I welcome issues and pull requests. Eventually, I will be submitting this to CRAN as well.


