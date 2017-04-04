---
title: "Spatial statistics with R"
author: "Simon"
date: "4 April 2017"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

These are some really basic notes on spatial statistics, made as I read Applied Spatial Data Analysis with R, by R. S. Bivand *et al*.

## Calculating expected cases
 1. Calculate the overall rate
 1. Multiply population at geographical unit by rate to get expected cases at geographical unit (p. 314)
 
## Detecting clusters of disease
```{r, eval=FALSE}
library(DCluster)
```
 1. First test homogeneity of relative risks
  * Chisq test for differences between expected and observed values as above. `DCluster::achisq.test()`