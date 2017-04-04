---
exclude: true
--- 

* TOC
{:toc}

These are some really basic notes on spatial statistics, made as I read Applied Spatial Data Analysis with R, by R. S. Bivand *et al*.

## Calculating expected cases
 1. Calculate the overall rate
 1. Multiply population at geographical unit by rate to get expected cases at geographical unit (p. 314)
 
## Detecting clusters of disease

```r
library(DCluster)
```
 1. First test homogeneity of relative risks
  * Chisq test for differences between expected and observed values as above. `DCluster::achisq.test()`
  * If over-dispersed prefer Negative Binomial to Poisson dist. 
 1. Moran's I test for spatial autocorrelation based on expected cases
 1. Kulldorf's statistic - only detects single cluster in region?
