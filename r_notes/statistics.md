---
exclude: true
---

Statistics
================

* TOC
{:toc}

Simple stats
------------

### Percentiles

``` r
data(mtcars)
quantile(mtcars$mpg, c(0.25, 0.5, 0.75), na.rm = TRUE)
```

    ##    25%    50%    75% 
    ## 15.425 19.200 22.800

### Chi-sq test

Make a contingency table and nest inside `chisq.test()`

``` r
data(infert) # built in fertility data
head(infert)
```

    ##   education age parity induced case spontaneous stratum pooled.stratum
    ## 1    0-5yrs  26      6       1    1           2       1              3
    ## 2    0-5yrs  42      1       1    1           0       2              1
    ## 3    0-5yrs  39      6       2    1           0       3              4
    ## 4    0-5yrs  34      4       2    1           0       4              2
    ## 5   6-11yrs  35      3       1    1           1       5             32
    ## 6   6-11yrs  36      4       2    1           1       6             36

``` r
table(infert$education, infert$case)#
```

    ##          
    ##            0  1
    ##   0-5yrs   8  4
    ##   6-11yrs 80 40
    ##   12+ yrs 77 39

``` r
chisq.test(table(infert$education, infert$case))
```

    ## Warning in chisq.test(table(infert$education, infert$case)): Chi-squared
    ## approximation may be incorrect

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  table(infert$education, infert$case)
    ## X-squared = 0.0022896, df = 2, p-value = 0.9989

The Chi square test results in a warning message because the cell case-0-5yrs is &lt; 5. Best to check with Fisher's exact test:

``` r
fisher.test(table(infert$education, infert$case))
```

    ## 
    ##  Fisher's Exact Test for Count Data
    ## 
    ## data:  table(infert$education, infert$case)
    ## p-value = 1
    ## alternative hypothesis: two.sided

It's possible to get the observed and expected cell counts from the output of a `chisq.test()`:

``` r
chisq.test(table(infert$education, infert$case))$expected
```

    ## Warning in chisq.test(table(infert$education, infert$case)): Chi-squared
    ## approximation may be incorrect

    ##          
    ##                   0         1
    ##   0-5yrs   7.983871  4.016129
    ##   6-11yrs 79.838710 40.161290
    ##   12+ yrs 77.177419 38.822581

Mantel-Haenzsel
---------------

It pains me to say it, but I don't think the output from R for MH analysis is as neat as that from Stata. The package epicalc had a reasonable function, `mhor()`, but has been removed from CRAN.

Base-r has `mantelhaen.test()` which 
> Performs a Cochran-Mantel-Haenszel chi-squared test of the null that two nominal variables are conditionally independent in each stratum, assuming that there is no three-way interaction.

However, 
> Currently, no inference on homogeneity of the odds ratios is performed.

i.e. there one can't use `mantelhaen.test()` to identify interaction, and the p-values obtained refer to the pooled odds ratio not being equal to one.

``` r
library(epicalc)
data(Oswego)
mhor(Oswego$ill, Oswego$chocolate, Oswego$sex)
```

would return

    Stratified analysis by  Var3 
                    OR lower lim. upper lim. P value
    Var3 F       0.417     0.0617       2.06  0.3137
    Var3 M       0.331     0.0512       1.83  0.2635
    M-H combined 0.364     0.1258       1.05  0.0611

    M-H Chi2(1) = 3.51 , P value = 0.061 
    Homogeneity test, chi-squared 1 d.f. = 0.05 , P value = 0.827
