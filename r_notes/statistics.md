---
    exclude: true
---


* TOC
{:toc}

# Statistics


```r
library(ggplot2)
# library(GGally)
data("mtcars")
# ggpairs(data = mtcars)
```

## Simple stats

### Percentiles


```r
data(mtcars)
quantile(mtcars$mpg, c(0.25, 0.5, 0.75), na.rm = TRUE)
```

```
##    25%    50%    75% 
## 15.425 19.200 22.800
```

### Chi-sq test

Make a contingency table and nest inside `chisq.test()`


```r
data(infert) # built in fertility data
head(infert)
```

```
##   education age parity induced case spontaneous stratum pooled.stratum
## 1    0-5yrs  26      6       1    1           2       1              3
## 2    0-5yrs  42      1       1    1           0       2              1
## 3    0-5yrs  39      6       2    1           0       3              4
## 4    0-5yrs  34      4       2    1           0       4              2
## 5   6-11yrs  35      3       1    1           1       5             32
## 6   6-11yrs  36      4       2    1           1       6             36
```


```r
chisq.test(table(infert$education, infert$case))
```

```
## Warning in chisq.test(table(infert$education, infert$case)): Chi-squared
## approximation may be incorrect
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  table(infert$education, infert$case)
## X-squared = 0.0022896, df = 2, p-value = 0.9989
```

The Chi square test results in a warning message because the cell case-0-5yrs is &lt; 5. Best to check with Fisher's exact test:


```r
fisher.test(table(infert$education, infert$case))
```

```
## 
## 	Fisher's Exact Test for Count Data
## 
## data:  table(infert$education, infert$case)
## p-value = 1
## alternative hypothesis: two.sided
```

It's possible to get the observed and expected cell counts from the output of a `chisq.test()`:


```r
chisq.test(table(infert$education, infert$case))$expected
```

```
## Warning in chisq.test(table(infert$education, infert$case)): Chi-squared
## approximation may be incorrect
```

```
##          
##                   0         1
##   0-5yrs   7.983871  4.016129
##   6-11yrs 79.838710 40.161290
##   12+ yrs 77.177419 38.822581
```


## Mantel-Haenzsel

It pains me to say it, but I don't think the output from R for MH analysis is as neat as that from Stata. 
The package epicalc had a reasonable function, `mhor()`, but has been removed from CRAN.

Base-r has `mantelhaen.test()` which

> Performs a Cochran-Mantel-Haenszel chi-squared test of the null that two nominal variables are conditionally independent in each stratum, assuming that there is no three-way interaction.

However,

> Currently, no inference on homogeneity of the odds ratios is performed.

i.e. there one can't use `mantelhaen.test()` to identify interaction, and the p-values obtained refer to the pooled odds ratio not being equal to one.


```r
library(epicalc)
data(Oswego)
mhor(Oswego$ill, Oswego$chocolate, Oswego$sex)
```

would return

```
    Stratified analysis by  Var3 
                    OR lower lim. upper lim. P value
    Var3 F       0.417     0.0617       2.06  0.3137
    Var3 M       0.331     0.0512       1.83  0.2635
    M-H combined 0.364     0.1258       1.05  0.0611

    M-H Chi2(1) = 3.51 , P value = 0.061 
    Homogeneity test, chi-squared 1 d.f. = 0.05 , P value = 0.827 
```

## Linear regression


```r
m1 <- lm(mpg ~ wt, data = mtcars)

# Multiple, with ordered categorical varliable
m2 <- lm(mpg ~ wt + factor(cyl), data = mtcars)

# check the results
summary(m1)
```

```
## 
## Call:
## lm(formula = mpg ~ wt, data = mtcars)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.5432 -2.3647 -0.1252  1.4096  6.8727 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  37.2851     1.8776  19.858  < 2e-16 ***
## wt           -5.3445     0.5591  -9.559 1.29e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.046 on 30 degrees of freedom
## Multiple R-squared:  0.7528,	Adjusted R-squared:  0.7446 
## F-statistic: 91.38 on 1 and 30 DF,  p-value: 1.294e-10
```


```r
# and get confidence intervals
confint(m1)
```

```
##                 2.5 %    97.5 %
## (Intercept) 33.450500 41.119753
## wt          -6.486308 -4.202635
```

The summary provide R2 which gives coefficient of determination, the amount of variance in data explained by regressors. Adjusted R2, adjusts this for number of predictor variables included in model. It should be on the scale 0 to 1. However, plot of fitted values (x axis) against residuals more useful (y axis).

### Departures from linearity


```r
library(ggplot2)
test.data <- data.frame(x = seq(1,100,1), y = seq(1,100,1))
test.data$z <- test.data$x^2

qplot(x=x, y=y, data = test.data)
```

![](statistics2_files/figure-html/unnamed-chunk-9-1.png)<!-- -->



## Polynomial regression


```r
# linear first
m1 <- lm(wt ~ disp, data = mtcars)
m2 <- lm(wt ~ poly(disp, 2), data = mtcars)
m3 <- lm(wt ~ poly(disp, 3), data = mtcars)
anova(m1, m2)
```

```
## Analysis of Variance Table
## 
## Model 1: wt ~ disp
## Model 2: wt ~ poly(disp, 2)
##   Res.Df    RSS Df Sum of Sq      F Pr(>F)
## 1     30 6.2768                           
## 2     29 6.2748  1 0.0020473 0.0095 0.9232
```

```r
anova(m2, m3)
```

```
## Analysis of Variance Table
## 
## Model 1: wt ~ poly(disp, 2)
## Model 2: wt ~ poly(disp, 3)
##   Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
## 1     29 6.2748                                  
## 2     28 3.0471  1    3.2277 29.659 8.205e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
anova(m1, m3)
```

```
## Analysis of Variance Table
## 
## Model 1: wt ~ disp
## Model 2: wt ~ poly(disp, 3)
##   Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
## 1     30 6.2768                                  
## 2     28 3.0471  2    3.2297 14.839 4.037e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
p <- ggplot(data = mtcars, aes(x = disp, y = wt)) + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 3)) + 
  geom_point()
p
```

![](statistics2_files/figure-html/polynomial-1.png)<!-- -->

## Quantile regression

## Ordinal regression


```r
# install.packages("ordinal")
library(ordinal)
```

```
## Warning: package 'ordinal' was built under R version 3.5.1
```

```r
data(wine)
head(wine)
```

```
##   response rating temp contact bottle judge
## 1       36      2 cold      no      1     1
## 2       48      3 cold      no      2     1
## 3       47      3 cold     yes      3     1
## 4       67      4 cold     yes      4     1
## 5       77      4 warm      no      5     1
## 6       60      4 warm      no      6     1
```

```r
m1 <- clm(rating ~ temp, data = wine)
m2 <- clm(rating ~ temp + contact, data = wine)

anova(m1, m2)
```

```
## Likelihood ratio tests of cumulative link models:
##  
##    formula:                link: threshold:
## m1 rating ~ temp           logit flexible  
## m2 rating ~ temp + contact logit flexible  
## 
##    no.par    AIC  logLik LR.stat df Pr(>Chisq)    
## m1      5 194.03 -92.013                          
## m2      6 184.98 -86.492  11.043  1  0.0008902 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

The anova above tests the null hypothesis that there is no difference in fit between the two models above. 
There is strong evidence against the null hypothesis and so it should be rejected. 

The summary gives the un-exponentiated coefficients for the two variables. 


```r
summary(m2)
```

```
## formula: rating ~ temp + contact
## data:    wine
## 
##  link  threshold nobs logLik AIC    niter max.grad cond.H 
##  logit flexible  72   -86.49 184.98 6(0)  4.02e-12 2.7e+01
## 
## Coefficients:
##            Estimate Std. Error z value Pr(>|z|)    
## tempwarm     2.5031     0.5287   4.735 2.19e-06 ***
## contactyes   1.5278     0.4766   3.205  0.00135 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Threshold coefficients:
##     Estimate Std. Error z value
## 1|2  -1.3444     0.5171  -2.600
## 2|3   1.2508     0.4379   2.857
## 3|4   3.4669     0.5978   5.800
## 4|5   5.0064     0.7309   6.850
```

I think verbalising the interpretation of the summary output is not easy. 
Here's my understanding. 
A wine at baseline will have intercept odds of a rating. 
A wine that is warm will have an odds of a 
So, the odds of a wine being at the next level up (out of a 1-5 rating) increases by exp(2.5) [12.2203428] for warm wines compared to cold. 

## Poisson regression

Poisson regression can be used to calculate incident rate ratios using either grouped data. 
This is an example from Kirkwood and Sterne Medical Statistics (2nd Ed), p 241. 
It compares numbers of infections among children in either good or poor quality housing. 
The manual calculations given in Kirkwood and Sterne give a rate ratio of 2.01 (95%CI 1.19-3.39)


```r
lrti <- data.frame(housing = c("poor", "good"), n_lrti = c(33, 24), pyar = c(355, 518), 
                   stringsAsFactors = FALSE)
lrti
```

```
##   housing n_lrti pyar
## 1    poor     33  355
## 2    good     24  518
```


```r
library(broom)
m1 <- glm(n_lrti ~ housing + offset(log(pyar)), family = "poisson", data = lrti)
tidy(m1, exponentiate = TRUE, conf.int = TRUE)
```

```
##          term   estimate std.error  statistic      p.value   conf.low
## 1 (Intercept) 0.04633205 0.2041241 -15.049280 3.490122e-51 0.03017981
## 2 housingpoor 2.00633803 0.2682717   2.595545 9.444108e-03 1.19097485
##    conf.high
## 1 0.06741652
## 2 3.42986561
```
