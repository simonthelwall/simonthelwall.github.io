---
output: 
  html_document:
    keep_md: true
    exclude: true
---

* TOC
{:toc}

# Set up


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

![](statistics_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

## Logistic regression


```r
data(infert)
library(broom)
m1 <- glm(case ~ education + induced, data = infert, family = "binomial")
tidy(m1, exponentiate = TRUE, conf.int = TRUE)
```

```
## # A tibble: 4 x 7
##   term             estimate std.error statistic p.value conf.low conf.high
##   <chr>               <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
## 1 (Intercept)         0.471     0.651   -1.16     0.247    0.118      1.62
## 2 education6-11yrs    1.04      0.655    0.0536   0.957    0.299      4.16
## 3 education12+ yrs    1.04      0.652    0.0631   0.950    0.303      4.16
## 4 induced             1.05      0.186    0.273    0.785    0.727      1.51
```

### Fitting an interaction

This uses data from Kirkwood and Sterne, Chapter 29, p324 onwards.


```r
oncho <- readxl::read_xls(
  "C:/Users/simon/Documents/r_stuff/kirkwood_and_sterne/oncho_ems.xls")
head(oncho)
```

```
## # A tibble: 6 x 7
##      id    mf  area agegrp   sex mfload lesions
##   <dbl> <dbl> <dbl>  <dbl> <dbl>  <dbl>   <dbl>
## 1     1     1     0      2     1      1       0
## 2     2     1     1      3     0      3       0
## 3     3     1     0      3     1      1       0
## 4     4     0     1      2     1      0       0
## 5     5     0     0      3     1      0       0
## 6     6     0     1      2     1      0       0
```

```r
oncho$agegrp <- factor(oncho$agegrp)
```


```r
m1 <- glm(mf ~ area + agegrp, data = oncho, family = "binomial")
tidy(m1, exponentiate = TRUE, conf.int = TRUE)
```

```
## # A tibble: 5 x 7
##   term        estimate std.error statistic  p.value conf.low conf.high
##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
## 1 (Intercept)    0.147     0.197     -9.74 2.02e-22   0.0992     0.215
## 2 area           3.08      0.138      8.18 2.82e-16   2.36       4.05 
## 3 agegrp1        2.60      0.222      4.30 1.70e- 5   1.69       4.04 
## 4 agegrp2        9.77      0.208     10.9  7.10e-28   6.54      14.8  
## 5 agegrp3       17.6       0.216     13.3  2.48e-40  11.7       27.2
```

The output above matches the values in table 29.3, bottom of p324. 
Now fit a model with an interaction term:


```r
m2 <- glm(mf ~ area * agegrp, data = oncho, family = "binomial")
tidy(m2, exponentiate = TRUE, conf.int = TRUE)
```

```
## # A tibble: 8 x 7
##   term         estimate std.error statistic  p.value conf.low conf.high
##   <chr>           <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
## 1 (Intercept)     0.208     0.275    -5.72  1.07e- 8    0.117     0.346
## 2 area            1.83      0.349     1.73  8.36e- 2    0.933     3.69 
## 3 agegrp1         2.12      0.375     2.00  4.57e- 2    1.02      4.48 
## 4 agegrp2         6.96      0.309     6.28  3.30e-10    3.89     13.1  
## 5 agegrp3        10.5       0.319     7.36  1.81e-13    5.74     20.2  
## 6 area:agegrp1    1.39      0.463     0.708 4.79e- 1    0.557     3.44 
## 7 area:agegrp2    1.66      0.415     1.23  2.20e- 1    0.730     3.73 
## 8 area:agegrp3    2.59      0.438     2.17  2.99e- 2    1.09      6.09
```

As decsribed in the text, the model now has eight parameters (equal to the number of area and age subgroups multiplied together).
Again, the output from this matches that in table 29.4, part b. 

As described in the text, the interaction parameter allows the effect of area to be different across the different age groups. 
To calculate the odds ratio for the effect of area in the first age group, one can do 
$\mbox{area} \times \mbox{area:factor(agegroup)1} = $
$1.83 \times 1.39 = 2.54$

And this is fine, but we want to go a bit further and do this more systematically, rather than relying on our manual calculations. 


```r
library(multcomp)
```

```
## Warning: package 'multcomp' was built under R version 3.6.1
```

```
## Warning: package 'TH.data' was built under R version 3.6.1
```

```r
summary(glht(m2, linfct = c("area + area:agegrp1 = 1")))
```

```
## 
## 	 Simultaneous Tests for General Linear Hypotheses
## 
## Fit: glm(formula = mf ~ area * agegrp, family = "binomial", data = oncho)
## 
## Linear Hypotheses:
##                          Estimate Std. Error z value Pr(>|z|)
## area + area:agegrp1 == 1   0.9307     0.3049  -0.227     0.82
## (Adjusted p values reported -- single-step method)
```

```r
summary(glht(m2, linfct = c("area + area:agegrp1 = 1", 
                            "area + area:agegrp2 = 1", "area + area:agegrp3 = 1")))
```

```
## 
## 	 Simultaneous Tests for General Linear Hypotheses
## 
## Fit: glm(formula = mf ~ area * agegrp, family = "binomial", data = oncho)
## 
## Linear Hypotheses:
##                          Estimate Std. Error z value Pr(>|z|)
## area + area:agegrp1 == 1   0.9307     0.3049  -0.227    0.994
## area + area:agegrp2 == 1   1.1121     0.2249   0.498    0.944
## area + area:agegrp3 == 1   1.5539     0.2653   2.088    0.106
## (Adjusted p values reported -- single-step method)
```

Getting the confidence intervals out is not tricky, just wrap the `glht()` in `confint()`.
Getting the exponentiated for out is tricky though.


```r
# tried converting to dataframe - won't work. Can I extract with purrr
# Also fails:
# library(purrr)
# map(t, "Estimate")
t <- glht(m2, linfct = c("area + area:agegrp1 = 1"))
s <- confint(t)
s
```

```
## 
## 	 Simultaneous Confidence Intervals
## 
## Fit: glm(formula = mf ~ area * agegrp, family = "binomial", data = oncho)
## 
## Quantile = 1.96
## 95% family-wise confidence level
##  
## 
## Linear Hypotheses:
##                          Estimate lwr    upr   
## area + area:agegrp1 == 1 0.9307   0.3332 1.5282
```

```r
names(s)
```

```
## [1] "model"       "linfct"      "rhs"         "coef"        "vcov"       
## [6] "df"          "alternative" "type"        "confint"
```

```r
s$confint
```

```
##                      Estimate       lwr      upr
## area + area:agegrp1 0.9306795 0.3331821 1.528177
## attr(,"conf.level")
## [1] 0.95
## attr(,"calpha")
## [1] 1.959964
```

```r
names(s$confint)
```

```
## NULL
```

```r
str(s$confint)
```

```
##  num [1, 1:3] 0.931 0.333 1.528
##  - attr(*, "dimnames")=List of 2
##   ..$ : chr "area + area:agegrp1"
##   ..$ : chr [1:3] "Estimate" "lwr" "upr"
##  - attr(*, "conf.level")= num 0.95
##  - attr(*, "calpha")= num 1.96
```

```r
s$confint[1:3]
```

```
## [1] 0.9306795 0.3331821 1.5281768
```

```r
exp(s$confint[1:3])
```

```
## [1] 2.536232 1.395401 4.609765
```

The exponentiated value is close enough to that calculated above and given in Kirkwood and Sterne. 

Does this work across all the combinations

```r
s <- confint(glht(m2, linfct = c("area + area:agegrp1 = 1", 
                            "area + area:agegrp2 = 1", "area + area:agegrp3 = 1")))
s
```

```
## 
## 	 Simultaneous Confidence Intervals
## 
## Fit: glm(formula = mf ~ area * agegrp, family = "binomial", data = oncho)
## 
## Quantile = 2.3876
## 95% family-wise confidence level
##  
## 
## Linear Hypotheses:
##                          Estimate lwr    upr   
## area + area:agegrp1 == 1 0.9307   0.2028 1.6585
## area + area:agegrp2 == 1 1.1121   0.5751 1.6490
## area + area:agegrp3 == 1 1.5539   0.9206 2.1873
```

```r
s$confint[1:3, ]
```

```
##                      Estimate       lwr      upr
## area + area:agegrp1 0.9306795 0.2028199 1.658539
## area + area:agegrp2 1.1120714 0.5751206 1.649022
## area + area:agegrp3 1.5539250 0.9205693 2.187281
```

```r
exp(s$confint[1:3, ])
```

```
##                     Estimate      lwr      upr
## area + area:agegrp1 2.536232 1.224852 5.251633
## area + area:agegrp2 3.040650 1.777345 5.201891
## area + area:agegrp3 4.729999 2.510719 8.910949
```

Yes, it does. 

Interactions: Done.

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

![](statistics_files/figure-html/polynomial-1.png)<!-- -->

## Quantile regression

## Ordinal regression


```r
# install.packages("ordinal")
library(ordinal)
```

```
## Warning: package 'ordinal' was built under R version 3.6.1
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
## # A tibble: 2 x 7
##   term        estimate std.error statistic  p.value conf.low conf.high
##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
## 1 (Intercept)   0.0463     0.204    -15.0  3.49e-51   0.0302    0.0674
## 2 housingpoor   2.01       0.268      2.60 9.44e- 3   1.19      3.43
```
