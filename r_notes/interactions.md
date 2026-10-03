# Interactions in R


## Background

Calculating stratum specific odds ratios in R has been really difficult
for a long time. It pains me to say it but Stata has historically done a
better job with the `lincom` command. However, I’ve finally found the
excellent [marginaleffects](https://marginaleffects.com/) package. This
has simple methods for calculating stratum-specific odds ratios.

## Data and methods

To ensure that marginaleffects is producing the outputs that I expect,
I’m going to replicate the analyses on pages 323 to 327 of Kirkwood and
Sterne’s Essential Medical Statistics. I’m using the oncho_ems data set
that can be downloaded
[here](https://resources.learning.wiley.com/isbn/9781444392845).

These data give 1302 observations on the presence or absence of
microfilaria in individuals as a binary variable (`mf`), area of
residence (0, 1 or 2) and age group (0, 1, 2 or 3). mf is the outcome of
interest and area of residence (rainforest or savannah) and age group
are factors that could affect the odds of the outcome.

``` r
head(dat)
```

      id mf area agegrp sex mfload lesions
    1  1  1    0      2   1      1       0
    2  2  1    1      3   0      3       0
    3  3  1    0      3   1      1       0
    4  4  0    1      2   1      0       0
    5  5  0    0      3   1      0       0
    6  6  0    1      2   1      0       0

I’m going to use the Broom package to get tidy outputs from a regression
model and marginaleffects to get stratum-specific odds ratios.

``` r
library(broom)
library(marginaleffects)
```

## Replication of the model with no interaction

This is quite straightforward, a simple logistic regression model:

``` r
m1 <- glm(mf ~ area + factor(agegrp), data = dat, family = binomial)
tidy(m1, exponentiate = TRUE, conf.int = TRUE)
```

    # A tibble: 5 × 7
      term            estimate std.error statistic  p.value conf.low conf.high
      <chr>              <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
    1 (Intercept)        0.147     0.197     -9.74 2.02e-22   0.0992     0.215
    2 area               3.08      0.138      8.18 2.82e-16   2.36       4.05 
    3 factor(agegrp)1    2.60      0.222      4.30 1.70e- 5   1.69       4.04 
    4 factor(agegrp)2    9.77      0.208     10.9  7.10e-28   6.54      14.8  
    5 factor(agegrp)3   17.6       0.216     13.3  2.48e-40  11.7       27.2  

which matches the output in table 29.3(b), p324.

## Replication of the model with an interaction between the two exposures

The above analysis assumes that the effect of age is the same for each
of the different areas. This may not be true, so we need to specify a
different model to allow the effect of age to vary according to area

``` r
m2 <- glm(mf ~ area * factor(agegrp), data = dat, family = binomial) 
tidy(m2, exponentiate = TRUE, conf.int = TRUE)
```

    # A tibble: 8 × 7
      term                 estimate std.error statistic  p.value conf.low conf.high
      <chr>                   <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
    1 (Intercept)             0.208     0.275    -5.72  1.07e- 8    0.117     0.346
    2 area                    1.83      0.349     1.73  8.36e- 2    0.933     3.69 
    3 factor(agegrp)1         2.12      0.375     2.00  4.57e- 2    1.02      4.48 
    4 factor(agegrp)2         6.96      0.309     6.28  3.30e-10    3.89     13.1  
    5 factor(agegrp)3        10.5       0.319     7.36  1.81e-13    5.74     20.2  
    6 area:factor(agegrp)1    1.39      0.463     0.708 4.79e- 1    0.557     3.44 
    7 area:factor(agegrp)2    1.66      0.415     1.23  2.20e- 1    0.730     3.73 
    8 area:factor(agegrp)3    2.59      0.438     2.17  2.99e- 2    1.09      6.09 

In this output, there are now three extra estimates, we can use these to
calculate stratum-specific odds ratios for the effect of area on mf
infection. At the bottom of page 3 it works this for the effect of area
in age group 1:

> OR for area in age group 1 = Area x Area.Agegrp(1)
>
> = 1.8275 x 1.3878 = 2.5362

Allowing for some differences in rounding, this matches our output
above, the OR for area in our tidied output is 1.83 and the OR for the
combination of area and age group 1 (`area:factor(agegrp)1`) is 1.39.

So, we can use these values to calculate stratum-specific odds ratios
one-by-one. But, I don’t want to treat R like a hand-held calculator, I
want to easily make a table of the stratum-specific odds ratios that I
can easily plug into a manuscript or report for publication. This is the
step that has been tricky until I learned to do it with marginaleffects.

``` r
comparisons(m2, variables = "area", newdata = datagrid(agegrp = c(0,1,2,3)), transform = "exp", comparison = "lnor")
```


     agegrp Estimate Pr(>|z|)    S 2.5 % 97.5 %
          0     1.83  0.08363  3.6 0.923   3.62
          1     2.54  0.00227  8.8 1.395   4.61
          2     3.04  < 0.001 20.3 1.957   4.72
          3     4.73  < 0.001 27.7 2.812   7.96

    Term: area
    Type: response
    Comparison: ln(odds(1) / odds(0))

This output replicates the stratum-specific odds ratios for the effect
of area, at each age group, as given at the bottom of p326:

> OR for area in age group 2 = Area x Area.Agegrp(2)
>
> = 1.8275 x 1.6638 = 3.0406
>
> OR for area in age group 2 = Area x Area.Agegrp(3)
>
> = 1.8275 x 2.5881 = 4.7300

Page 327 goes on and does the equivalent to calculate the odds ratios
for age group for the rainforest area

> OR for age group 1 in rainforest areas = Agegrp(1) x Area.Agegrp(1) =
> 2.1175 x 1.3878 = 2.9386

``` r
comparisons(m2, variables = "agegrp", newdata = datagrid(area = c(0, 1)), transform = "exp", comparison = "lnor")
```

    Warning: The `agegrp` variable is treated as a categorical (factor) variable,
    but the original data is of class integer. It is safer and faster to convert
    such variables to factor before fitting the model and calling a
    `marginaleffects` function. This warning appears once per session.


                  Contrast area Estimate Pr(>|z|)    S 2.5 % 97.5 %
     ln(odds(1) / odds(0))    0     2.12   0.0457  4.5  1.01   4.42
     ln(odds(1) / odds(0))    1     2.94   <0.001 13.8  1.73   5.00
     ln(odds(2) / odds(0))    0     6.96   <0.001 31.5  3.80  12.76
     ln(odds(2) / odds(0))    1    11.59   <0.001 60.0  6.73  19.94
     ln(odds(3) / odds(0))    0    10.50   <0.001 42.3  5.61  19.64
     ln(odds(3) / odds(0))    1    27.18   <0.001 91.4 15.10  48.90

    Term: agegrp
    Type: response

This is slightly different from the first output from `comparisons`. We
now have an extra column `Contrast`. This is because, when looking at
the age-specific odds ratios for the effect of area there were only two
levels of area, so only one odds ratio for the effect of area to
calculate. Now that we are looking at it the other way around, looking
at the area specific odds ratios of age, we have three levels of age to
calculate for each area. The output still matches the value given at the
top of p327 in Kirkwood and Sterne, but we need to hunt to find it a
bit. The top of 327 gives the area-specific OR for age group 1, compared
to the baseline age group (0) for the rainforest area (1). So, in the
`comparisons` output, we find the `Contrast` ln(odds(1) / odds(0)) and
`area` 1, for which the estimate is 2.94, matching Kirkwood and Sterne’s
estimate of 2.9386.

## Further reading

There’s loads more to the `marginaleffects` package, including using it
for producing outputs for splines and non-linear relationships between
variables. Further reading should include [the marginaleffects
website](https://marginaleffects.com/) and Andrew Heiss’ blog post
[marginalia](https://www.andrewheiss.com/blog/2022/05/20/marginalia/).
