# statistics



# Polynomial regression


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
