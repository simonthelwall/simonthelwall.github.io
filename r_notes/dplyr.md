---
    toc: true
    toc_float: true
    exclude: true
---

  
# dplyr and tidyr
  
dplyr introduces a consistent grammar of data manipulation for R. 
The significance of this should not be underestimated. 
It makes the standard data manipulation that all data analysts need to do so much easier. 
A particularly nice feature is the ability to connect a series of commands in the same way as pipes in Unix systems. 
Rather than the vertical bar `|`, the symbol is `%>%`.
The pipe takes input, commonly a dataframe from the left-hand side and passes it to functions on the right-hand side. 

As well as dplyr, Hadley has introduced another new package for reshaping: tidyr. 
Together with ggplot2, lubridate, stringr and others they form the 'tidyverse'.

* TOC
{:toc}

## The basics

dplyr has, as described in the introductory documentation, "five basic data manipulation verbs that work on a single table: filter(), arrange(), select(), mutate() and summarise()."

## Creating new variables

New variables are created using the `mutate()` verb, one takes a dataframe, passes it in a pipe to `mutate` and create a new variable in that function call.


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(stringr)
library(tidyr)

data("mtcars")

mtcars <- mtcars %>% 
  mutate(cyl_2 = cyl + 1, 
         mpg_2 = mpg * 2)

head(mtcars)
```

```
##    mpg cyl disp  hp drat    wt  qsec vs am gear carb cyl_2 mpg_2
## 1 21.0   6  160 110 3.90 2.620 16.46  0  1    4    4     7  42.0
## 2 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4     7  42.0
## 3 22.8   4  108  93 3.85 2.320 18.61  1  1    4    1     5  45.6
## 4 21.4   6  258 110 3.08 3.215 19.44  1  0    3    1     7  42.8
## 5 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2     9  37.4
## 6 18.1   6  225 105 2.76 3.460 20.22  1  0    3    1     7  36.2
```

### Detecting strings

I have a vector in a dataframe and I wish to determine whether I can detect any one of a series of strings. 
I would normally use str_detect for this, but it doesn't work without a little trickery. 
The `paste(something, collapse = "|")` is the important bit here. 


```r
test_data <- data.frame(item = c("Apple", "Bear", "Orange", "Pear", "Two Apples"), 
                        stringsAsFactors = FALSE)
fruit <- c("Apple", "Orange", "Pear")
test_data
```

```
##         item
## 1      Apple
## 2       Bear
## 3     Orange
## 4       Pear
## 5 Two Apples
```

```r
test_data <- test_data %>%  
  mutate(is.fruit = str_detect(item, paste(fruit, collapse = "|")))
test_data
```

```
##         item is.fruit
## 1      Apple     TRUE
## 2       Bear    FALSE
## 3     Orange     TRUE
## 4       Pear     TRUE
## 5 Two Apples     TRUE
```

### Replacing values with dplyr functions

Can use `recode()` or `case_when()`. 
`recode()` is a vectorised switch. 
It's important to note that the data types must be the same when using `recode()`, and that a double is not the same type as an integer, which seems obvious when typed. 
If they are not the same type, then unspecified replacements get NA values. 


```r
mtcars %>% 
  mutate(new_cyl = recode(cyl, `6` = 16, `8` = 200) ) %>% 
  select(cyl, new_cyl) %>% 
  tbl_df() %>%
  head()
```

```
## # A tibble: 6 x 2
##     cyl new_cyl
##   <dbl>   <dbl>
## 1     6      16
## 2     6      16
## 3     4       4
## 4     6      16
## 5     8     200
## 6     6      16
```

`case_when()` vectorises multiple `if()` `else()` statements, and it's a lot neater than writing nested `ifelse()` statements. 
`case_when()` can be used inside a `mutate()` call, or outside.

There are times when the column needs to be specified with a `.$` in front but the situations in which this is required have changed as the package has developed.



```r
mtcars %>% 
mutate(
  new_cyl = case_when(cyl == 4 ~ "Four", 
                      cyl == 6 ~ "Six", 
                      cyl == 8 ~ "Eight"
                      )) %>%
  select(cyl, new_cyl) %>% 
  head()
```

```
##   cyl new_cyl
## 1   6     Six
## 2   6     Six
## 3   4    Four
## 4   6     Six
## 5   8   Eight
## 6   6     Six
```

There's an important gotcha here though. 
One has to make sure that each possible outcome is specified, otherwise unspecified levels are replaced with `<NA>` values.
One can work around this as in the second example. 
I haven't yet tested what will happen if numeric output is required, I expect that one would need `TRUE ~ as.double(.$cyl)`.
For numeric columns, it's also worth remembering that integer data types are not the same as other numeric data types such as doubles and so `as.numeric()` may still return `<NA>` values.


```r
mtcars %>% 
  mutate( cyl2 = case_when(
    cyl == 4 ~ "four"
  )
  ) %>% 
  select(cyl, cyl2) %>% 
  head()
```

```
##   cyl cyl2
## 1   6 <NA>
## 2   6 <NA>
## 3   4 four
## 4   6 <NA>
## 5   8 <NA>
## 6   6 <NA>
```

```r
mtcars %>% 
  mutate( cyl2 = case_when(
    cyl == 4 ~ "four", 
    TRUE ~ as.character(cyl)
  )
  ) %>% 
  select(cyl, cyl2) %>% 
  head()
```

```
##   cyl cyl2
## 1   6    6
## 2   6    6
## 3   4 four
## 4   6    6
## 5   8    8
## 6   6    6
```

This also works for detecting strings in columns. 
It's a bit weird giving the `TRUE ~ 0` for the non-true cases. 
I tried `FALSE ~ 0` and that doesn't work. 


```r
data(mtcars)
mtcars$name <- row.names(mtcars)
names_to_detect <- c("Mazda", "Datsun")

mtcars %>% 
  mutate(has_name = case_when(
    str_detect(name, paste(names_to_detect, collapse = "|")) == TRUE ~ 1, 
    TRUE ~ 0)
  )%>% 
  select(name, has_name) %>% 
  head()
```

```
##                name has_name
## 1         Mazda RX4        1
## 2     Mazda RX4 Wag        1
## 3        Datsun 710        1
## 4    Hornet 4 Drive        0
## 5 Hornet Sportabout        0
## 6           Valiant        0
```

## Working on multiple columns at once

`mutate_all` or `summarise_all` will affect all variables in the dataframe in the same way. 
`mutate_at` will mutate only the specified columns


```r
library("lubridate")
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
dat <- data.frame(x = c(1,2,3), 
  y_date = c("01/01/2017", "02/01/2017", "03/01/2017"), stringsAsFactors = FALSE)
dat <- dat %>% mutate_at(vars(contains("date")), funs(lubridate::dmy))
dat
```

```
##   x     y_date
## 1 1 2017-01-01
## 2 2 2017-01-02
## 3 3 2017-01-03
```

summarise_at allows you to specify a set of columns one which to work.


```r
data("mtcars")
mtcars %>% summarise_at(vars(mpg), funs(sum)) 
```

```
##     mpg
## 1 642.9
```

```r
mtcars %>% summarise_at(vars(-mpg, - contains("cyl")), funs(sum))
```

```
##     disp   hp   drat      wt   qsec vs am gear carb
## 1 7383.1 4694 115.09 102.952 571.16 14 13  118   90
```

```r
mtcars %>% summarise_at(vars(matches("mpg")), funs(sum))
```

```
##     mpg
## 1 642.9
```

```r
my_vars <- c("mpg", "cyl")
mtcars %>% summarise_at(vars(my_vars), funs(sum))
```

```
##     mpg cyl
## 1 642.9 198
```

## Subsetting (aka filtering)

Much less verbose than `test[test$var1 == "something",]`

```r
filter(test, var1 == "something")
```
       
But getting unique combinations is a little more awkward. 
`group_by()` is covered a little later.


```r
test <- data.frame(id = c(1,1,1,2,2),
org = c("apple", "apple", "bear", "orange", "pear"),
test = c("S", "R","S", "R", "S"))
test
```

```
##   id    org test
## 1  1  apple    S
## 2  1  apple    R
## 3  1   bear    S
## 4  2 orange    R
## 5  2   pear    S
```

```r
out <- test %>% group_by(id, org) %>% 
  filter(row_number() == 1)
out
```

```
## # A tibble: 4 x 3
## # Groups:   id, org [4]
##      id org    test 
##   <dbl> <fct>  <fct>
## 1     1 apple  S    
## 2     1 bear   S    
## 3     2 orange R    
## 4     2 pear   S
```

## Summarising data

Uses the `summarise()` function. `n()` is a function introduced in dplyr and produces a count of the number of rows in the data set. 
                   

```r
library(binom)

test <- data.frame(group = c(rep("A", 10), rep("B", 10)),
outcome = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0))
head(test)
```

```
##   group outcome
## 1     A       1
## 2     A       0
## 3     A       0
## 4     A       0
## 5     A       0
## 6     A       0
```

```r
table(test$group, test$outcome, dnn = c("Group", "Outcome"))
```

```
##      Outcome
## Group 0 1
##     A 9 1
##     B 8 2
```

```r
out <-test %>% group_by(group) %>% 
  summarise(n = n(), successes = sum(outcome)) %>% 
  mutate(pc.succ = round(binom.confint(successes, n, 
                                       methods = "exact")[[4]]*100, 2),
         pc.lci = round(binom.confint(successes, n, 
                                      methods = "exact")[[5]]*100, 2),
         pc.uci = round(binom.confint(successes, n, 
                                      methods = "exact")[[6]]*100, 2)
            )

out
```

```
## # A tibble: 2 x 6
##   group     n successes pc.succ pc.lci pc.uci
##   <fct> <int>     <dbl>   <dbl>  <dbl>  <dbl>
## 1 A        10         1      10   0.25   44.5
## 2 B        10         2      20   2.52   55.6
```
                   
Outside dplyr I would write `binom.confint(x,n,methods = "exact")$mean`. 
This doesn't work in dplyr and returns the error `"Error in binom.confint(c(6928L, 5704L, 4746L, 5166L, 6779L, 6275L, 4832L,  : 
                                                                                                 invalid subscript type 'closure' "`
However, indexing the columns does work. 
Also, wrapping the `round()` and `binom.confint()` in a function would improve the readability of the process. 

### Passing arguments to `summarise_all`

For example, you might want to specify removal of NA values in a sum or mean function. 
You can do this by


```r
data(mtcars)
summarise_all(mtcars, funs(sum(., na.rm = TRUE)))
```
## Grouped operations

Grouping performs operations by levels of the grouping variable or variables. 
For example, I might want to get the mean mpg for each value of cylinder in mtcars:


```r
mtcars %>% group_by(cyl) %>% 
  mutate(grouped_mpg = mean(mpg)) %>% 
  select(mpg, cyl, grouped_mpg) %>% 
  head()
```

```
## # A tibble: 6 x 3
## # Groups:   cyl [3]
##     mpg   cyl grouped_mpg
##   <dbl> <dbl>       <dbl>
## 1  21       6        19.7
## 2  21       6        19.7
## 3  22.8     4        26.7
## 4  21.4     6        19.7
## 5  18.7     8        15.1
## 6  18.1     6        19.7
```

### Adding a group id

Sometimes one might want to create an integer group id. 
i.e. for all rows with the same groupings, these will have the same id. 
`group_indices()` goes some of the way to resolve this by creating a vector of integers corresponding to the index numbers of the groups. 

Unfortunately, one can't use `group_indices()` in `mutate()`, it returns an error.
Therefore, one has to sort the dataframe by the grouping variables before adding in the id. 
It's important to note that the data **must** be sorted prior to the creation of `i`, otherwise the `mutate(group_id = i)` will be in the wrong sort order.


```r
data(mtcars)
mtcars <- mtcars %>% group_by(cyl, am) %>% arrange(cyl, am)

i <- mtcars %>% group_indices()

mtcars <- mtcars %>% ungroup() %>% mutate(group_id = i)
mtcars %>% select(cyl, am, group_id)
```

```
## # A tibble: 32 x 3
##      cyl    am group_id
##    <dbl> <dbl>    <int>
##  1     4     0        1
##  2     4     0        1
##  3     4     0        1
##  4     4     1        2
##  5     4     1        2
##  6     4     1        2
##  7     4     1        2
##  8     4     1        2
##  9     4     1        2
## 10     4     1        2
## # ... with 22 more rows
```

## Dropping and renaming variables

Variables can be renamed with `rename()`. 
The syntax is `rename(data, new.var = old.var)`. 
Note that there are no quotations marks. 
e.g. 


```r
rename(iris, petal_length = Petal.Length) %>% head()
```

```
##   Sepal.Length Sepal.Width petal_length Petal.Width Species
## 1          5.1         3.5          1.4         0.2  setosa
## 2          4.9         3.0          1.4         0.2  setosa
## 3          4.7         3.2          1.3         0.2  setosa
## 4          4.6         3.1          1.5         0.2  setosa
## 5          5.0         3.6          1.4         0.2  setosa
## 6          5.4         3.9          1.7         0.4  setosa
```

Variables can be dropped using `select(data, -var_name)`. 
Or, if quicker, retained from a long list of variables, e.g. `select(data, var1, var2)`.
                                                                                               

```r
select(iris, -Sepal.Length) %>% head
```

```
##   Sepal.Width Petal.Length Petal.Width Species
## 1         3.5          1.4         0.2  setosa
## 2         3.0          1.4         0.2  setosa
## 3         3.2          1.3         0.2  setosa
## 4         3.1          1.5         0.2  setosa
## 5         3.6          1.4         0.2  setosa
## 6         3.9          1.7         0.4  setosa
```


```r
select(iris, Sepal.Length, Species) %>% head
```

```
##   Sepal.Length Species
## 1          5.1  setosa
## 2          4.9  setosa
## 3          4.7  setosa
## 4          4.6  setosa
## 5          5.0  setosa
## 6          5.4  setosa
```
                                                                                               
It's also possible to select columns based on text that the column name contains:
                                                                                               

```r
pps <- pps %>% select(p.full.id, contains("_atc"))
```
                                                                                               
thanks to: http://stackoverflow.com/questions/25923392/r-dplyr-select-columns-based-on-string

 
## Lagged values and windows

```r
#  test windowed functions
test <- data.frame(cbind(time = c(1:10), value = rep(1, 10)), 
                   stringsAsFactors = FALSE)
head(test)
```

```
##   time value
## 1    1     1
## 2    2     1
## 3    3     1
## 4    4     1
## 5    5     1
## 6    6     1
```

```r
test <- test %>% 
  mutate( lag1 = lag(value),
          lag2 = lag(value, 2),
          lag3 = lag(value, 3),
          lag4 = lag(value, 4)
          
          ) %>% # next line needs to be outside first mutate call otherwise it won't work.
  mutate(rolling_4 = rowSums(.[3:6], na.rm = TRUE))
test
```

```
##    time value lag1 lag2 lag3 lag4 rolling_4
## 1     1     1   NA   NA   NA   NA         0
## 2     2     1    1   NA   NA   NA         1
## 3     3     1    1    1   NA   NA         2
## 4     4     1    1    1    1   NA         3
## 5     5     1    1    1    1    1         4
## 6     6     1    1    1    1    1         4
## 7     7     1    1    1    1    1         4
## 8     8     1    1    1    1    1         4
## 9     9     1    1    1    1    1         4
## 10   10     1    1    1    1    1         4
```

## Wrapping dplyr in a function

Programming with dplyr has changed a lot with the release of dplyr 0.7. 
Previously there was a convoluted process using lazy eval and the `interp` function. 
Now, dplyr incorporates tidyeval and is much simpler. 
The vignette on [programming with dplyr](https://github.com/simonthelwall/simonthelwall.github.io.git) provides a much longer explanation of how it works.

### A simple tidyeval example
Say I want a function to sort a dataframe by a named column. 
I know that dplyr will already do this, but this is simply to illustrate how evaluation works when writing functions with dplyr. 


```r
data(mtcars)
select_a_column <- function(dat, column){
  col_to_use <- enquo(column)
  dat <- select(dat, !!col_to_use)
  return(dat)
}
select_a_column(mtcars, mpg) %>% head()
```

```
##                    mpg
## Mazda RX4         21.0
## Mazda RX4 Wag     21.0
## Datsun 710        22.8
## Hornet 4 Drive    21.4
## Hornet Sportabout 18.7
## Valiant           18.1
```

Two steps here: `enquo()` to capture the variable of interest and `!!` to use it. 

One has to interpret the multiple variables supplied, otherwise you get all sorts of errors. 

```r
# require(lazyeval) # I thought dplyr loaded this by default, but apparently not.
tabFun <- function(dat, y, z) {
  grouping_var <- enquo(y)
  summing_var <- enquo(z)
  a <- dat %>% group_by(!!grouping_var) %>%
    summarise(n = length(!!summing_var), 
              sum_z = sum(!!summing_var)) %>%
    mutate(pc = round((sum_z / n) * 100, 1)
           )
  return(a)
}
tabFun(mtcars, cyl, am)
```

```
## # A tibble: 3 x 4
##     cyl     n sum_z    pc
##   <dbl> <int> <dbl> <dbl>
## 1     4    11     8  72.7
## 2     6     7     3  42.9
## 3     8    14     2  14.3
```
Phew. 

Oh, and incidentally, trying to simplify this with n = n() returns the error `Error in n() : This function should not be called directly`. 

I don't know why. 

### Mutate in a function


```r
temp_dat <- data.frame(dob = as.Date("01/01/2017", format = "%d/%m/%Y"), 
                       sampledate = as.Date("15/06/2017", format = "%d/%m/%Y"))


age_fun <- function(dat, dob_var, sampledate_var) {
  var1 <- enquo(dob_var)
  var2 <- enquo(sampledate_var)
  out <- dat %>% 
    mutate(
      age = as.numeric( (!!var2) - (!!var1) ) / 365.25
    )
  return(out)
}

# doesn't work
age_fun2 <- function(dat, dob_var, sampledate_var) {
  # var1 <- enquo(dob_var)
  # var2 <- enquo(sampledate_var)
  out <- dat %>% 
    mutate(
      age = as.numeric( dob_var - sampledate_var ) / 365.25
    )
  return(out)
}

temp_dat <- age_fun(dat = temp_dat, dob_var = dob, sampledate_var = sampledate)
temp_dat <- age_fun2(dat = temp_dat, dob_var = dob, sampledate_var = sampledate)
temp_dat
```

## Joining

Very similar to plyr, but the function name specifies the type of join rather than specifying within the function. 


```r
                                                                                 data3 <- left_join(data1, data2, by = "common.var")
```
                                                                                 See [SQL Venn](https://blog.codinghorror.com/a-visual-explanation-of-sql-joins/) for reminders. 

Usefully, one can join by more than one variable. 
This is useful when a single variable does not provide a unique identifier. 



```r
x <- data.frame(
  var1 = c(1, 1, 2),
  var2 = c("a", "b", "b"),
  var3 = c("x", "y", "z")
  )
y <- data.frame(var1 = c(1, 1, 2),
                var2 = c("a", "b", "b"),
                var4 = c(20, 21, 22)
                )
x
```

```
##   var1 var2 var3
## 1    1    a    x
## 2    1    b    y
## 3    2    b    z
```

```r
z <- left_join(x, y, by = c("var1", "var2"))
z
```

```
##   var1 var2 var3 var4
## 1    1    a    x   20
## 2    1    b    y   21
## 3    2    b    z   22
```

### anti_join

Anti join retains those records which are in the left table but not the right and retains the columns of the left table. 

This is probably more useful than one might at first think. 


```r
require(dplyr)

# want those in 2, that do not appear in 1
anti1 <- data.frame(id = c(1, 1, 1, 2, 3),
                    abx = c("pen", "amx", "flu", "pen", "amx"))
anti2 <- data.frame( id = c(1, 1, 1, 2, 4),
                     abx = c("pen", "amx", "flu", "flu", "pen"),
                     res = c("r", "i", "s", "i", "s")
                     )
anti1
```

```
##   id abx
## 1  1 pen
## 2  1 amx
## 3  1 flu
## 4  2 pen
## 5  3 amx
```

```r
anti2
```

```
##   id abx res
## 1  1 pen   r
## 2  1 amx   i
## 3  1 flu   s
## 4  2 flu   i
## 5  4 pen   s
```

```r
anti3 <- anti_join(anti2, anti1, by = "id")
anti3
```

```
##   id abx res
## 1  4 pen   s
```

```r
anti3 <- anti_join(anti2, anti1, by = c("id", "abx"))
anti3
```

```
##   id abx res
## 1  2 flu   i
## 2  4 pen   s
```

## Hypothesis tests and dplyr

Technically possible, see broom and dplyr. 
I haven't (yet) managed this for a `chisq.test()` , but: 


```r
test <- as.data.frame(structure(
  list(organism.species.name = c("ec", "ec", "kp"),
       abx = c("ceph", "carb", "ceph"),
       n_sus_start = c(5L, 5L, 10L),
       n_res_start = c(10L, 5L, 5L),
       n_sus_end = c(10L, 5L, 5L),
       n_res_end = c(5L, 5L, 10L)),
  .Names = c("organism.species.name", "abx", "n_sus_start", "n_res_start",
  "n_sus_end", "n_res_end"), class = "data.frame", row.names = c(NA,-3L)
  ))
  
test %>% group_by(organism.species.name, abx) %>%
  mutate(p.val = chisq.test(matrix(
  c(n_sus_start, n_res_start, n_sus_end, n_res_end), nrow = 2))$p.value)
```

```
## # A tibble: 3 x 7
## # Groups:   organism.species.name, abx [3]
##   organism.specie~ abx   n_sus_start n_res_start n_sus_end n_res_end p.val
##   <chr>            <chr>       <int>       <int>     <int>     <int> <dbl>
## 1 ec               ceph            5          10        10         5 0.144
## 2 ec               carb            5           5         5         5 1    
## 3 kp               ceph           10           5         5        10 0.144
```
    
## tidyr

### wide to long

```r
stocks <-
  data.frame(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
  )
  stocks
```

```
##          time           X          Y          Z
## 1  2009-01-01 -0.04585453 -1.4495466  0.2587919
## 2  2009-01-02  0.22757262  0.4319155 -1.5743721
## 3  2009-01-03 -0.39376239 -0.8340409 -2.8023892
## 4  2009-01-04 -2.00358729  5.1717332 -4.2780069
## 5  2009-01-05  2.62465922 -3.0533851 -1.0727224
## 6  2009-01-06 -0.96244722 -0.3959880 -3.5751754
## 7  2009-01-07  0.15607861  4.5448018 -3.0587058
## 8  2009-01-08  1.17352798 -0.6613273  1.0263555
## 9  2009-01-09 -0.96637365  1.0986367  3.1425586
## 10 2009-01-10  0.90033313  3.5056913  5.4665945
```

```r
  gather(data = stocks, key = stock, value = price, -time)
```

```
##          time stock       price
## 1  2009-01-01     X -0.04585453
## 2  2009-01-02     X  0.22757262
## 3  2009-01-03     X -0.39376239
## 4  2009-01-04     X -2.00358729
## 5  2009-01-05     X  2.62465922
## 6  2009-01-06     X -0.96244722
## 7  2009-01-07     X  0.15607861
## 8  2009-01-08     X  1.17352798
## 9  2009-01-09     X -0.96637365
## 10 2009-01-10     X  0.90033313
## 11 2009-01-01     Y -1.44954657
## 12 2009-01-02     Y  0.43191551
## 13 2009-01-03     Y -0.83404093
## 14 2009-01-04     Y  5.17173319
## 15 2009-01-05     Y -3.05338512
## 16 2009-01-06     Y -0.39598803
## 17 2009-01-07     Y  4.54480178
## 18 2009-01-08     Y -0.66132732
## 19 2009-01-09     Y  1.09863666
## 20 2009-01-10     Y  3.50569129
## 21 2009-01-01     Z  0.25879187
## 22 2009-01-02     Z -1.57437215
## 23 2009-01-03     Z -2.80238921
## 24 2009-01-04     Z -4.27800687
## 25 2009-01-05     Z -1.07272236
## 26 2009-01-06     Z -3.57517542
## 27 2009-01-07     Z -3.05870585
## 28 2009-01-08     Z  1.02635549
## 29 2009-01-09     Z  3.14255858
## 30 2009-01-10     Z  5.46659454
```

### long to wide

`spread()` takes long data and makes it wide. 
One specifies the `key` which contains the cells that will become column headers and then `value` column which contains the data that will go in the cells beneath the new columns.


```r
data(infert)
long <- infert %>%
  mutate(id = seq_along(education)) %>% # create integer id
  gather(key = variable, value = val,-id)
```

```
## Warning: attributes are not identical across measure variables;
## they will be dropped
```

```r
head(long)
```

```
##   id  variable     val
## 1  1 education  0-5yrs
## 2  2 education  0-5yrs
## 3  3 education  0-5yrs
## 4  4 education  0-5yrs
## 5  5 education 6-11yrs
## 6  6 education 6-11yrs
```

```r
long %>% spread(key = variable, value = val) %>%
  head()
```

```
##   id age case education induced parity pooled.stratum spontaneous stratum
## 1  1  26    1    0-5yrs       1      6              3           2       1
## 2  2  42    1    0-5yrs       1      1              1           0       2
## 3  3  39    1    0-5yrs       2      6              4           0       3
## 4  4  34    1    0-5yrs       2      4              2           0       4
## 5  5  35    1   6-11yrs       1      3             32           1       5
## 6  6  36    1   6-11yrs       2      4             36           1       6
```
                                                                                       
For a more complex approach see [this SO post](http://stackoverflow.com/questions/30592094/r-spreading-multiple-columns-with-tidyr#30592293).

See also `unite()` in tidyr

If `spread()` tells you `"Error: All columns must be named"`, then you have `NA` values in your key column. 


```r
dat <- data_frame(Person = rep(c("greg", "sally", "sue"), each = 2),
                  Time = rep(c("Pre", "Post"), 3),
                  Score1 = round(rnorm(6, mean = 80, sd = 4), 0),
                  Score2 = round(jitter(Score1, 15), 0),
                  Score3 = 5 + (Score1 + Score2) / 2)
  
head(dat)
```

```
## # A tibble: 6 x 5
##   Person Time  Score1 Score2 Score3
##   <chr>  <chr>  <dbl>  <dbl>  <dbl>
## 1 greg   Pre       85     83   89  
## 2 greg   Post      91     88   94.5
## 3 sally  Pre       84     84   89  
## 4 sally  Post      81     84   87.5
## 5 sue    Pre       82     83   87.5
## 6 sue    Post      80     79   84.5
```

```r
dat %>%
  gather(temp, score, starts_with("Score")) %>%
  unite(temp1, Time, temp, sep = "_") %>%
  spread(temp1, score)
```

```
## # A tibble: 3 x 7
##   Person Post_Score1 Post_Score2 Post_Score3 Pre_Score1 Pre_Score2
##   <chr>        <dbl>       <dbl>       <dbl>      <dbl>      <dbl>
## 1 greg            91          88        94.5         85         83
## 2 sally           81          84        87.5         84         84
## 3 sue             80          79        84.5         82         83
## # ... with 1 more variable: Pre_Score3 <dbl>
```

### Expanding data to cover all permutations

There are a number of options for this. 
A combination of spreading to wide, then gathering to long is one approach. 
However, tidyr also provides `complete` and `expand`. 
I quite often need to produce summary counts by an organisation and if one organisation hasn't reported any cases for a period then there will be a single `NA` row for that organisation, rather than multiple rows of zero. 

By the description in the manual, `complete` is 

> "a wrapper around expand(), dplyr::left_join() and replace_na() that’s useful for completing missing combinations of data"

Given the data below, I want to have all the rows for St James Infirmary that I also have for St Elsewhere. 


```r
dat <- structure(list(org_code = c("1A", "1A", "1A", "1A", "1A", "1A", 
                                   "1A", "1A", "1A", "1A", "1A", "1A", "2F"), 
                      status = c("FT", "FT", "FT", "FT", "FT", "FT", "FT", 
                                 "FT", "FT", "FT", "FT", "FT", "--"), 
                      org_name = c("St Elsewhere", "St Elsewhere", "St Elsewhere", 
                                   "St Elsewhere", "St Elsewhere", "St Elsewhere", 
                                   "St Elsewhere", "St Elsewhere", "St Elsewhere", 
                                   "St Elsewhere", "St Elsewhere", "St Elsewhere", 
                                   "St James Infirmary"), 
                      year = c(2016L, 2016L, 2016L, 2016L, 2016L, 2016L, 2017L, 
                               2017L, 2017L, 2017L, 2017L, 2017L, NA), 
                      month = c(11L, 11L, 11L, 12L, 12L, 12L, 1L, 1L, 1L, 2L, 
                                2L, 2L, NA), 
                      measure = c("measure_a", "measure_b", "measure_c", 
                                  "measure_a", "measure_b", "measure_c", 
                                  "measure_a", "measure_b", "measure_c", 
                                  "measure_a", "measure_b", "measure_c", NA), 
                      count = c(1L, 5L, 7L, 5L, 9L, 3L, 7L, 4L, 2L, 9L, 5L, 7L, 
                                NA)), .Names = c("org_code", "status", 
                                                 "org_name", "year", "month", 
                                                 "measure", "count"), 
                 class = "data.frame", row.names = c(NA, -13L))

dat
```

```
##    org_code status           org_name year month   measure count
## 1        1A     FT       St Elsewhere 2016    11 measure_a     1
## 2        1A     FT       St Elsewhere 2016    11 measure_b     5
## 3        1A     FT       St Elsewhere 2016    11 measure_c     7
## 4        1A     FT       St Elsewhere 2016    12 measure_a     5
## 5        1A     FT       St Elsewhere 2016    12 measure_b     9
## 6        1A     FT       St Elsewhere 2016    12 measure_c     3
## 7        1A     FT       St Elsewhere 2017     1 measure_a     7
## 8        1A     FT       St Elsewhere 2017     1 measure_b     4
## 9        1A     FT       St Elsewhere 2017     1 measure_c     2
## 10       1A     FT       St Elsewhere 2017     2 measure_a     9
## 11       1A     FT       St Elsewhere 2017     2 measure_b     5
## 12       1A     FT       St Elsewhere 2017     2 measure_c     7
## 13       2F     -- St James Infirmary   NA    NA      <NA>    NA
```

Using `complete` I get what I need. 
I also need to use `unite` and `separate`, otherwise I get rows for 2016 month 1 and 2016 month 2.


```r
dat2 <- dat %>% 
  unite(year_month, year, month) %>% 
  complete(year_month, measure, 
                         nesting(org_code, status, org_name), 
                        fill = list(count = 0)) %>% 
  separate(year_month, into = c("year", "month")) %>%
  # don't strictly need these lines, but they make the result more logical based on original data
  select(org_code, status, org_name, year, month, measure) %>% # re-order cols
  arrange(org_code, year, month, measure) # sort data
tail(dat2, n = 12)
```

```
## # A tibble: 12 x 6
##    org_code status org_name           year  month measure  
##    <chr>    <chr>  <chr>              <chr> <chr> <chr>    
##  1 2F       --     St James Infirmary 2017  1     measure_a
##  2 2F       --     St James Infirmary 2017  1     measure_b
##  3 2F       --     St James Infirmary 2017  1     measure_c
##  4 2F       --     St James Infirmary 2017  1     <NA>     
##  5 2F       --     St James Infirmary 2017  2     measure_a
##  6 2F       --     St James Infirmary 2017  2     measure_b
##  7 2F       --     St James Infirmary 2017  2     measure_c
##  8 2F       --     St James Infirmary 2017  2     <NA>     
##  9 2F       --     St James Infirmary NA    NA    measure_a
## 10 2F       --     St James Infirmary NA    NA    measure_b
## 11 2F       --     St James Infirmary NA    NA    measure_c
## 12 2F       --     St James Infirmary NA    NA    <NA>
```

The manual adds a helpfull explanation of the use of `nesting`:

> "To find all unique combinations of x, y and z, including those not found in the data, supply each variable as a separate argument. To find only the combinations that occur in the data, use nest: expand(df, nesting(x, y, z))."

Crossing is the tidy equivalent of expand.grid and doesn't convert strings to factors.


```r
crossing(year = c(2016, 2017, 2018), 
         month = c("January", "February", "March"))
```

```
## # A tibble: 9 x 2
##    year month   
##   <dbl> <chr>   
## 1  2016 February
## 2  2016 January 
## 3  2016 March   
## 4  2017 February
## 5  2017 January 
## 6  2017 March   
## 7  2018 February
## 8  2018 January 
## 9  2018 March
```
