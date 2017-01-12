---
exclude: true
---

# dplyr and tidyr

dplyr is the replacement for plyr. 
I used plyr a lot for my work, but the replacement should change things considerably, including by making it easier to create wrapper functions for split-apply-combine operations. 
One of the particularly nice things is that for repetitive operations, it should be relatively easy to create wrapper functions to achieve this. 

One really nice feature is the ability to connect a series of commands in the same way as pipes in Unix systems. 
Rather than the vertical bar `|`, the symbol is `%>%`.

As well as dplyr, Hadley has introduced another new package for reshaping: tidyr. 

* TOC
{:toc}

## The basics

dplyr has, as described in the introductory documentation, "five basic data manipulation verbs that work on a single table: filter(), arrange(), select(), mutate() and summarise()."

## Creating new variables

Uses `mutate()`

### Detecting strings

I have a vector in a dataframe and I wish to determine whether I can detect any one of a series of strings. 
I would normally use str_detect for this, but it doesn't work without a little trickery. 
The `paste(something, collapse = "|")` is the important bit here. 

```r
test.data <- data.frame(item = c("Apple", "Bear", "Orange", "Pear", "Two Apples"))
fruit <- c("Apple", "Orange", "Pear")
test.data
  item
1 Apple
2 Bear
3 Orange
4 Pear
5 Two Apples

test.data <- test.data %>% 
    mutate(is.fruit = str_detect(item, paste(fruit, collapse = "|")))
test.data
  item          is.fruit
1 Apple         TRUE
2 Bear          FALSE
3 Orange        TRUE
4 Pear          TRUE
5 Two Apples    TRUE
```
## Replacing values with dplyr functions
Can use `recode()` or `case_when()`. 
`recode()` is a vectorised switch. 
It's important to note that the data types must be the same when using `recode()`, and that a double is not the same type as an integer, which seems obvious when typed. 
If they are not the same type, then unspecified replacements get NA values. 

```r
mtcars %>% 
  mutate(new_cyl = recode(cyl, `6` = 16, `8` = 200)
         ) %>% 
  select(cyl, new_cyl) %>% 
  tbl_df() %>%
  head()
  
# A tibble: 6 × 2
    cyl new_cyl
  <dbl>   <dbl>
1     6      16
2     6      16
3     4       4
4     6      16
5     8     200
6     6      16
```

`case_when()` vectorises multiple `if()` `else()` statements, and it's a lot neater than writing nested `ifelse()` statements. 
`case_when()` can be used inside a `mutate()` call, or outside. 
If inside, then `.$` needs to be used to specify the column upon which tests should be predicated. 

```r
mtcars %>% 
  mutate(new_cyl = case_when(.$cyl == 4 ~ "Four", 
                             .$cyl == 6 ~ "Six", 
                             .$cyl == 8 ~ "Eight"
                             )
  ) %>% 
  select(cyl, new_cyl) %>% 
  head()

  cyl new_cyl
1   6     Six
2   6     Six
3   4    Four
4   6     Six
5   8   Eight
6   6     Six
```

## Working on multiple columns at once

`mutate_each` or `summarise_each`

```r
dat <- dat %>% mutate_each(funs(mean), contains("date"))
```

At some points you might want to work on some columns and not others. 
Here it is easiest to create a vector to store your column names, which you can create with greps. 
Requires lazyeval and NSE

```r
library(lazyeval)
date_cols <- names(dat)[str_detect(names(dat), "date") == TRUE & 
                           str_detect(names(dat), "interactions") == FALSE]
head(date_cols)
# then
dat %>% mutate_each_(funs(mean), date_cols)
```

### Passing arguments to summarise_each or mutate_each

For example, you might want to specify removal of NA values in a sum or mean function. 
You can do this by
```r
mutate_each(dat, funs(sum(., na.rm = TRUE))
```

## Subsetting (aka filtering)

Much less verbose than `test[test$var1 == "something",]`
```r
filter(test, var1 == "something)
```

But getting unique combinations is a little more awkward. 
One can hijack `n_distinct()` introduced in dplyr to achieve this.

```r
test <- data.frame(id = c(1,1,1,2,2),
                     org = c("apple", "apple", "bear", "orange", "pear"),
                     test = c("S", "R","S", "R", "S"))
test
id    org test
 1  apple    S
 1  apple    R
 1   bear    S
 2 orange    R
 2   pear    S
 
out <- test %>% group_by(id, org) %>% 
  filter(row_number() == 1)
out

Source: local data frame [4 x 3]
Groups: id, org

id    org test
 1  apple    S
 1   bear    S
 2 orange    R
 2   pear    S
```

## Summarising data

Uses the `summarise()` function. `n()` is a function introduced in dplyr and produces a count of the number of rows in the data set. 

```r
require(binom)
require(dplyr)

test <- data.frame(group = c(rep("A", 10), rep("B", 10)),
                   outcome = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0))
test
...results not shown

table(test$group, test$outcome, dnn = c("Group", "Outcome"))
     Outcome
Group 0 1
    A 9 1
    B 8 2

out <-test %>% group_by(group) %>% 
  summarise(n = n(), successes = sum(outcome), 
            pc.succ = round(binom.confint(successes, n, methods = "exact")[[4]]*100, 2),
            pc.lci = round(binom.confint(successes, n, methods = "exact")[[5]]*100, 2),
            pc.uci = round(binom.confint(successes, n, methods = "exact")[[6]]*100, 2))
out
Source: local data frame [2 x 6]

  group  n successes pc.succ pc.lci pc.uci
1     A 10         1      10   0.25  44.50
2     B 10         2      20   2.52  55.61
```

Outside dplyr I would write `binom.confint(x,n,methods = "exact")$mean`. 
This doesn't work in dplyr and returns the error `"Error in binom.confint(c(6928L, 5704L, 4746L, 5166L, 6779L, 6275L, 4832L,  : 
  invalid subscript type 'closure' "`
However, indexing the columns does work. 

Also, wrapping the `round()` and `binom.confint()` in a function would improve the readability of the process. 

## Grouped operations

There may be a variable name length limit in `group_by()`.

```r 
test <- data.frame(name = rep(c("orange", "pear", "apple", "bear"), 2) , 
+                    value = rnorm(8), stringsAsFactors = FALSE)

group_by(test, name) %>% summarise(mean(value))
Source: local data frame [4 x 2]

    name mean(value)
1  apple -0.34881128
2   bear  1.01526013
3 orange -0.68778455
4   pear -0.06635953

test$reallyreallyreallyreallyreallylongvarname <- test$name
group_by(test, reallyreallyreallyreallyreallylongvarname) %>% summarise(mean(value))
Error in eval(expr, envir, enclos) : index out of bounds
```

This can be resolved using `quote()`.

```r
group_by(test, quote(reallyreallyreallyreallyreallylongvarname)) %>% summarise(mean(value))
Source: local data frame [4 x 2]

  quote(reallyreallyreallyreallyreallyl... mean(value)
1                                    apple -0.34881128
2                                     bear  1.01526013
3                                   orange -0.68778455
4                                     pear -0.06635953
```

### Adding a group id
Sometime one might want to create an integer group id. 
i.e. for all rows with the same groupings, these will have the same id. 
`group_indices()` goes some of the way to resolve this by creating a vector of integers corresponding to the index numbers of the groups. 
Unfortunately, one can't use `group_indices()` in `mutate()`, it returns an error.
Therefore, one has to sort the dataframe by the grouping variables before adding in the id. 

It's important to note that the data **must** be sorted prior to the creation of `i`, otherwise the `mutate(group_id = i)` will be in the wrong sort order.

```r
library(dplyr)
data(mtcars)
mtcars <- mtcars %>% group_by(cyl, am) %>% arrange(cyl, am)
i <- mtcars %>% group_indices(cyl, am)
mtcars <- mtcars %>% ungroup() %>% mutate(group_id = i)
mtcars %>% select(cyl, am, group_id)
```

```r
# A tibble: 32 × 3
     cyl    am group_id
   <dbl> <dbl>    <int>
1      4     0        1
2      4     0        1
3      4     0        1
4      4     1        2
5      4     1        2
6      4     1        2
7      4     1        2
8      4     1        2
9      4     1        2
10     4     1        2
```

## Dropping and renaming variables

Variables can be renamed with `rename()`. 
The syntax is `rename(data, new.var = old.var)`. 
Note that there are no quotations marks. 
e.g. 

```r
rename(iris, petal_length = Petal.Length) %>% head()
  Sepal.Length Sepal.Width petal_length Petal.Width Species
1          5.1         3.5          1.4         0.2  setosa
2          4.9         3.0          1.4         0.2  setosa
3          4.7         3.2          1.3         0.2  setosa
4          4.6         3.1          1.5         0.2  setosa
5          5.0         3.6          1.4         0.2  setosa
6          5.4         3.9          1.7         0.4  setosa
```

Variables can be dropped using `select(data, -var.name)`. 
Or, if quicker, retained from a long list of variables, e.g. `select(data, var1, var2)`.

```r
select(iris, -Sepal.Length) %>% head
  Sepal.Width Petal.Length Petal.Width Species
1         3.5          1.4         0.2  setosa
2         3.0          1.4         0.2  setosa
3         3.2          1.3         0.2  setosa
4         3.1          1.5         0.2  setosa
5         3.6          1.4         0.2  setosa
6         3.9          1.7         0.4  setosa


> select(iris, Sepal.Length, Species) %>% head
  Sepal.Length Species
1          5.1  setosa
2          4.9  setosa
3          4.7  setosa
4          4.6  setosa
5          5.0  setosa
6          5.4  setosa
```

It's also possible to select columns based on text that the column name contains:

```r
pps <- pps %>% select(p.full.id, contains("_atc"))
```

thanks to: http://stackoverflow.com/questions/25923392/r-dplyr-select-columns-based-on-string

## Lagged values and windows

```r
#  test windowed functions
require(dplyr)

test <- data.frame(cbind(time = c(1:10), value = rep(1, 10)), stringsAsFactors = FALSE)
head(test)

test <- test %>% mutate(lag1 = lag(value), lag2 = lag(value,2),
                        lag3 = lag(value, 3), lag4 = lag(value, 4), 
                        rolling_4 = rowSums(.[3:6], na.rm = TRUE))
test
   time value lag1 lag2 lag3 lag4 rolling_4
1     1     1   NA   NA   NA   NA         0
2     2     1    1   NA   NA   NA         1
3     3     1    1    1   NA   NA         2
4     4     1    1    1    1   NA         3
5     5     1    1    1    1    1         4
6     6     1    1    1    1    1         4
7     7     1    1    1    1    1         4
8     8     1    1    1    1    1         4
9     9     1    1    1    1    1         4
10   10     1    1    1    1    1         4
```

## Wrapping dplyr in a function

Can be a little tricky and some of the information on the internet seems out of date. However, managed to wrap the following in a function to produce nice univariable tables. It does require a variable called obs though. This should be fixable with a little wrangling. 

```r
niceTable <- function(data = x, ...){
  # from https://groups.google.com/d/msg/manipulatr/htt0kO9Dhds/O5eVE2cfvsoJ
  z <- data %>% group_by_(...) %>% 
    summarise(n = sum(obs)) %>% ungroup() %>% 
    mutate(cum.pc = round((cumsum(n)/sum(n))*100,1), pc = round( (n/sum(n))*100, 2 ))
  return(z)
}
 
data(mtcars)
mtcars$obs <- 1
 
out <- niceTable(mtcars, "vs")
out
Source: local data frame [2 x 4]

  vs  n cum.pc    pc
1  0 18   56.2 56.25
2  1 14  100.0 43.75
```

Done.

Or so I thought. To go much further with this is actually quite difficult. 
Let's try without wrapping in a function:

```r
require(dplyr)
mtcars <- mtcars

mtcars %>% group_by(cyl) %>% summarise(n = n(), am = sum(am)) %>%
  mutate(pc.am = round((am/n)*100, 2))
Source: local data frame [3 x 4]

  cyl  n am pc.am
1   4 11  8 72.73
2   6  7  3 42.86
3   8 14  2 14.29
```

Then, to get this in a function, one needs to supply more than one variable to the data. 
This turns out to be rather tricky. 
One has to interpret the multiple variables supplied, otherwise you get all sorts of errors. 

```r
require(lazyeval) # I thought dplyr loaded this by default, but apparently not. 
tabFun <- function(x, y, z){
  a <- x %>% group_by_(as.name(y)) %>% 
    summarise_(n = interp(~length(var), var = as.name(z)), 
               sum.z = interp(~sum(var), var = as.name(z))) %>%
    mutate(pc.am = round((sum.z/n)*100, 2))
  return(a)
}

tabFun(mtcars, "cyl", "am")
Source: local data frame [3 x 4]

  cyl  n sum.z pc.am
1   4 11     8 72.73
2   6  7     3 42.86
3   8 14     2 14.29
```

Phew. 

Oh, and incidentally, trying to simplify this with n = n() returns the error `Error in n() : This function should not be called directly`. 
I don't know why. 

## Mutate in a function

Took me a long time to find a solution for this. 
I tinkered around with `quote()` and `substitute()` for a bit. 
I don't understand why a simple `mutate_()` doesn't work. 
I don't really see the need for `[]` indexing, but the following allows you to avoid quotes in the call. 

```r
age_fun <- function(data, dob, sampledate){
 var1 <- deparse(substitute(dob))
 var2 <- deparse(substitute(sampledate))
 data <- mutate(data, age = data[, var2] - data[, var1],
          age = as.numeric(age)/365.25)
}

ec <- age_fun(ec, date_of_birth, sample_date)
from http://stackoverflow.com/questions/24606282/passing-data-frame-to-mutate-within-function
```

## Wrapping filter in a function

```r
require(dplyr)
data(mtcars)
mtcars$name <- row.names(mtcars)

just_1 <- mtcars %>% filter(name == "Mazda RX4")
just_1

  mpg cyl disp  hp drat   wt  qsec vs am gear carb      name
1  21   6  160 110  3.9 2.62 16.46  0  1    4    4 Mazda RX4

require(lazyeval)
trend_fun <- function(data, org){
  filter_criteria <- interp(~ colmn == org, colmn =as.name("name"))
  z <- data %>% filter_(filter_criteria)
}

trend_fun(mtcars, "Mazda RX4")
just_2 <- trend_fun(mtcars, "Mazda RX4")
just_2

  mpg cyl disp  hp drat   wt  qsec vs am gear carb      name
1  21   6  160 110  3.9 2.62 16.46  0  1    4    4 Mazda RX4
```

Adapted from: http://stackoverflow.com/questions/26492280/non-standard-evaluation-nse-in-dplyrs-filter-pulling-data-from-mysql

## Using `select()` in a function

```r
require(dplyr)
require(lazyeval)
data(mtcars)
head(mtcars)

select_a_column <- function(data, column){
  z <- data %>% select_("mpg", column)
  return(z)
}

select_a_column(mtcars, "hp")
```

## Arrange in a function

```r
arrange_a_column <- function(data, column){
    z <- data %>% arrange_(lazyeval::interp(~var, var = as.name(column)))
}
arrange_a_column(dat, "my_col_name")
```

## Hypothesis tests and dplyr

Technically possible, see broom and dplyr. 
I haven't (yet) managed this for a `chisq.test()`, but: 

```r
test <- as.data.frame(structure(list(organism.species.name = c("ec", "ec", "kp"), 
                                     abx = c("ceph", "carb", "ceph"), 
                                     n_sus_start = c(5L, 5L, 10L), 
                                     n_res_start = c(10L, 5L, 5L), 
                                     n_sus_end = c(10L, 5L, 5L), 
                                     n_res_end = c(5L, 5L, 10L)), 
                                .Names = c("organism.species.name", "abx", "n_sus_start", "n_res_start", "n_sus_end", "n_res_end"), 
                                class = "data.frame", row.names = c(NA, -3L)))

test %>% group_by(organism.species.name, abx) %>% 
  mutate(p.val = chisq.test(
    matrix(c(n_sus_start, n_res_start, n_sus_end, n_res_end), nrow = 2)
    )$p.value
)
Source: local data frame [3 x 7]
Groups: organism.species.name, abx

  organism.species.name  abx n_sus_start n_res_start n_sus_end n_res_end    p.val
1                    ec ceph           5          10        10         5 0.144127
2                    ec carb           5           5         5         5 1.000000
3                    kp ceph          10           5         5        10 0.144127
```

## tidyr

### wide to long

```r
stocks <- data.frame(
   time = as.Date('2009-01-01') + 0:9,
   X = rnorm(10, 0, 1),
   Y = rnorm(10, 0, 2),
   Z = rnorm(10, 0, 4)
)
stocks
         time          X          Y          Z
1  2009-01-01  0.7877668 -0.2938621  2.6682693
2  2009-01-02  1.6313870  1.4439570 -2.9431324
3  2009-01-03 -1.2691361  0.4672836  3.6694435
4  2009-01-04 -1.2445373  0.9238149  1.8050772
5  2009-01-05  0.4251908  2.8406133  0.6584698
6  2009-01-06  1.4262145  3.5163632 -4.0324487
7  2009-01-07 -0.3578377 -1.0957885 -3.3847781
8  2009-01-08 -0.1034854 -4.8278236 -1.6016573
9  2009-01-09  0.8060879 -2.3204739 -2.2709526
10 2009-01-10 -1.9365305 -5.7522708  3.1133933

gather(stocks, stock, price, -time)
         time stock      price
1  2009-01-01     X  0.7877668
2  2009-01-02     X  1.6313870
3  2009-01-03     X -1.2691361
4  2009-01-04     X -1.2445373
5  2009-01-05     X  0.4251908
6  2009-01-06     X  1.4262145
```

### Long to wide

`spread()`
For a more complex approach see [this SO post](http://stackoverflow.com/questions/30592094/r-spreading-multiple-columns-with-tidyr#30592293).

See also `unite()` in tidyr

If `spread()` tells you `"Error: All columns must be named"`, then you have `NA` values in your key column. 

```r
dat <- data_frame(
  Person = rep(c("greg", "sally", "sue"), each=2),
  Time = rep(c("Pre", "Post"), 3),
  Score1 = round(rnorm(6, mean = 80, sd=4), 0),
  Score2 = round(jitter(Score1, 15), 0),
  Score3 = 5 + (Score1 + Score2)/2
)

head(dat)
Source: local data frame [6 x 5]

  Person Time Score1 Score2 Score3
1   greg  Pre     84     85   89.5
2   greg Post     77     76   81.5
3  sally  Pre     74     73   78.5
4  sally Post     78     80   84.0
5    sue  Pre     81     81   86.0
6    sue Post     88     90   94.0

dat %>% 
  gather(temp, score, starts_with("Score")) %>% 
  unite(temp1, Time, temp, sep = ".") %>% 
  spread(temp1, score)
Source: local data frame [3 x 7]

  Person Post.Score1 Post.Score2 Post.Score3 Pre.Score1 Pre.Score2 Pre.Score3
1   greg          77          76        81.5         84         85       89.5
2  sally          78          80        84.0         74         73       78.5
3    sue          88          90        94.0         81         81       86.0
```

## Joining

Very similar to plyr, but the function name specifies the type of join rather than specifying within the function. 

```r
data3 <- left_join(data1, data2, by = "common.var")
```
See SQL Venn for reminders. 

Usefully, one can join by more than one variable. 
This is useful when a single variable does not provide a unique identifier. 

```r
x <- data.frame(var1 = c(1,1,2), var2 = c("a", "b", "b"), var3 = c("x", "y", "z"))
y <- data.frame(var1 = c(1,1,2), var2 = c("a", "b", "b"), var4 = c(20, 21, 22))
x
  var1 var2 var3
1    1    a    x
2    1    b    y
3    2    b    z
y
  var1 var2 var4
1    1    a   20
2    1    b   21
3    2    b   22
 
z <- left_join(x, y, by = c("var1", "var2"))
z
  var1 var2 var3 var4
1    1    a    x   20
2    1    b    y   21
3    2    b    z   22
```

### anti_join

Anti join retains those records which are in the left table but not the right and retains the columns of the left table. 
This is probably more useful than one might at first think. 

```
require(dplyr)

# want those in 2, that do not appear in 1

anti1 <- data.frame(id = c(1,1,1,2,3), abx = c("pen", "amx", "flu", "pen", "amx"))
anti2 <- data.frame(id = c(1,1,1,2,4), abx = c("pen", "amx", "flu", "flu", "pen"), 
                    res = c("r", "i", "s", "i", "s"))
anti1
  id abx
1  1 pen
2  1 amx
3  1 flu
4  2 pen
5  3 amx

anti2
  id abx res
1  1 pen   r
2  1 amx   i
3  1 flu   s
4  2 flu   i
5  4 pen   s

anti3 <- anti_join(anti2, anti1, by = "id")
anti3
  id abx res
1  4 pen   s

anti3 <- anti_join(anti2, anti1, by = c("id", "abx"))
anti3
  id abx res
1  2 flu   i
2  4 pen   s
```
