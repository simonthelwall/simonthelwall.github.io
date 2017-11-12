


## Dates


```r
x <- "02/10/2017"
x
```

```
## [1] "02/10/2017"
```

```r
x <- as.Date(x, format = "%d/%m/%Y")
x
```

```
## [1] "2017-10-02"
```

Date components:
Day

 * `%a` Abbreviated day of week
 * `%A` Full day of week
 * `%d` Numeric day of the month (01-31)
 * `%e` Numeric day of month with leading space for single digits
 * `%u` Numeric day of week, Monday = 1
 
Month 

 * `%b` Abbreviated month
 * `%B` Full month name
 * `%m` Numeric month (01-12)

Year

 * `%y` Year without century
 * `%Y` Year with century
 
So one can format a date:


```r
format(x, "%a")
```

```
## [1] "Mon"
```

```r
format(x, "%A, %d %B %Y")
```

```
## [1] "Monday, 02 October 2017"
```

## Logical operators


```r
x <- 5
x == 5
```

```
## [1] TRUE
```

```r
x < 1
```

```
## [1] FALSE
```

```r
x > 2
```

```
## [1] TRUE
```

```r
x < 5
```

```
## [1] FALSE
```

```r
x <= 5
```

```
## [1] TRUE
```

```r
x %in% c(1, 2, 3, 4, 5)
```

```
## [1] TRUE
```

## Data types

### Data frames

These are the bread-and-butter data types where you'll do most of your work. 
They are rectangular data where each row forms an observation and each column is a variable. 
Technically, a dataframe is a `list` of equal-length `vectors`. 

### Making a dataframe


```r
x <- data.frame(id = c(1,2,3,4,5),
                x_var = c(4,2,9,4,7), 
                height = c("Tall", "Short", "Medium", "Tall", "Short"), 
                stringsAsFactors = FALSE)
x
```

```
##   id x_var height
## 1  1     4   Tall
## 2  2     2  Short
## 3  3     9 Medium
## 4  4     4   Tall
## 5  5     7  Short
```

### Accessing elements of a dataframe

Square-bracket indexing is used to access elements of a dataframe



```r
# First row
x[1, ]
```

```
##   id x_var height
## 1  1     4   Tall
```

```r
# First column
x[ , 1]
```

```
## [1] 1 2 3 4 5
```

```r
# Second row, third col
x[2, 3]
```

```
## [1] "Short"
```

```r
# all cols for 'short' observations
x[x$height == "Short", ]
```

```
##   id x_var height
## 2  2     2  Short
## 5  5     7  Short
```

## Missing values

Missing values are represented as `NA` in R. 
These are handled slightly differently to missing values in other statistical packages. 
`NA` values cannot be compared with other values.


```r
x <- NA
class(x)
```

```
## [1] "logical"
```

```r
x < 1
```

```
## [1] NA
```

```r
x > 1
```

```
## [1] NA
```

```r
is.na(x)
```

```
## [1] TRUE
```

```r
dat <- data.frame(x = c(1,2, NA), y = c("a", "b", "c"))
dat[is.na(dat$x) == TRUE, ]
```

```
##    x y
## 3 NA c
```

## Factors

Text can be stored in two ways: as character strings or as factors. 
When sorted, character strings are ordered alphabetically. 
Factors, however, are ordered as set by the levels of the factor. 


```r
dat <- data.frame(x = c("triangle", "triangle", "triangle", "square", "square",
                        "circle"), 
                  stringsAsFactors = FALSE)

table(dat$x) # alphabetical sorting
```

```
## 
##   circle   square triangle 
##        1        2        3
```

```r
dat$x <- factor(dat$x, levels = c("square", "circle", "triangle"))
table(dat$x) # sorting as determined by levels of factor.
```

```
## 
##   square   circle triangle 
##        2        1        3
```

So this can be used to determine the ordering of groups in plots. 
