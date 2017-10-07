---
exclude: TRUE
---

* TOC
{:toc}

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
