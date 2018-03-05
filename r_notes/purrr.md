---
exclude: true
--- 

# purrr

## Introduction

I've found purrr conceptually quite tricky. 
These are some notes to help me understand what I've already done. 
If you have any tips on how to tweak my code, please let me know. 

The idea is that purrr will iterate a function over a vector.
This can be used to iterate regression models by subsets of data, or load in data with a common structure. 

* TOC
{:toc}

## Read multiple Excel files into memory

I have a bunch of Excel files from Office for National Statistics and they give population estimates for different years. 
Each file has three sheets: male, female and persons. 
I want only the persons sheet so I'm going to create a vector of files to read in, set the path that I want to use to read the files and hopefully read the specific sheets in.

My code below is cribbed from [readxl.tidyverse.org](http://readxl.tidyverse.org/articles/articles/readxl-workflows.html#concatenate-worksheets-into-one-data-frame)

```
# setup

library(purrr)
library(readxl)
library(dplyr)

# Get a list of files to read
files_to_read <- list.files(path = "./2018/december/denominators", 
                            pattern = ".XLS")
files_to_read <- paste0("./2018/december/denominators/", files_to_read)
files_to_read[1]
[1] "./2018/december/denominators/CCG_2007_transposed.XLS"

```

Since the data themselves don't contain the year to which the data correspond, I need to obtain this information from the filename. 
It should be possible to use the `.id` to create a column in the data frame that contains the filename, but this doesn't work if I've pasted the filepath into the filename as above. 
What happens instead is that I get a column with values from 1 to the length of the `files_to_read` vector rather than values within the vector. 

So, next, I create a look up table converting the file name to the files position in the vector `files_to_read`. 

```
files_lut <- data.frame(filenm = files_to_read, 
                        file = seq_along(files_to_read), 
                        stringsAsFactors = FALSE)
files_lut <- files_lut %>% 
  mutate(year = str_extract(filenm, "\\_\\d\\d\\d\\d\\_"), 
         year = as.numeric(str_replace_all(year, "\\_", ""))) %>% 
  select(-filenm)
```

This look-up table is then used to join on the required information after reading all the files in. 

purrr syntax asks for `.x` the objects over which it will iterate and `.f`, the function that will be used. 
Supplementary arguments to the function `.f`  can be supplied afterwards and `.id` is the name of a column that will be created, containing the iterator. 

```
popn <- map_dfr(.x = files_to_read, 
                .f = read_xls, 
                sheet = "persons", 
                .id = "file") %>% 
  # left_join won't work on different data types
  mutate(file = as.integer(file)) %>% 
  # join in look-up table from above
  left_join(., files_lut)

table(popn$year, useNA = "ifany")

2007 2008 2009 2010 2011 2012 2013 2014 2015 
 207  207  207  207  207  207  207  207  207 
```

This has loaded all of the Excel files into R and I can continue my work. 

## Mutating a single variable in purrr

Having loaded my data in with map as above, I had another problem: in different data sets the same variables had different classes and `bind_rows` would not coerce the two classes. 
My immediate response was to try to mutate the column in a `map` command. 
It was much harder than expected, but the solution was surprisingly easy. 

The problem was where I was trying to put the `.` 
I thought it went `map(., ~mutate(am = as.integer(am)))` which doesn't work. 


```r
# set up
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.4.2
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
library(purrr)
```

```
## Warning: package 'purrr' was built under R version 3.4.3
```

```r
data("mtcars")

mtcars$am <- as.character(mtcars$am)

# a list of 3 data frames
mtcars <- mtcars %>% 
  split(.$cyl)

# one variable has a different class for one data frame
mtcars[[3]]$am <- as.integer(mtcars[[3]]$am)

# check 
for(i in 1:length(mtcars)){
  print(class(mtcars[[i]]$am))
}
```

```
## [1] "character"
## [1] "character"
## [1] "integer"
```

```r
# The solution
mtcars <- mtcars %>% 
  map(~mutate(., am = as.integer(am))) %>% 
  bind_rows()
```
