---
layout: page
title: R notes
tagline: Things I want to remember about using R
description: Some notes about using R in a fairly disorganised collection
---

* TOC
{:toc}

## Getting data in
First set the working directory

```r
setwd("C:/filepath/folder")
```

In windows, either single forward slashes as above, or double backslashes. 

Then read in a csv file

```r
dat <- read.csv(file = "filename.csv", stringsAsFactors = FALSE)
```

Headers are `TRUE` by default, and one can set the separator to something other than the default comma with `r sep = "|"`

Omitting `quote=""` may result in R not reading all rows in, weirdly.
Another potential gotcha occurs with comments. 
Reading in a data file you might come across the following error:

```r
Error in scan(file, what, nmax, sep, dec, quote, skip, nlines, na.strings,  : 
  line 361853 did not have 20 elements
```
	  
So, for some reason this line does not have the same number of variables as all the others, and R declines to load in the file. 
The reason why is given [here](http://cran.r-project.org/doc/manuals/R-data.html#Spreadsheet_002dlike-data).  
In this instance, line 361853 contained a string variable with a hash symbol in it. 
R sees this as a comment character and so ignores the rest of the line. 
This is resolved by adding `comment.char=""` to the `read.csv()` command. 

It's generally worth checking that the data has imported correctly:

```r
head(data)
```
This prints the first six lines of the dataframe.

If you're feeling more advanced, it's possible to loop over files in a directory and append them to a single data frame. e.g.

```r
setwd() # give actual path here
files <- list.files(pattern = ".txt")

for (i in 1:length(files)){
  print(files[i])
  z <- read.delim(files[i], header = TRUE, sep = "|")
  if (!exists("x")){
    x <- z
  }else{
    x <- rbind(z, x)
  }
```

for the `exists()` statement, the object needs to be in quote marks otherwise it doesn't work. 

What if your data have different numbers of columns though? 
`rbind()` won't work in this situation. 
I found this online. 

```r
#shamelessly pinched from here: https://amywhiteheadresearch.wordpress.com/2013/05/13/combining-dataframes-when-the-columns-dont-match/

rbind.all.columns <- function(x, y) {
  
  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))
  
  x[, c(as.character(y.diff))] <- NA
  
  y[, c(as.character(x.diff))] <- NA
  
  return(rbind(x, y))
}
```
	
## Really basic stuff

If you've succeeded in getting your data in, then you probably want to start trying some calculations.

```r
mean(data$var1, na.rm=TRUE)
```
This will calculate the mean of the values in variable var1. The na.rm statement tells R to exclude any missing values. 

## Installing and loading libraries

```r
install.packages("library name")
```

then choose the mirror site of choice. 
To use in scripts:

```r
library("ggplot2")
```

## Capturing errors to a logfile

```r
error.file <- file("new_errorfile.txt", open = "wt")
sink(error.file, type = "message")
sink()
close(error.file)
```

## Installing packages from github

```r
library(devtools)
httr::set_config(httr::use_proxy("123.123.123.123",8080)) # replace 123... with your proxy settings
install_github("user/repository")
```

## List of subpages

 - [bookdown](bookdown.md)
 - [dplyr](dplyr.md)