---
exclude: true
--- 

* TOC
{:toc}


Writing packages can be a lot of fun, and it can be a lot of pain. 
These are some notes to ease the pain.
[Hadley's notes](http://r-pkgs.had.co.nz/) are much better.

## Set up

Use RStudio. Make sure that the following are installed. 


```r
install.packages(c("devtools", "roxygen2", "testthat"))
```

## Write functions

And save them to a file in the `/R` directory as a `.R` file.
One function per file works for me, but it is possible to have >1 function per `.R` file. 

## Write documentaton for functions
ROxygen builds documentation automatically from tags


```r
#' Gives the absolute value of the per cent or total change between two values
#'
#' @param from The starting value
#' @param to The finishing value
#' @param type Either "percent" or "value"
#' @examples
#' change(200, 100, "percent")
#' change(200, 100, "value")
#' change(100, 200, "percent")
#' @export
```

## Document the package

If you need to use other packages in your own package, use devtools to add the dependency.


```r
devtools::use_package("packagename")
```

## Run automated checks on the package

Building can be a slow process if done repeatedly. 
Using `check()` can identify faults before building saving pain.


```r
devtools::check()
```

And to build a pdf manual


```r
devtools::check(package = "hcaidcs", document = TRUE)
```

## Add vignette(s)

Because it makes the whole thing easier to comprehend.
Create folder and add a basic vigentte called 'my-vignette'

```r
devtools::use_vignette("my-vignette")
```

Test that the vignette builds, and copy the html file to the inst/doc folder by doing:

```r
devtools::build_vignette()
```

Then, if building a package as binary 

```r
devtools::build(binary = TRUE, manual = TRUE, vignettes = TRUE)
```

Then, in my experience, only `devtools::install()` actually adds the final vignette
From the package directory, do: 

```r
devtools::install()
```

Then open the vignette with 


```r
vignette("my-vignette")
```

## Building the package

Finally, build the package. 
This installs the package locally, allowing you to load it and use the functions that you've written. 

### First make sure that all the exported functions have been exported

Build will fail if R can't find a function

```r
devtools::document()
```

### Then, actually build the package
Using RStudio under 'Build' tab, click 'Build & Reload'.

One can also do: 

```r
devtools::build(binary = TRUE, manual = TRUE)
```
`binary = TRUE` creates a zip file for the package. 

If build stops working, then you may need to makes sure all the following are up to date:

* R
* Rtools (download from CRAN)
* All packages
* RStudio

### (Re)Install the package


```r
install.packages("C:/pkg_name_0.0.0.9000.zip", repos = NULL, type = "win.binary")
```
and then reload

```r
library(pkg_name)
```
