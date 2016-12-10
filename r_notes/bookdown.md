---
layout: page
title: bookdown notes
tagline: Things I want to remember about using bookdown
description: Some notes about using bookdown in a fairly disorganised collection
---

Bookdown makes it easy to write prose in markdown, code in R and compile both to well featured document that incorporates cross-references, a bibliography and captions.

## Getting started
All that's needed is an .Rmd file called index that has the following settings in its YAML header. 

```YAML
output: bookdown::gitbook
site: bookdown::bookdown_site
```

Once ready, select the Build tab in RStudio (the top right quadrent of the interface on my setup), and click 'Build Book'.
Bookdown will look for .Rmd files in the sathe files to reme directory and bundle them into a file (`_main.Rmd`) it creates itself. 
Each .Rmd file will constitute a single chapter, with the first level header forming the chapter title and second level headers forming section titales.

**Important note:** The bookdown files should not have their own YAML headers as with a normal .Rmd file, this causes the chapter sectioning to fail. 