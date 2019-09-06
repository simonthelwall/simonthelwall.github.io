---
layout: page
title: bookdown notes
tagline: Things I want to remember about using bookdown
description: Some notes about using bookdown in a fairly disorganised collection
exclude: true
---

Bookdown makes it easy to write prose in markdown, code in R and compile both to well featured document that incorporates cross-references, a bibliography and captions.

## Getting started
All that's needed is an .Rmd file called index that has the following settings in its YAML header. 

```YAML
output: bookdown::gitbook
site: bookdown::bookdown_site
```

Once ready, select the Build tab in RStudio (the top right quadrent of the interface on my setup), and click 'Build Book'.
Bookdown will look for .Rmd files in the sathe files to same directory and bundle them into a file (`_main.Rmd`) it creates itself. 
Each .Rmd file will constitute a single chapter, with the first level header forming the chapter title and second level headers forming section titales.

**Important note:** The bookdown files should not have their own YAML headers as with a normal .Rmd file, this causes the chapter sectioning to fail. 

If you haven't used RStudio to create a bookdown project, then you can build a book by doing

```{r, eval = FALSE}
bookdown::render_book("index.Rmd")
```

## Further configurations to output

To fully control the output of bookdown, you need two YAML files: 

 * `_output.yml`
 * `_bookdown.yml`
 
I found Sean Kross's blog post [How to Start a Bookdown Book](http://seankross.com/2016/11/17/How-to-Start-a-Bookdown-Book.html) very useful.
`_output.yml` contains the configurations to different output types and `_bookdown.yml`
contains the list of chapters to include in the book

## Output to MS Word

In the `_output.yml` file, put the following line:

```YAML
bookdown::word_document2: default
```

`word_document2` appears to take all the same arguments of Rmarkdown to Word, so one can include a template, toc and figure settings in the same way as in Rmarkdown.
I'm not certain that the table of contents works at the moment though. 


```YAML
bookdown::word_document2: 
  fig_caption: yes
  fig_height: 6
  fig_width: 8
  reference_docx: mytemplate.docx
  toc: true
  toc_depth: 3
```