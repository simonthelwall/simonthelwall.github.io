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
