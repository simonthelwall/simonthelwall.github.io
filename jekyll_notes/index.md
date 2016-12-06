---
layout: page
title: jekyll notes
tagline: Things I want to remember about using jekyll
description: Some notes about using jekyll in a fairly disorganised collection
---

I'm seriously impressed with Jekyll. 
At a very basic level it takes a bunch of markdown documents and serves them as web pages. 
It makes building a clean looking website ridiculously easy. 
That said, I didn't find it easy to set up. 
I had an SSL error that could only be resolved by uninstalling Ruby and reinstalling the latest gemfile. 

Once up and running it's been a dream. 

* TOC
{:toc}

## Inserting a table of contents
Inset a single-item list where you want the toc to go. 
Then put `{:toc}` directly underneath, so that the whole thing looks like:

```md
* TOC
{:toc}
```

This looks for headers indicated by the double hash and uses them to populate the table. 

## Subpages

There are two options for having subpages. 
The first is to have separately named pages in the root directory. 
The second is to have subfolders, each containing a file named `index.md`. 
Other subpages can then live in those subfolder. 
This is the approach I've taken. 
However, there is a slight downside to this, having multiple files named `index.md` can make it tricky to figure out what's changing in git bash. 
Once you've set up a folder with a sub-page you can link to it with the usual markdown linking approach.

```md
 - [bookdown](bookdown.md)
```