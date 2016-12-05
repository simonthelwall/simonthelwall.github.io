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