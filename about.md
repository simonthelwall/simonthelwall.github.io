---
layout: page
title: About
permalink: /about/
---

This is a set of pages that are intended as semi-open notebook for things I want to remember how to do. 
You will find mostly R, a reasonable amount of git, maybe a bit of TeX and some Ruby and other odds and ends.

## Handy commands for the linux terminal

Find files from command line:

```
 find / -name '*.RData' 2>/dev/null
```

`/` indicates from root directory, `-name` specifies filename, rather than any other attribute. 
`2>/dev/null` specifies command errors should be discarded. 

Grep:
Find strings from the terminal

```
grep -R -i --include='*.R' 'mse|mean square'
```

`-R` is recursive, `-i` case insensitive, `--include` limits to a certain filename/type. 
Then comes the string, and the bar symbol means OR. 

## Handy commands for the windows command prompt

Find all .R files in the current directory and subdirectories. 

```
dir /S *.R
```

`/S` indicates search subdirectories. 
Also

```
dir > myfile.txt
```

If you want to copy any output to the clipboard, then pipe to clip. 

```
dir | clip
```

If you want to sort by date modified then

```
dir /o:-d
```

Find strings in files in windows (like grep). 

```
findstr /s /c:"string" C:\folder\*.R
```

If you're looking for something in an R file, that is. 
`/s` specifies all subfolders, i.e. it is recursive. 
`/c:` specifies literal string. 
More [https://www.microsoft.com/resources/documentation/windows/xp/all/proddocs/en-us/findstr.mspx?mfr=true](here). 

If you have git installed (and you should have), you can use grep within git bash. 
The syntax is slightly different, which is a little frustrating.  
To search for a a term and limit to particular file extensions you can do something like:

```
grep -i 'summarise' -- *.r *.Rmd
```

This will case-insensitively search for the string summarise in .R and .Rmd filetypes.

This is also brilliant: http://blog.jasonmeridth.com/posts/use-git-grep-to-replace-strings-in-files-in-your-git-repository/ though I find that things run very slowly unless I specify the extension. 

```
grep -l -i 'R scripts' -- *.R *.Rmd | xargs sed -i 's/R scripts/R_scripts/g'
```

I won't pretend to understand everything that is going on here, but `-l` lists only file name and not the full path. 
`-i` is of course case insensitivity and `s/left/right/g` substitutes text on the left for the right. 
