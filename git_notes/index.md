---
layout: page
title: git notes
tagline: Things I want to remember about using R
description: Some notes about using R in a fairly disorganised collection
---

I'm not a git ninja by any means. 
These are some notes.

## Typical workflow

Create a new repository on GitHub (easy, point and click).

Create a new repository:

```git
git init
```

Add remote to local repository:

```git
git remote add origin https://github.com/simonthelwall/repository_name.git
```

Add files:

```git
git add [filename]
```

Also folders, and files within folders:

```git
git add [foldername]/*
```

Check the status of the current repository. 

```git
git status
```

Commit:

```git
git commit -m "message goes here"
```

Commit only changed files:

```git
git commit -a -m "message goes here"
```

Sync local changes with server

```git
git push [alias] [branch]
```

where alias is the alias of the remote repository and branch is the alias of the local repository. e.g. git push origin master  

Sync remote changes to local

```git
git pull [repository]
```

Copy an entirely new repository to local

```git
git clone [repository url]
```

## Removing files

```git
git rm file.txt
git commit -m "Removed file: file.txt"
```

## Branching


Create a new branch and move to it. 

```git
git checkout -b [new branch name]
```

## Pushing branches to a remote

```git
git push origin [branchname]
```

Once work has been completed in a branch it can be merged into master

```git
git checkout master
git merge [branchname]
git push origin master
```

The temporary branch can then be deleted (depending on how brave you are feeling)

```git
git branch -d [branchname]
```

This can be done from the master branch.

## Resolving conflicts

I'm not great at this at all. I've installed meld from the software centre and configured git to use meld as the merge tool using

```git
git config --global merge.tool "meld"
git config --global mergetool.meld.path "/c/Program Files (x86)/meld/meld.exe"
```

but haven't tried it as yet. The central pane should provide the final version, and the left-hand pane the local version. 
When a conflict does occur, then one should simply have to do 

```git
git mergetool
```

and meld will launch. 

## Committing the fact that files have been manually deleted
Manually deleting files leaves their presence on the git repo, making your `git status` results look messy. 
One can remove multiple deleted files from the repo using the following command. 

```git
git rm $(git -ls --deleted)
```