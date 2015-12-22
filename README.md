<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**  *generated with [DocToc](https://github.com/thlorenz/doctoc)*

- [ashlar: A workflow template for statistical computing projects](#ashlar-a-workflow-template-for-statistical-computing-projects)
    - [Step-by-step guide](#step-by-step-guide)
    - [Resources(#resources)](#resourcesresources)
  - [](#)
    - [Cloning [*ashlar*](http://github.com/jhsiao999/ashlar])](#cloning-ashlarhttpgithubcomjhsiao999ashlar)
  - [](#-1)
    - [Reset git remote directory](#reset-git-remote-directory)
  - [](#-2)
    - [Producing and publishing the website <a id = 'publish-website'></a>](#producing-and-publishing-the-website-a-id--publish-websitea)
    - [Resources <a id = 'resources'></a>](#resources-a-id--resourcesa)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->



---

## ashlar: A workflow template for statistical computing projects

[*ashlar*](http://github.com/jhsiao999/ashlar) repository is a workflow template created foe pr [Stephens Lab](http://stephenslab.uchicago.edu/) at the University of Chicago. [*ashlar*](http://github.com/jhsiao999/ashlar) 


---

## Making your own ashlar


### Cloning [*ashlar*](http://github.com/jhsiao999/ashlar]) 

[*ashlar*](http://github.com/jhsiao999/ashlar) is set up to mimic the workflow implemented in [*singleCellSeq*](https://github.com/jdblischak/singleCellSeq). Both projects adopt the popular [*rmarkdown*](http://rmarkdown.rstudio.com/) website layout.

I suggest cloning into a new folder in order to distinguish your work from the example repository.

```
git clone https://github.com/jhsiao999/ashlar.git ashlar-trial
```


### Reset git remote directory 

Remote directory of the clone is still  [*ashlar*][http://github.com/jhsiao999/ashlar]. Make sure you change the name of the remote repository.

```
git remote rm origin
git remote add origin https://github.com/jhsiao999/ashlar-trial.git
```

Create a repository at *github.com*. Then, push content of the entire directory to the *master* branch. We use *git add -f* option to force add *html* files to the master branch, such as *index.html* for table of content. The default *.gitignore* in [*ashlar*][http://github.com/jhsiao999/ashlar] ignores *htmls*. 

```
git add -f --all
git commit -m "first commit"
git push origin master
```



### Producing and publishing the website 

#### Option 1: All content for my eyes only

Open index.html. This is the homepage of your unpubished website. You are DONE!

If you choose this option, you only have the master branch. The default of the master branch is to not push *htmls, pngs, pdfs, etc*, so edit the *.gitignore* to add these files when updating the remote directory. 


#### Option 2: Publish it! Keep a two-branch workflow.

Create a branch named gh-pages. GitHub then publishes the content for you.

```
git checkout -b gh-pages 
git add -f --all
git commit -m "Build site"
git push origin gh-pages
```

Note that since the site is kept under the anaysis folder, your site address is under the analysis directory.

*https://jhsiao999.github.io/ashlar-trial/analysis*

This two-branch workflow is set up to keep the source files (such as *Rmds*) separate from the *html* pages and the output figures. It allows me to keep clean repositories: master for the source, and gh-pages only for the website. 

I mostly use RStudio to generate htmls, but when there are a large number of analysis files that need to be updated, I choose to use the simple make command. Below are two of my most recently used paths of update GitHub Pages.

Path 1: I mostly use this one when there's only a small analysis file to be updated.

```
## Work at the gh-pages branch, push website content to gh-pages,
## push source to the master
git checkout gh-pages
cd analysis
make # (or use knitr to render the Rmds)
git add -f *Rmd *html figure/*
git commit -m "add new analysis"
git push origin gh-pages

git checkout master
git merge gh-pages
git add new-analysis.Rmd index.Rmd
git commit -m "add new analysis"
git push origin master
```

Path 2: I use this when the site has not been update in a while, and I need to compile a large number of Rmds.

```
## Work at the master branch, keep all htmls local
## Push source to the master branch, use make to generate htmls
## for the gh-pages branch

git checkout master
cd analysis
git add new-analysis.Rmd index.Rmd
git commit -m "add new analysis"
git push origin master

git checkout gh-pages
git merge master
make
git add *Rmd *html figure/*
git commit -m "add new analysis"
git push origin gh-pages
```



---

## Resources 

* [John Blischak's tips on worflow management.][contrib]


[site]: http://jhsiao999.github.io/ashlar/analysis
[contrib]: https://github.com/jdblischak/singleCellSeq/blob/master/CONTRIBUTING.md
