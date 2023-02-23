# Modeling range-wide environmental variation and connectivity for the northern bobwhite

This is a working repository for the analysis of Bobwhite quail (*Colinus virginianus*) habitat suitability and connectivity.

**Repository maintainer(s):**

Patrick Freeman, Scientist, Conservation Science Partners, Inc.

Justin Suraci, Lead Scientist, Conservation Science Partners, Inc.

------------------------------------------------------------------------

**For repository contributors:**

The suggested workflow for the time being is as follows:

1.  Fork this repository into your own GitHub account.
2.  Clone the repository onto your local machine.
3.  Create a branch on which you'll work on a specific task (e.g. writing code for SDM modeling).
4.  Make your changes, add code, etc.
5.  Stage a commit to capture changes.
6.  Push your changes up to GitHub.
7.  Submit a pull request to the 'main' repo with a detailed description of changes.
8.  Review pull requests and merge into 'main' repo (Admins only).

Detailed guides on how to do this both [via command line](https://docs.github.com/en/get-started/quickstart/contributing-to-projects) and using the [GitHub Desktop client](https://docs.github.com/en/desktop/contributing-and-collaborating-using-github-desktop/adding-and-cloning-repositories/cloning-and-forking-repositories-from-github-desktop) are available.

**Suggested Standard Operating Procedures (SOPs) related to file and code management:**

We are focused on making the workflows that emerge from this project (relatively) easily reproducible. Although everyone has their own way of coding - we would like to suggest some minimal set of standard filenaming and formatting to help current and future users.

***File Organization***

We will use GitHub primarily as a place for tracking code and processes and not for data storage. You are welcome to create your own data storage folder structure and we will work toward a more formalized data management system as the project evolves.

While the ultimate organization of the repo is likely to evolve over time we request that all code (inclusive of R code, notebooks, etc.) is kept within the 'code' folder. Additional sub-folders can be created in your branch and can be added to the 'main' repository through pull requests over time.

A note on the 'utils': Often, we write many small scripts that house particular functions, carry out useful data processing steps, or are otherwise auxiliary to the major tasks at hand. They are nevertheless extremely important to document! The 'utils' folder is a useful place to store these kinds of scripts and their documented functions - see the next section for suggested documentation templates.

***Script and Notebook Naming***

All code (inclusive of R code, R notebooks, Jupyter/Colab notebooks) should be given informative names using all lowercase with spaces represented using the '-' dash.

Example: prep-norbo-range-maps.R

For R scripts in particular, please use the following header to provide a minimal collection of documentation:

```{r}
## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: 
##
## Date Created: 
## Date last modified:
##
## Email: 
##
## ---------------------------
##
## Notes:
## 
##
## ---------------------------
```

Scripts written in Jupyter/Colab notebooks should use equivalent headers.

***Documenting workflows***

For the time being, we are using a [document here](https://docs.google.com/document/d/1yPoX90D-3GDC4uxYxkjF8r4zELiCaKERMUydKMRVBDs/edit#heading=h.nujcvc9bj98v) to capture decisions made by the project team regarding data sourcing, processing, and modeling to ease the construction of transparent and reproducible methods and workflows. Please make sure to document your processes (including with links to necessary scripts) in this document.

Within individual scripts - please use concise comments to describe what your code is doing!

If you have any questions, concerns, or issues, let's talk about them!
