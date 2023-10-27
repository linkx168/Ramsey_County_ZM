#'---
#' title: "Plant Data Assembly"
#' author: "Mike Verhoeven"
#' output: 
#'    html_document:
#'       toc: true
#'       theme: default
#'       toc_depth: 3
#'       toc_float:
#'           collapsed: false
#'---

#' This script will filter the full MN plants survey db for ramsey county lakes, then add in a bunch of other surveys that have been added since that project ended. 
#' 


#' # Document Preamble
#+ warning = FALSE

# load libraries ------------------------------------------------------------------
# # Load Libraries
library(data.table)
# update_dev_pkg()# remotes::install_github("Rdatatable/data.table")

# load in data -------------------------------------------------