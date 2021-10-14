# Install and initiate renv

if (!requireNamespace("remotes"))
  install.packages("remotes")

remotes::install_github("rstudio/renv")

renv::init()

# Install packages and snapshot

install.packages("tidyverse")

renv::snapshot()