# Install and initiate renv ----------------------------------------------------------------

if (!requireNamespace("remotes"))
  install.packages("remotes")

remotes::install_github("rstudio/renv")

renv::init()