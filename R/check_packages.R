# CHECK PACAKGE ----
# Check to see if packages are installed. Install them if they are not, then load them into the R session.

#' Load packages
#'
#' @description
#' Installs and loads multiple R packages.
#'
#' @param pkg Vector of package names.
#' @param repo Repository mirror from where the package needs to be installed.
#'
#' @details
#'  - Package names must be in String format
#'  - Repository default is cran.rstudio.com
#'
#' @examples
#'
#'  library(tidyverse)
#'
#'  check.packages(c('dplyr', 'ggplot2'))
#'
#' @export
check.packages = function(pkg, repo = "https://cran.rstudio.com/"){
  new.pkg = pkg[!(pkg %in% utils::installed.packages()[, "Package"])]
  if (length(new.pkg))
    utils::install.packages(new.pkg, repos = repo, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
