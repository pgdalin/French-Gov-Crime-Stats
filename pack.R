# Packages ----------------------------------------------------------------

# A function running the list of necessary packages. Remove the '#' using the ALT key to uncomment it.

pack <- function() {
  
  packages <- c("readxl", "ggplot2", "gridExtra", "gtable", "scales", "readr")
  installed <- installed.packages()[, "Package"]
  to_load <- setdiff(packages, installed)
  
  if (length(to_load) > 0) {
    install.packages(to_load)
  }
  lapply(packages, require, character.only = TRUE)
}
