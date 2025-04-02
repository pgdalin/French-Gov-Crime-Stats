# Packages ----------------------------------------------------------------

pack <- function() {
  packages <- c("readxl", "ggplot2", "gridExtra", "gtable", "scales", "readr")
  installed <- rownames(installed.packages())
  to_install <- setdiff(packages, installed)
  
  if (length(to_install) > 0) {
    tryCatch(
      {
        install.packages(to_install)
      },
      error = function(e) {
        warning("Failed to install one or more packages: ", conditionMessage(e))
      }
    )
  }
  
  results <- lapply(packages, function(pkg) {
    tryCatch(
      {
        suppressPackageStartupMessages(require(pkg, character.only = TRUE))
      },
      error = function(e) {
        warning(sprintf("Failed to load package '%s': %s", pkg, conditionMessage(e)))
        FALSE
      }
    )
  })
  
  invisible(results)
}

