# Run the files

(function(fichier = c("pack.R", "graph.R", "table.R", "fetch.R")) {
  lapply(fichier, function(file) {
    tryCatch(
      {
        source(file)
        message(sprintf("Successfully sourced '%s'", file))
      },
      error = function(e) {
        warning(sprintf("Failed to source '%s': %s", file, conditionMessage(e)))
      }
    )
  })
})()

