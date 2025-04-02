# Find the information related to a 'CODGEO_2024' or a 'nomCommune'

fetch <- function(d, pattern, col_name) {
  
  if (is.null(d)) {
    stop("❌ Le jeu de données est NULL.")
  }
  
  if (!is.data.frame(d)) {
    stop("❌ L'objet fourni n'est pas un data frame.")
  }
  
  if (!col_name %in% names(d)) {
    stop("❌ Colonne '", col_name, "' introuvable dans : ", paste(names(d), collapse = ", "))
  }
  
  if (!is.character(d[[col_name]])) {
    warning("⚠️ La colonne '", col_name, "' n'est pas de type caractère. Conversion automatique.")
  }
  
  column_values <- as.character(d[[col_name]])
  
  matches <- tryCatch({
    grep(pattern, column_values)
  }, error = function(e) {
    message("❗ Une erreur est survenue pendant la recherche : ", conditionMessage(e))
    return(integer(0))
  })
  
  if (length(matches) == 0) {
    message("ℹ️ Aucun résultat ne correspond au motif '", pattern, "'.")
    return(invisible(NULL))
  }

  result <- d[matches, c(2, 3), drop = FALSE]
  print(result)
  
  invisible(result)
  
}
