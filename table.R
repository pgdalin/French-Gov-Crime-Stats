# Table creation - Function ----------------------------------------------------

table <- function(data, col1 = "indicateur", col2 = "unite_de_compte",
                  crime, status, fun, y_var = NULL) {
  
  if (!is.data.frame(data)) {
    stop("The 'data' argument must be a data frame.")
  }
  
  use_codgeo <- "CODGEO_2024" %in% names(data)
  required_cols <- c(col1, col2, "annee", if (use_codgeo) "CODGEO_2024" else "Code_departement")
  missing_cols <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  }
  
  tryCatch({
    if (!crime %in% unique(data[[col1]])) {
      stop(paste("Crime value not found:", crime))
    }
    if (!status %in% unique(data[[col2]])) {
      stop(paste("Status value not found:", status))
    }
  }, error = function(e) {
    stop("Error during crime/status validation: ", conditionMessage(e))
  })
  
  if (use_codgeo) {
    data <- subset(data, !(CODGEO_2024 %in% c(69123, 75056, 13055)))
  }
  
  data <- tryCatch({
    subset(data, data[[col1]] == crime & data[[col2]] == status)
  }, error = function(e) {
    stop("Error filtering data: ", conditionMessage(e))
  })
  
  if (nrow(data) == 0) {
    stop("No data remaining after filtering by crime and status.")
  }
  
  zones <- if (use_codgeo) {
    list(
      "Mainland France" = function(d) {
        dept <- substr(d$CODGEO_2024, 1, 2)
        dept3 <- substr(d$CODGEO_2024, 1, 3)
        dept_full <- ifelse(dept3 %in% c("2A", "2B"), dept3, dept)
        dept_full %in% c(sprintf("%02d", 1:95), "2A", "2B")
      },
      "City of Paris" = function(d) {
        substr(d$CODGEO_2024, 1, 5) %in% as.character(75101:75120)
      },
      "Ile-de-France" = function(d) {
        substr(d$CODGEO_2024, 1, 2) %in% c("75", "77", "78", "91", "92", "93", "94", "95")
      },
      "Overseas territories" = function(d) {
        dept <- substr(d$CODGEO_2024, 1, 2)
        !(dept %in% c(sprintf("%02d", 1:95), "2A", "2B"))
      }
    )
  } else {
    list(
      "Mainland France" = function(d) d$Code_departement %in% c(sprintf("%02d", 1:95), "2A", "2B"),
      "City of Paris" = function(d) d$Code_departement == "75",
      "Ile-de-France" = function(d) d$Code_departement %in% c("75", "77", "78", "91", "92", "93", "94", "95"),
      "Overseas territories" = function(d) !d$Code_departement %in% c(sprintf("%02d", 1:95), "2A", "2B")
    )
  }
  
  aggregated_tables <- list()
  
  for (zone_name in names(zones)) {
    
    zone_data <- tryCatch({
      data[zones[[zone_name]](data), ]
    }, error = function(e) {
      warning(sprintf("Failed to subset data for zone '%s': %s", zone_name, conditionMessage(e)))
      return(NULL)
    })
    
    if (is.null(zone_data) || nrow(zone_data) == 0) next
    
    geo_col <- if (use_codgeo) "CODGEO_2024" else "Code_departement"
    
    dependent_var <- tryCatch({
      if (!is.null(y_var) &&
          y_var %in% names(zone_data) &&
          is.numeric(zone_data[[y_var]])) {
        y_var
      } else {
        if (!is.null(y_var)) {
          warning(sprintf("y_var '%s' not available or not numeric in zone '%s' - using default.", y_var, zone_name))
        }
        candidates <- setdiff(names(zone_data), c("annee", geo_col, col1, col2))
        numeric_vars <- candidates[sapply(zone_data[candidates], is.numeric)]
        if (length(numeric_vars) == 0) stop("No numeric variable available.")
        numeric_vars[1]
      }
    }, error = function(e) {
      warning(sprintf("Skipping zone '%s': %s", zone_name, conditionMessage(e)))
      return(NULL)
    })
    
    if (is.null(dependent_var)) next
    
    form <- reformulate("annee", response = dependent_var)
    
    aggregated <- tryCatch({
      aggregate(form, data = zone_data, FUN = fun)
    }, error = function(e) {
      warning(sprintf("Aggregation failed in '%s': %s", zone_name, conditionMessage(e)))
      return(NULL)
    })
    
    if (!is.null(aggregated)) {
      aggregated_tables[[zone_name]] <- aggregated
    }
  }
  
  if (length(aggregated_tables) == 0) {
    message("No tables to display.")
    return(invisible(NULL))
  }
  
  table_grobs <- lapply(names(aggregated_tables), function(zone) {
    tryCatch({
      tg <- gridExtra::tableGrob(aggregated_tables[[zone]], rows = NULL)
      title <- grid::textGrob(zone, gp = grid::gpar(fontsize = 14, fontface = "bold"))
      padding <- grid::unit(5, "mm")
      tg <- gtable::gtable_add_rows(tg, heights = grid::grobHeight(title) + padding, pos = 0)
      gtable::gtable_add_grob(tg, title, 1, 1, 1, ncol(tg))
    }, error = function(e) {
      warning(sprintf("Failed to create table for zone '%s': %s", zone, conditionMessage(e)))
      return(NULL)
    })
  })
  
  valid_grobs <- Filter(Negate(is.null), table_grobs)
  
  tryCatch({
    gridExtra::grid.arrange(grobs = valid_grobs, nrow = 1)
  }, error = function(e) {
    warning("Failed to render table grid: ", conditionMessage(e))
  })
  
  invisible(aggregated_tables)
}
