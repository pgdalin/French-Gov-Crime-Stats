# Table creation - Main comment ------------------------------------------------------------

# This is a function to generate 4x1 tables of predefined zones of France with as a basis crime data.
# To know which are the correct combination of the arguments 'cime' and 'status', run this command : 'unique(cbind(df$indicateur, df$unite_de_compte))'.
# It will return all possibilities for you to generate plots. 

# note : I'm just practicing r-base, the code could probably be improved a lot and might be full of mistakes, be careful using the output.

# Table creation - Function ----------------------------------------------------

table <- function(data, col1 = "indicateur", col2 = "unite_de_compte", crime, status, fun, y_var = NULL) {
  # Validate that data is a data frame
  if (!is.data.frame(data)) {
    stop("The 'data' argument must be a data frame.")
  }
  
  # Determine if fine-grained geographic codes are present
  use_codgeo <- "CODGEO_2024" %in% names(data)
  
  # Define required columns based on available identifiers
  required_cols <- c(col1, col2, "annee", if (use_codgeo) "CODGEO_2024" else "Code_departement")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Validate that the specified crime and status exist in the dataset
  if (!crime %in% unique(data[[col1]])) {
    stop(paste("Crime value not found:", crime))
  }
  if (!status %in% unique(data[[col2]])) {
    stop(paste("Status value not found:", status))
  }
  
  # Remove major cities with administrative subdivisions if using CODGEO_2024
  if (use_codgeo) {
    data <- subset(data, !(CODGEO_2024 %in% c(69123, 75056, 13055)))
  }
  
  # Filter data based on crime and status
  data <- subset(data, data[[col1]] == crime & data[[col2]] == status)
  if (nrow(data) == 0) {
    stop("No data remaining after filtering by crime and status.")
  }
  
  # Define geographic zones using either fine-grained codes or department codes
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
  
  # Initialize a list to store aggregated tables (one per zone)
  aggregated_tables <- list()
  
  # Loop over each zone to aggregate data by year
  for (zone_name in names(zones)) {
    zone_data <- data[zones[[zone_name]](data), ]
    if (nrow(zone_data) == 0) next
    
    # Identify the geographic column used (for exclusion in aggregation)
    geo_col <- if (use_codgeo) "CODGEO_2024" else "Code_departement"
    
    # Determine which variable to aggregate:
    # Use y_var if provided and valid; otherwise, choose the first available numeric variable
    dependent_var <- NULL
    if (!is.null(y_var)) {
      if (y_var %in% names(zone_data) && is.numeric(zone_data[[y_var]])) {
        dependent_var <- y_var
      } else {
        warning(paste("y_var", y_var, "not available or not numeric in zone", zone_name, 
                      "- using a default numeric variable if available."))
      }
    }
    if (is.null(dependent_var)) {
      candidates <- setdiff(names(zone_data), c("annee", geo_col, col1, col2))
      numeric_vars <- candidates[sapply(zone_data[candidates], is.numeric)]
      if (length(numeric_vars) == 0) {
        warning(paste("No numeric variable available to aggregate in zone", zone_name))
        next
      }
      dependent_var <- numeric_vars[1]
    }
    
    # Create an aggregation formula (dependent_var ~ annee) and aggregate data by year
    form <- reformulate("annee", response = dependent_var)
    aggregated <- aggregate(form, data = zone_data, FUN = fun)
    
    aggregated_tables[[zone_name]] <- aggregated
  }
  
  if (length(aggregated_tables) == 0) {
    message("No tables to display.")
    return(invisible(NULL))
  }
  
  # Convert each aggregated table to a table grob for display in a grid
  library(gridExtra)
  library(grid)
  
  table_grobs <- lapply(names(aggregated_tables), function(zone) {
    tg <- tableGrob(aggregated_tables[[zone]], rows = NULL)
    # Add a title (zone name) above the table
    title <- textGrob(zone, gp = gpar(fontsize = 14, fontface = "bold"))
    padding <- unit(5, "mm")
    tg <- gtable::gtable_add_rows(tg, heights = grobHeight(title) + padding, pos = 0)
    tg <- gtable::gtable_add_grob(tg, title, 1, 1, 1, ncol(tg))
    tg
  })
  
  # Arrange the table grobs in a single horizontal row
  gridExtra::grid.arrange(grobs = table_grobs, nrow = 1)  
  
  # Optionally, return the aggregated tables invisibly for further processing
  invisible(aggregated_tables)
}