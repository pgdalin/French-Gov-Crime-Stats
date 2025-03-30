# Packages ----------------------------------------------------------------

# A function running the list of necessary packages. Remove the '#' using the ALT key to uncomment it.

necessary_packages <- function() {
 
 packages <- c("readxl", "ggplot2", "gridExtra", "gtable", "scales", "readr")
 installed <- installed.packages()[, "Package"]
 to_load <- setdiff(packages, installed)
 
 if (length(to_load) > 0) {
   install.packages(to_load)
 }
 lapply(packages, require, character.only = TRUE)
}

# Original data --------------------------------------------------------------------

# The datasets have been gathered from the following URL : 'https://www.data.gouv.fr/fr/datasets/bases-statistiques-communale-departementale-et-regionale-de-la-delinquance-enregistree-par-la-police-et-la-gendarmerie-nationales/'

# More precisely : - 'https://www.data.gouv.fr/fr/datasets/bases-statistiques-communale-departementale-et-regionale-de-la-delinquance-enregistree-par-la-police-et-la-gendarmerie-nationales/#/resources/8debb975-02da-4bfc-808f-42d18ad76d0b'
#                  - 'https://www.data.gouv.fr/fr/datasets/bases-statistiques-communale-departementale-et-regionale-de-la-delinquance-enregistree-par-la-police-et-la-gendarmerie-nationales/#/resources/17a807b1-8c4b-4c6d-afdd-8c62f26fd2c5'

# Their references were : - '8debb975-02da-4bfc-808f-42d18ad76d0b'
#                         - '17a807b1-8c4b-4c6d-afdd-8c62f26fd2c5'

# Last updated the 27 March 2025

# Loading it --------------------------------------------------------------

raw_df_com <- read_csv2("~/data/gouv/gouv_crime/donnee-data.gouv-2024-geographie2024-produit-le2025-03-14.csv")
raw_df_dep <- read_csv2("~/data/gouv/gouv_crime/donnee-dep-data.gouv-2024-geographie2024-produit-le2025-03-14.csv")

# Graph generation - Main comment ------------------------------------------------------------

# This is a function to generate 2x2 plots of predefined zones of France with as a basis crime data.
# To know which are the correct combination of the arguments 'cime' and 'status', run this command : 'unique(cbind(df$indicateur, df$unite_de_compte))'.
# It will return all possibilities for you to generate plots. 

# note : I'm just practicing r-base, the code could probably be improved a lot and might be full of mistakes, be careful using the output.

# Graph generation - Function ----------------------------------------------------------

FR_gouv_crime_graph <- function(data, col1 = "indicateur", col2 = "unite_de_compte", crime, status, fun, y_var = NULL) {
  
  # Check that the input is a data frame
  if (!is.data.frame(data)) {
    stop("The 'data' argument must be a data frame.")
  }
  
  # Check if fine-grained geography column is present
  use_codgeo <- "CODGEO_2024" %in% names(data)
  
  # Dynamically define required columns depending on the dataset's structure
  required_cols <- c(col1, col2, "annee", if (use_codgeo) "CODGEO_2024" else "Code_departement")
  missing_cols <- setdiff(required_cols, names(data))
  
  # Stop execution if any required columns are missing
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns in the dataset:", paste(missing_cols, collapse = ", ")))
  }
  
  # Check if the 'crime' value exists in the specified column
  if (!crime %in% unique(data[[col1]])) {
    stop(paste("Value for", col1, "not found:", crime))
  }
  
  # Check if the 'status' value exists in the specified column
  if (!status %in% unique(data[[col2]])) {
    stop(paste("Value for", col2, "not found:", status))
  }
  
  # Remove major cities with arrondissements (Marseille, Lyon, Paris)
  if (use_codgeo) {
    data <- subset(data, !(CODGEO_2024 %in% c(69123, 75056, 13055)))
  }
  
  # Filter dataset by selected crime and status
  data <- subset(data, data[[col1]] == crime & data[[col2]] == status)
  
  # Stop if nothing remains after filtering
  if (nrow(data) == 0) {
    stop("No data remaining after filtering by crime and status.")
  }
  
  # Define geographic zones based on dataset structure
  zones <- if (use_codgeo) {
    list(
      "France Métro" = function(data) {
        dept <- substr(data$CODGEO_2024, 1, 2)
        dept3 <- substr(data$CODGEO_2024, 1, 3)
        dept_full <- ifelse(dept3 %in% c("2A", "2B"), dept3, dept)
        dept_full %in% c(sprintf("%02d", 1:95), "2A", "2B")
      },
      "Ville de Paris" = function(data) {
        substr(data$CODGEO_2024, 1, 5) %in% as.character(75101:75120)
      },
      "Ile-de-France" = function(data) {
        dept <- substr(data$CODGEO_2024, 1, 2)
        dept %in% c("75", "77", "78", "91", "92", "93", "94", "95")
      },
      "Hors France Métro" = function(data) {
        dept <- substr(data$CODGEO_2024, 1, 2)
        !(dept %in% c(sprintf("%02d", 1:95), "2A", "2B"))
      }
    )
  } else {
    list(
      "France Métro" = function(data) {
        data$Code_departement %in% c(sprintf("%02d", 1:95), "2A", "2B")
      },
      "Ville de Paris" = function(data) {
        data$Code_departement == "75"
      },
      "Ile-de-France" = function(data) {
        data$Code_departement %in% c("75", "77", "78", "91", "92", "93", "94", "95")
      },
      "Hors France Métro" = function(data) {
        !(data$Code_departement %in% c(sprintf("%02d", 1:95), "2A", "2B"))
      }
    )
  }
  
  # Initialize lists to store aggregate data and plots
  stock <- list()
  plots <- list()
  
  # Loop over each geographic zone
  for (zone_name in names(zones)) {
    
    # Filter data for the current zone
    subset_data <- data[zones[[zone_name]](data), ]
    
    # Skip to next zone if no data matches
    if (nrow(subset_data) == 0) next
    
    # Determine which column to exclude from aggregation
    geo_col <- if (use_codgeo) "CODGEO_2024" else "Code_departement"
    
    # Determine which variable to aggregate (dependent variable)
    if (!is.null(y_var)) {
      if (!y_var %in% names(subset_data)) {
        warning(paste("Dependent variable", y_var, "not found in", zone_name, "- skipping."))
        next
      }
      if (!is.numeric(subset_data[[y_var]])) {
        warning(paste("Dependent variable", y_var, "is not numeric in", zone_name, "- skipping."))
        next
      }
      dependent_var <- y_var
    } else {
      # Find numeric columns available for aggregation
      vars_candidates <- setdiff(names(subset_data), c("annee", geo_col, col1, col2))
      numeric_vars <- vars_candidates[sapply(subset_data[vars_candidates], is.numeric)]
      
      if (length(numeric_vars) == 0) {
        warning(paste("No numeric variable to aggregate in", zone_name))
        next
      }
      
      dependent_var <- numeric_vars[1]  # Default to the first numeric variable found
    }
    
    # Create a formula like: dependent_var ~ annee
    form <- reformulate("annee", response = dependent_var)
    
    # Aggregate by year using the provided function (e.g., mean, sum)
    stock[[zone_name]] <- aggregate(form, data = subset_data, FUN = fun)
    
    # Generate a trend plot for the zone
    plots[[zone_name]] <- ggplot(stock[[zone_name]], aes(x = annee, y = .data[[dependent_var]])) +
      geom_point(color = "black", size = 2) +
      geom_smooth(method = "loess", color = "#0072B2", fill = "#0072B2", alpha = 0.2, linewidth = 1) +
      scale_y_continuous(
        labels = label_number(big.mark = " ", decimal.mark = ","),
        expand = expansion(mult = c(0, 0.05))
      ) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
      labs(
        title = paste(zone_name, "\nTrend of", crime, status, "\n2016 to 2024"),
        x = "Years",
        y = "Count"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text = element_text(size = 9),
        strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()
      )
  }
  
  # Display the generated plots, or show a message if none were created
  if (length(plots) > 0) {
    gridExtra::grid.arrange(grobs = plots, ncol = 2)
  } else {
    message("No plots to display.")
  }
}


# Table creation - Main comment ------------------------------------------------------------

# This is a function to generate 4x1 tables of predefined zones of France with as a basis crime data.
# To know which are the correct combination of the arguments 'cime' and 'status', run this command : 'unique(cbind(df$indicateur, df$unite_de_compte))'.
# It will return all possibilities for you to generate plots. 

# note : I'm just practicing r-base, the code could probably be improved a lot and might be full of mistakes, be careful using the output.

# Table creation - Function ----------------------------------------------------

FR_gouv_crime_table <- function(data, col1 = "indicateur", col2 = "unite_de_compte", crime, status, fun, y_var = NULL) {
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
      "France Métro" = function(d) {
        dept <- substr(d$CODGEO_2024, 1, 2)
        dept3 <- substr(d$CODGEO_2024, 1, 3)
        dept_full <- ifelse(dept3 %in% c("2A", "2B"), dept3, dept)
        dept_full %in% c(sprintf("%02d", 1:95), "2A", "2B")
      },
      "Ville de Paris" = function(d) {
        substr(d$CODGEO_2024, 1, 5) %in% as.character(75101:75120)
      },
      "Ile-de-France" = function(d) {
        substr(d$CODGEO_2024, 1, 2) %in% c("75", "77", "78", "91", "92", "93", "94", "95")
      },
      "Hors Métro" = function(d) {
        dept <- substr(d$CODGEO_2024, 1, 2)
        !(dept %in% c(sprintf("%02d", 1:95), "2A", "2B"))
      }
    )
  } else {
    list(
      "France Métro" = function(d) d$Code_departement %in% c(sprintf("%02d", 1:95), "2A", "2B"),
      "Ville de Paris" = function(d) d$Code_departement == "75",
      "Ile-de-France" = function(d) d$Code_departement %in% c("75", "77", "78", "91", "92", "93", "94", "95"),
      "Hors Métro" = function(d) !d$Code_departement %in% c(sprintf("%02d", 1:95), "2A", "2B")
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
