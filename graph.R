# Graph generation - Function ----------------------------------------------------------

graph <- function(data, col1 = "indicateur", col2 = "unite_de_compte",
                  crime, status, fun, y_var = NULL) {
  
  # Validate input type
  if (!is.data.frame(data)) {
    stop("The 'data' argument must be a data frame.")
  }
  
  use_codgeo <- "CODGEO_2024" %in% names(data)
  zones <- if (use_codgeo) {
    list(
      "Mainland France" = function(data) {
        dept <- substr(data$CODGEO_2024, 1, 2)
        dept3 <- substr(data$CODGEO_2024, 1, 3)
        dept_full <- ifelse(dept3 %in% c("2A", "2B"), dept3, dept)
        dept_full %in% c(sprintf("%02d", 1:95), "2A", "2B")
      },
      "City of Paris" = function(data) {
        substr(data$CODGEO_2024, 1, 5) %in% as.character(75101:75120)
      },
      "Ile-de-France" = function(data) {
        dept <- substr(data$CODGEO_2024, 1, 2)
        dept %in% c("75", "77", "78", "91", "92", "93", "94", "95")
      },
      "Overseas territories" = function(data) {
        dept <- substr(data$CODGEO_2024, 1, 2)
        !(dept %in% c(sprintf("%02d", 1:95), "2A", "2B"))
      }
    )
  } else {
    list(
      "Mainland France" = function(data) {
        data$Code_departement %in% c(sprintf("%02d", 1:95), "2A", "2B")
      },
      "City of Paris" = function(data) {
        data$Code_departement == "75"
      },
      "Ile-de-France" = function(data) {
        data$Code_departement %in% c("75", "77", "78", "91", "92", "93", "94", "95")
      },
      "Overseas territories" = function(data) {
        !(data$Code_departement %in% c(sprintf("%02d", 1:95), "2A", "2B"))
      }
    )
  }
  required_cols <- c(col1, col2, "annee", if (use_codgeo) "CODGEO_2024" else "Code_departement")
  missing_cols <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns in the dataset:", paste(missing_cols, collapse = ", ")))
  }
  
  # Validate values
  tryCatch({
    if (!crime %in% unique(data[[col1]])) {
      stop(paste("Value for", col1, "not found:", crime))
    }
    
    if (!status %in% unique(data[[col2]])) {
      stop(paste("Value for", col2, "not found:", status))
    }
  }, error = function(e) {
    stop("Error checking values for crime or status: ", conditionMessage(e))
  })
  
  if (use_codgeo) {
    data <- subset(data, !(CODGEO_2024 %in% c(69123, 75056, 13055)))
  }
  
  # Filter data
  data <- tryCatch({
    subset(data, data[[col1]] == crime & data[[col2]] == status)
  }, error = function(e) {
    stop("Error during data filtering: ", conditionMessage(e))
  })
  
  if (nrow(data) == 0) {
    stop("No data remaining after filtering by crime and status.")
  }
  
  # Defining zones
  
  stock <- list()
  plots <- list()
  
  for (zone_name in names(zones)) {
    subset_data <- tryCatch({
      data[zones[[zone_name]](data), ]
    }, error = function(e) {
      warning(paste("Failed to subset data for", zone_name, ":", conditionMessage(e)))
      next
    })
    
    if (nrow(subset_data) == 0) next
    geo_col <- if (use_codgeo) "CODGEO_2024" else "Code_departement"
    
    dependent_var <- tryCatch({
      if (!is.null(y_var)) {
        if (!y_var %in% names(subset_data)) stop()
        if (!is.numeric(subset_data[[y_var]])) stop()
        y_var
      } else {
        vars_candidates <- setdiff(names(subset_data), c("annee", geo_col, col1, col2))
        numeric_vars <- vars_candidates[sapply(subset_data[vars_candidates], is.numeric)]
        if (length(numeric_vars) == 0) stop()
        numeric_vars[1]
      }
    }, error = function(e) {
      warning(paste("No valid numeric variable found for zone", zone_name))
      next
    })
    
    form <- reformulate("annee", response = dependent_var)
    
    stock[[zone_name]] <- tryCatch({
      aggregate(form, data = subset_data, FUN = fun)
    }, error = function(e) {
      warning(paste("Aggregation failed in", zone_name, ":", conditionMessage(e)))
      next
    })
    
    plots[[zone_name]] <- tryCatch({
      ggplot(stock[[zone_name]], aes(x = annee, y = .data[[dependent_var]])) +
        geom_point(color = "black", size = 2) +
        geom_smooth(method = "loess", color = "#0072B2", fill = "#0072B2", alpha = 0.2, linewidth = 1) +
        scale_y_continuous(
          labels = scales::label_number(big.mark = " ", decimal.mark = ","),
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
    }, error = function(e) {
      warning(paste("Plotting failed for", zone_name, ":", conditionMessage(e)))
      NULL
    })
  }
  
  if (length(plots) > 0) {
    do.call(gridExtra::grid.arrange, c(plots, ncol = 2))
  } else {
    message("No plots to display.")
  }
}
