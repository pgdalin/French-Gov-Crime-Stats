# Original data --------------------------------------------------------------------

# The datasets have been gathered from the following URL : 'https://www.data.gouv.fr/fr/datasets/bases-statistiques-communale-departementale-et-regionale-de-la-delinquance-enregistree-par-la-police-et-la-gendarmerie-nationales/'

# More precisely : - 'https://www.data.gouv.fr/fr/datasets/bases-statistiques-communale-departementale-et-regionale-de-la-delinquance-enregistree-par-la-police-et-la-gendarmerie-nationales/#/resources/8debb975-02da-4bfc-808f-42d18ad76d0b'
#                  - 'https://www.data.gouv.fr/fr/datasets/bases-statistiques-communale-departementale-et-regionale-de-la-delinquance-enregistree-par-la-police-et-la-gendarmerie-nationales/#/resources/17a807b1-8c4b-4c6d-afdd-8c62f26fd2c5'

# Their references were : - '8debb975-02da-4bfc-808f-42d18ad76d0b'
#                         - '17a807b1-8c4b-4c6d-afdd-8c62f26fd2c5'# 

# [,1]                                            [,2]              
# [1,] "Coups et blessures volontaires"                "Victime"         
# [2,] "Coups et blessures volontaires intrafamiliaux" "Victime"         
# [3,] "Autres coups et blessures volontaires"         "Victime"         
# [4,] "Violences sexuelles"                           "Victime"         
# [5,] "Vols avec armes"                               "Infraction"      
# [6,] "Vols violents sans arme"                       "Infraction"      
# [7,] "Vols sans violence contre des personnes"       "Victime entendue"
# [8,] "Cambriolages de logement"                      "Infraction"      
# [9,] "Vols de véhicules"                             "Véhicule"        
# [10,] "Vols dans les véhicules"                       "Véhicule"        
# [11,] "Vols d'accessoires sur véhicules"              "Véhicule"        
# [12,] "Destructions et dégradations volontaires"      "Infraction"      
# [13,] "Usage de stupéfiants"                          "Mis en cause"    
# [14,] "Usage de stupéfiants (AFD)"                    "Mis en cause"    
# [15,] "Trafic de stupéfiants"                         "Mis en cause"    
# [16,] "Escroqueries"                                  "Victime"     

# [,1]                                            [,2]              
# [1,] "Homicides"                                     "Victime"         
# [2,] "Tentatives d'homicides"                        "Victime"         
# [3,] "Coups et blessures volontaires"                "Victime"         
# [4,] "Coups et blessures volontaires intrafamiliaux" "Victime"         
# [5,] "Autres coups et blessures volontaires"         "Victime"         
# [6,] "Violences sexuelles"                           "Victime"         
# [7,] "Vols avec armes"                               "Infraction"      
# [8,] "Vols violents sans arme"                       "Infraction"      
# [9,] "Vols sans violence contre des personnes"       "Victime entendue"
# [10,] "Cambriolages de logement"                      "Infraction"      
# [11,] "Vols de véhicules"                             "Véhicule"        
# [12,] "Vols dans les véhicules"                       "Véhicule"        
# [13,] "Vols d'accessoires sur véhicules"              "Véhicule"        
# [14,] "Destructions et dégradations volontaires"      "Infraction"      
# [15,] "Usage de stupéfiants"                          "Mis en cause"    
# [16,] "Usage de stupéfiants (AFD)"                    "Mis en cause"    
# [17,] "Trafic de stupéfiants"                         "Mis en cause"    
# [18,] "Escroqueries"                                  "Victime" 

# Last updated the 27 March 2025

# Loading it --------------------------------------------------------------

# cities_data <- read_csv("~/data/gouv/gouv_commune/cog-communes-filtre-annee-2024-comd-coma-true-partition-departement-tri-commune.csv")
# raw_df_com <- read_csv2("~/data/gouv/gouv_crime/donnee-data.gouv-2024-geographie2024-produit-le2025-03-14.csv")
# raw_df_dep <- read_csv2("~/data/gouv/gouv_crime/donnee-dep-data.gouv-2024-geographie2024-produit-le2025-03-14.csv")


# Comments on the data ----------------------------------------------------

# We notice in raw_df_com that cities appear twice in the sense that their 'arrondissements' are also listed. See :

# Étrange comportement.

# (function() {
#   info <- grep("^Paris\\s\\d+", cities_data$nomCommune)
#   print(cities_data$nomCommune[info])
# }) ()

# [1] "Paris 1er Arrondissement" "Paris 2e Arrondissement" 
# [3] "Paris 3e Arrondissement"  "Paris 4e Arrondissement" 
# [5] "Paris 5e Arrondissement"  "Paris 6e Arrondissement" 
# [7] "Paris 7e Arrondissement"  "Paris 8e Arrondissement" 
# [9] "Paris 9e Arrondissement"  "Paris 10e Arrondissement"
# [11] "Paris 11e Arrondissement" "Paris 12e Arrondissement"
# [13] "Paris 13e Arrondissement" "Paris 14e Arrondissement"
# [15] "Paris 15e Arrondissement" "Paris 16e Arrondissement"
# [17] "Paris 17e Arrondissement" "Paris 18e Arrondissement"
# [19] "Paris 19e Arrondissement" "Paris 20e Arrondissement"

# ...
# > sort(filtered_codes)
# [1] 75056 75101 75102 75103 75104 75105 75106 75107 75108 75109 75110 75111
# [13] 75112 75113 75114 75115 75116 75117 75118 75119 75120

# That is why, in the functions, you will find:

# Remove major cities with arrondissements (Marseille, Lyon, Paris)
# if (use_codgeo) {
#   data <- subset(data, !(CODGEO_2024 %in% c(69123, 75056, 13055)))
# }

# Graph generation - Main comment ------------------------------------------------------------

# This is a function to generate 2x2 plots of predefined zones of France with as a basis crime data.
# To know which are the correct combination of the arguments 'cime' and 'status', run this command : 'unique(cbind(df$indicateur, df$unite_de_compte))'.
# It will return all possibilities for you to generate plots. 

# note : I'm just practicing r-base, the code could probably be improved a lot and might be full of mistakes, be careful using the output.

# Graph generation - Function ----------------------------------------------------------

graph <- function(data, col1 = "indicateur", col2 = "unite_de_compte", crime, status, fun, y_var = NULL) {
  
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
