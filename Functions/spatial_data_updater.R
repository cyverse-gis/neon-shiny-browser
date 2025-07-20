# NEON Spatial Data Updater
# This module downloads and caches NEON spatial datasets with version checking

library(httr)
library(sf)
library(jsonlite)

# Configuration for NEON spatial datasets
NEON_SPATIAL_CONFIG <- list(
  field_boundaries = list(
    name = "Field Site Boundaries",
    url = "https://www.neonscience.org/sites/default/files/Field_Sampling_Boundaries_202503.zip",
    filename = "Field_Sampling_Boundaries_202503.zip",
    cache_dir = "NEON-data/field_boundaries/",
    extract_to = "field_boundaries",
    version_file = "field_boundaries_version.json"
  ),
  tos_plots = list(
    name = "TOS Sampling Locations",
    url = "https://www.neonscience.org/sites/default/files/All_NEON_TOS_Plots_V11.zip", 
    filename = "All_NEON_TOS_Plots_V11.zip",
    cache_dir = "NEON-data/tos_plots/",
    extract_to = "tos_plots",
    version_file = "tos_plots_version.json"
  ),
  domain_polygons = list(
    name = "Domain Polygons",
    url = "https://www.neonscience.org/sites/default/files/NEONDomains_2024.zip",
    filename = "NEONDomains_2024.zip", 
    cache_dir = "NEON-data/domains/",
    extract_to = "domains",
    version_file = "domains_version.json"
  ),
  flight_boundaries = list(
    name = "Flight Boundaries",
    url = "https://www.neonscience.org/sites/default/files/AOP_flightBoxes_0.zip",
    filename = "AOP_flightBoxes_0.zip",
    cache_dir = "NEON-data/flight_boundaries/", 
    extract_to = "flight_boundaries",
    version_file = "flight_boundaries_version.json"
  ),
  aquatic_watersheds = list(
    name = "Aquatic Watersheds", 
    url = "https://www.neonscience.org/sites/default/files/NEONAquaticWatershed_1.zip",
    filename = "NEONAquaticWatershed_1.zip",
    cache_dir = "NEON-data/aquatic_watersheds/",
    extract_to = "aquatic_watersheds", 
    version_file = "aquatic_watersheds_version.json"
  )
)

#' Check if we need to update spatial data
#' @param dataset_config Configuration for the dataset
#' @param force_update Force download even if cache exists
#' @return TRUE if update needed, FALSE if cache is current
check_spatial_data_update <- function(dataset_config, force_update = FALSE) {
  
  if (force_update) {
    return(TRUE)
  }
  
  # Check if cache directory and version file exist
  cache_dir <- dataset_config$cache_dir
  version_file <- file.path(cache_dir, dataset_config$version_file)
  
  if (!dir.exists(cache_dir) || !file.exists(version_file)) {
    return(TRUE)
  }
  
  # Check cache age (update if older than 30 days)
  cache_age <- difftime(Sys.Date(), file.info(version_file)$mtime, units = "days")
  if (cache_age > 30) {
    message(sprintf("Cache for %s is %d days old, checking for updates", 
                    dataset_config$name, as.numeric(cache_age)))
    return(TRUE)
  }
  
  return(FALSE)
}

#' Download and extract NEON spatial dataset
#' @param dataset_config Configuration for the dataset  
#' @param timeout Download timeout in seconds
#' @return TRUE if successful, FALSE if failed
download_spatial_dataset <- function(dataset_config, timeout = 300) {
  
  tryCatch({
    # Create cache directory
    cache_dir <- dataset_config$cache_dir
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE)
    }
    
    # Download file
    zip_path <- file.path(cache_dir, dataset_config$filename)
    message(sprintf("Downloading %s from %s", dataset_config$name, dataset_config$url))
    
    response <- httr::GET(
      dataset_config$url,
      httr::timeout(timeout),
      httr::progress(),
      httr::write_disk(zip_path, overwrite = TRUE)
    )
    
    if (httr::status_code(response) != 200) {
      warning(sprintf("Failed to download %s: HTTP %d", 
                      dataset_config$name, httr::status_code(response)))
      return(FALSE)
    }
    
    # Extract zip file
    extract_dir <- file.path(cache_dir, dataset_config$extract_to)
    if (dir.exists(extract_dir)) {
      unlink(extract_dir, recursive = TRUE)
    }
    dir.create(extract_dir, recursive = TRUE)
    
    message(sprintf("Extracting %s", dataset_config$name))
    unzip(zip_path, exdir = extract_dir, overwrite = TRUE)
    
    # Create version file with metadata
    version_info <- list(
      dataset = dataset_config$name,
      url = dataset_config$url, 
      filename = dataset_config$filename,
      download_date = as.character(Sys.time()),
      file_size = file.size(zip_path),
      extract_dir = extract_dir
    )
    
    version_file <- file.path(cache_dir, dataset_config$version_file)
    writeLines(jsonlite::toJSON(version_info, pretty = TRUE), version_file)
    
    message(sprintf("Successfully downloaded and cached %s", dataset_config$name))
    return(TRUE)
    
  }, error = function(e) {
    warning(sprintf("Error downloading %s: %s", dataset_config$name, e$message))
    return(FALSE)
  })
}

#' Load spatial data from cache or download if needed
#' @param dataset_name Name of dataset from NEON_SPATIAL_CONFIG
#' @param force_update Force download even if cache exists
#' @return Path to extracted data directory or NULL if failed
load_spatial_dataset <- function(dataset_name, force_update = FALSE) {
  
  if (!(dataset_name %in% names(NEON_SPATIAL_CONFIG))) {
    stop(sprintf("Unknown dataset: %s", dataset_name))
  }
  
  config <- NEON_SPATIAL_CONFIG[[dataset_name]]
  
  # Check if update needed
  if (check_spatial_data_update(config, force_update)) {
    success <- download_spatial_dataset(config)
    if (!success) {
      # Try to use cached data if download failed
      extract_dir <- file.path(config$cache_dir, config$extract_to)
      if (dir.exists(extract_dir)) {
        warning(sprintf("Download failed for %s, using cached data", config$name))
        return(extract_dir)
      } else {
        warning(sprintf("No cached data available for %s", config$name))
        return(NULL)
      }
    }
  }
  
  # Return path to extracted data
  extract_dir <- file.path(config$cache_dir, config$extract_to)
  if (dir.exists(extract_dir)) {
    return(extract_dir)
  } else {
    warning(sprintf("No data directory found for %s", config$name))
    return(NULL)
  }
}

#' Update all NEON spatial datasets
#' @param force_update Force update all datasets
#' @param datasets Vector of dataset names to update (default: all)
#' @return List of results for each dataset
update_all_spatial_data <- function(force_update = FALSE, datasets = names(NEON_SPATIAL_CONFIG)) {
  
  message("Starting NEON spatial data update process...")
  results <- list()
  
  for (dataset_name in datasets) {
    message(sprintf("\n--- Processing %s ---", NEON_SPATIAL_CONFIG[[dataset_name]]$name))
    
    result <- tryCatch({
      path <- load_spatial_dataset(dataset_name, force_update)
      list(success = !is.null(path), path = path, error = NULL)
    }, error = function(e) {
      list(success = FALSE, path = NULL, error = e$message)
    })
    
    results[[dataset_name]] <- result
    
    if (result$success) {
      message(sprintf("✓ %s ready at %s", NEON_SPATIAL_CONFIG[[dataset_name]]$name, result$path))
    } else {
      message(sprintf("✗ %s failed: %s", NEON_SPATIAL_CONFIG[[dataset_name]]$name, 
                      result$error %||% "Unknown error"))
    }
  }
  
  # Summary
  successful <- sum(sapply(results, function(x) x$success))
  total <- length(results)
  message(sprintf("\nSpatial data update complete: %d/%d datasets successful", successful, total))
  
  return(results)
}

#' Load field site boundaries as sf object
#' @return sf object with field site boundaries
load_field_boundaries <- function() {
  data_dir <- load_spatial_dataset("field_boundaries")
  if (is.null(data_dir)) return(NULL)
  
  # Find shapefiles or geojson files
  boundary_files <- c(
    list.files(data_dir, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE),
    list.files(data_dir, pattern = "\\.geojson$", recursive = TRUE, full.names = TRUE)
  )
  
  if (length(boundary_files) == 0) {
    warning("No spatial files found in field boundaries data")
    return(NULL)
  }
  
  # Load the first found file
  tryCatch({
    boundaries <- st_read(boundary_files[1])
    message(sprintf("Loaded field boundaries with %d features", nrow(boundaries)))
    return(boundaries)
  }, error = function(e) {
    warning(sprintf("Error loading field boundaries: %s", e$message))
    return(NULL)
  })
}

#' Load TOS plots as sf object
#' @return sf object with TOS plot locations
load_tos_plots <- function() {
  data_dir <- load_spatial_dataset("tos_plots") 
  if (is.null(data_dir)) return(NULL)
  
  # Find spatial files
  plot_files <- c(
    list.files(data_dir, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE),
    list.files(data_dir, pattern = "\\.geojson$", recursive = TRUE, full.names = TRUE)
  )
  
  if (length(plot_files) == 0) {
    warning("No spatial files found in TOS plots data")
    return(NULL)
  }
  
  tryCatch({
    plots <- st_read(plot_files[1])
    message(sprintf("Loaded TOS plots with %d features", nrow(plots)))
    return(plots)
  }, error = function(e) {
    warning(sprintf("Error loading TOS plots: %s", e$message)) 
    return(NULL)
  })
}

#' Load domain polygons as sf object
#' @return sf object with NEON domain polygons
load_domain_polygons <- function() {
  data_dir <- load_spatial_dataset("domain_polygons")
  if (is.null(data_dir)) return(NULL)
  
  # Find spatial files
  domain_files <- c(
    list.files(data_dir, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE),
    list.files(data_dir, pattern = "\\.geojson$", recursive = TRUE, full.names = TRUE)
  )
  
  if (length(domain_files) == 0) {
    warning("No spatial files found in domain polygons data")
    return(NULL)
  }
  
  tryCatch({
    domains <- st_read(domain_files[1])
    message(sprintf("Loaded domain polygons with %d features", nrow(domains)))
    return(domains)
  }, error = function(e) {
    warning(sprintf("Error loading domain polygons: %s", e$message))
    return(NULL)
  })
}

#' Load flight boundaries as sf object  
#' @return sf object with flight boundaries
load_flight_boundaries <- function() {
  data_dir <- load_spatial_dataset("flight_boundaries")
  if (is.null(data_dir)) return(NULL)
  
  # Find spatial files
  flight_files <- c(
    list.files(data_dir, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE),
    list.files(data_dir, pattern = "\\.geojson$", recursive = TRUE, full.names = TRUE)
  )
  
  if (length(flight_files) == 0) {
    warning("No spatial files found in flight boundaries data")
    return(NULL)
  }
  
  # Load and combine all flight boundary files
  tryCatch({
    all_flights <- NULL
    for (file in flight_files) {
      flight_data <- st_read(file, quiet = TRUE)
      if (is.null(all_flights)) {
        all_flights <- flight_data
      } else {
        all_flights <- rbind(all_flights, flight_data)
      }
    }
    message(sprintf("Loaded flight boundaries with %d features from %d files", 
                    nrow(all_flights), length(flight_files)))
    return(all_flights)
  }, error = function(e) {
    warning(sprintf("Error loading flight boundaries: %s", e$message))
    return(NULL)
  })
}

#' Load aquatic watersheds as sf object
#' @return sf object with aquatic site watersheds  
load_aquatic_watersheds <- function() {
  data_dir <- load_spatial_dataset("aquatic_watersheds")
  if (is.null(data_dir)) return(NULL)
  
  # Find spatial files
  watershed_files <- c(
    list.files(data_dir, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE),
    list.files(data_dir, pattern = "\\.geojson$", recursive = TRUE, full.names = TRUE)
  )
  
  if (length(watershed_files) == 0) {
    warning("No spatial files found in aquatic watersheds data")
    return(NULL)
  }
  
  tryCatch({
    watersheds <- st_read(watershed_files[1])
    message(sprintf("Loaded aquatic watersheds with %d features", nrow(watersheds)))
    return(watersheds)
  }, error = function(e) {
    warning(sprintf("Error loading aquatic watersheds: %s", e$message))
    return(NULL)
  })
}

# Utility function for null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x