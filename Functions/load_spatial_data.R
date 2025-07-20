# Modern Spatial Data Loading for NEON Shiny Browser
# This replaces the old manual file loading with automated cache management

# Load required libraries
suppressMessages(library(sf))
suppressMessages(library(jsonlite))

# Define null-coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

source('Functions/spatial_data_updater.R')

#' Initialize all spatial data for the NEON Shiny Browser
#' @param force_update Force download of all spatial data
#' @param check_updates Check for updates (default: TRUE on app start)
load_neon_spatial_data <- function(force_update = FALSE, check_updates = TRUE) {
  
  message("Initializing NEON spatial data...")
  
  # Initialize global spatial data objects
  assign("FieldSite_boundaries", NULL, envir = .GlobalEnv)
  assign("TOS_plots", NULL, envir = .GlobalEnv) 
  assign("NEON_domains", NULL, envir = .GlobalEnv)
  assign("flight_boundaries", NULL, envir = .GlobalEnv)
  assign("aquatic_watersheds", NULL, envir = .GlobalEnv)
  
  if (check_updates) {
    # Update datasets that need updating
    update_results <- update_all_spatial_data(force_update = force_update)
  }
  
  # Load field site boundaries
  tryCatch({
    message("Loading field site boundaries...")
    FieldSite_boundaries <<- load_field_boundaries()
    if (!is.null(FieldSite_boundaries)) {
      message(sprintf("✓ Field boundaries loaded: %d features", nrow(FieldSite_boundaries)))
    }
  }, error = function(e) {
    warning(sprintf("Could not load field boundaries: %s", e$message))
  })
  
  # Load TOS plots
  tryCatch({
    message("Loading TOS sampling plots...")
    TOS_plots <<- load_tos_plots()
    if (!is.null(TOS_plots)) {
      message(sprintf("✓ TOS plots loaded: %d features", nrow(TOS_plots)))
    }
  }, error = function(e) {
    warning(sprintf("Could not load TOS plots: %s", e$message))
  })
  
  # Load domain polygons  
  tryCatch({
    message("Loading NEON domains...")
    NEON_domains <<- load_domain_polygons()
    if (!is.null(NEON_domains)) {
      message(sprintf("✓ Domain polygons loaded: %d features", nrow(NEON_domains)))
    }
  }, error = function(e) {
    warning(sprintf("Could not load domain polygons: %s", e$message))
  })
  
  # Load flight boundaries with backward compatibility
  tryCatch({
    message("Loading flight boundaries...")
    flight_boundaries_new <<- load_flight_boundaries()
    if (!is.null(flight_boundaries_new) && (is.data.frame(flight_boundaries_new) || inherits(flight_boundaries_new, "sf"))) {
      message(sprintf("✓ Flight boundaries loaded: %d features", nrow(flight_boundaries_new)))
      
      # Create backward compatibility with existing flight_data structure
      create_legacy_flight_data()
    } else {
      # Fallback to old system if new system fails
      message("WARNING: New flight boundaries failed or returned NULL, using legacy system...")
      flight_boundaries_new <<- NULL  # Ensure it's explicitly NULL
      load_legacy_flight_data()
    }
  }, error = function(e) {
    warning(sprintf("Could not load flight boundaries: %s", e$message))
    # Fallback to legacy system
    load_legacy_flight_data()
  })
  
  # Load aquatic watersheds
  tryCatch({
    message("Loading aquatic watersheds...")
    aquatic_watersheds <<- load_aquatic_watersheds()
    if (!is.null(aquatic_watersheds)) {
      message(sprintf("✓ Aquatic watersheds loaded: %d features", nrow(aquatic_watersheds)))
    }
  }, error = function(e) {
    warning(sprintf("Could not load aquatic watersheds: %s", e$message))
  })
  
  message("✓ Spatial data initialization complete")
}

#' Create legacy flight_data structure for backward compatibility
create_legacy_flight_data <- function() {
  if (is.null(flight_boundaries_new)) {
    message("WARNING: flight_boundaries_new is NULL, cannot create legacy flight data")
    return()
  }
  
  if (!is.data.frame(flight_boundaries_new) && !inherits(flight_boundaries_new, "sf")) {
    message("WARNING: flight_boundaries_new is not a valid data structure, cannot create legacy flight data")
    return()
  }
  
  tryCatch({
    # Process new flight data to match old structure
    # This maintains compatibility with existing map rendering code
    
    # Extract flight info from new data structure with safe extraction
    safe_extract <- function(obj, field, default) {
      tryCatch({
        value <- obj[[field]]
        if (is.null(value) || length(value) == 0 || is.function(value)) {
          return(default)
        }
        return(value)
      }, error = function(e) {
        return(default)
      })
    }
    
    # Get number of rows safely
    n_rows <- tryCatch({
      if (is.data.frame(flight_boundaries_new)) {
        nrow(flight_boundaries_new)
      } else if (inherits(flight_boundaries_new, "sf")) {
        nrow(flight_boundaries_new)
      } else {
        1
      }
    }, error = function(e) 1)
    
    flight_info <- data.frame(
      Name = safe_extract(flight_boundaries_new, "name", paste0("flight_", seq_len(n_rows))),
      DomainID = as.numeric(gsub("D", "", safe_extract(flight_boundaries_new, "domain_id", "1"))),
      SiteAbb = safe_extract(flight_boundaries_new, "site_code", "UNKN"),
      Site = safe_extract(flight_boundaries_new, "site_name", "Unknown Site"),
      SiteType = safe_extract(flight_boundaries_new, "site_type", "UNKNOWN"), 
      SiteType_number = safe_extract(flight_boundaries_new, "type_number", "1"),
      Priority = safe_extract(flight_boundaries_new, "priority", "1"),
      Version = safe_extract(flight_boundaries_new, "version", "1"),
      Year = safe_extract(flight_boundaries_new, "year", "2024"),
      stringsAsFactors = FALSE
    )
    
    # Combine with geometry - create sf object properly
    if (inherits(flight_boundaries_new, "sf")) {
      # If flight_boundaries_new is an sf object, use st_geometry
      flight_data <<- flight_info
      flight_data$geometry <<- st_geometry(flight_boundaries_new)
      flight_data <<- st_sf(flight_data)
    } else {
      # Fallback to simple data.frame if no geometry
      flight_data <<- flight_info
    }
    
    message("✓ Legacy flight_data structure created for backward compatibility")
    
  }, error = function(e) {
    warning(sprintf("Could not create legacy flight data structure: %s", e$message))
    load_legacy_flight_data()
  })
}

#' Fallback to old flight data loading system
load_legacy_flight_data <- function() {
  message("Using legacy flight data loading...")
  
  tryCatch({
    # First try to load new 2025 flight boundaries
    if (file.exists('NEON-data/Flightdata/Flight_boundaries_2025')) {
      message("Loading 2025 flight boundaries...")
      
      flight_filenames_all_2025 <- Sys.glob('NEON-data/Flightdata/Flight_boundaries_2025/D*')
      flight_filenames_2025 <- Sys.glob('NEON-data/Flightdata/Flight_boundaries_2025/D*.geojson')
      
      if (length(flight_filenames_2025) > 0 && file.exists("Functions/flight_function.R")) {
        source("Functions/flight_function.R")
        
        process_flight_data(flightlist_info = flight_filenames_all_2025, 
                   flightlist_geo = flight_filenames_2025, 
                   year = "2025", name = "flight_data")
        
        if (exists("flight_data")) {
          message("✓ 2025 flight data loaded successfully")
          return()
        }
      }
    }
    
    # Fallback to older flight data if 2025 not available
    if (file.exists('NEON-data/Flightdata/Flight_boundaries_2016') && 
        file.exists('NEON-data/Flightdata/Flight_boundaries_2017')) {
      
      message("Falling back to legacy 2016/2017 flight boundaries...")
      
      # Use original loading code
      flight_filenames_all_2016 <- Sys.glob('NEON-data/Flightdata/Flight_boundaries_2016/D*')
      flight_filenames_2016 <- Sys.glob('NEON-data/Flightdata/Flight_boundaries_2016/D*.geojson')
      
      # Source the flight function if it exists
      if (file.exists("Functions/flight_function.R")) {
        source("Functions/flight_function.R")
        
        process_flight_data(flightlist_info = flight_filenames_all_2016, 
                   flightlist_geo = flight_filenames_2016, 
                   year = "2016", name = "flight_data_2016")
        
        flight_filenames_all_2017 <- Sys.glob('NEON-data/Flightdata/Flight_boundaries_2017/D*')
        flight_filenames_2017 <- Sys.glob('NEON-data/Flightdata/Flight_boundaries_2017/D*.geojson')
        process_flight_data(flightlist_info = flight_filenames_all_2017, 
                   flightlist_geo = flight_filenames_2017, 
                   year = "2017", name = "flight_data_2017")
        
        if (exists("flight_data_2016") && exists("flight_data_2017")) {
          flight_data <<- rbind(flight_data_2016, flight_data_2017)
          message("✓ Legacy flight data loaded successfully")
        }
      }
    } else {
      message("No flight data found")
    }
  }, error = function(e) {
    warning(sprintf("Legacy flight data loading failed: %s", e$message))
  })
}

#' Force update of specific spatial dataset
#' @param dataset_name Name of dataset to update
update_spatial_dataset <- function(dataset_name) {
  if (!(dataset_name %in% names(NEON_SPATIAL_CONFIG))) {
    stop(sprintf("Unknown dataset: %s", dataset_name))
  }
  
  message(sprintf("Force updating %s...", NEON_SPATIAL_CONFIG[[dataset_name]]$name))
  result <- update_all_spatial_data(force_update = TRUE, datasets = dataset_name)
  
  # Reload the specific dataset
  switch(dataset_name,
    "field_boundaries" = {
      FieldSite_boundaries <<- load_field_boundaries()
    },
    "tos_plots" = {
      TOS_plots <<- load_tos_plots() 
    },
    "domain_polygons" = {
      NEON_domains <<- load_domain_polygons()
    },
    "flight_boundaries" = {
      flight_boundaries_new <<- load_flight_boundaries()
      create_legacy_flight_data()
    },
    "aquatic_watersheds" = {
      aquatic_watersheds <<- load_aquatic_watersheds()
    }
  )
  
  return(result)
}

#' Get spatial data status and cache information
get_spatial_data_status <- function() {
  status <- list()
  
  for (dataset_name in names(NEON_SPATIAL_CONFIG)) {
    config <- NEON_SPATIAL_CONFIG[[dataset_name]]
    version_file <- file.path(config$cache_dir, config$version_file)
    
    if (file.exists(version_file)) {
      version_info <- tryCatch({
        jsonlite::fromJSON(readLines(version_file, warn = FALSE))
      }, error = function(e) NULL)
      
      status[[dataset_name]] <- list(
        name = config$name,
        cached = TRUE,
        download_date = version_info$download_date %||% "Unknown",
        file_size = version_info$file_size %||% "Unknown",
        cache_dir = config$cache_dir
      )
    } else {
      status[[dataset_name]] <- list(
        name = config$name, 
        cached = FALSE,
        download_date = "Not downloaded",
        file_size = "N/A",
        cache_dir = config$cache_dir
      )
    }
  }
  
  return(status)
}