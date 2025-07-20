#!/usr/bin/env Rscript
# Test script for spatial data update system

cat("Testing NEON Spatial Data Update System\n")
cat("=======================================\n\n")

# Load required libraries
suppressMessages({
  library(httr)
  library(sf) 
  library(jsonlite)
})

# Source the spatial data functions
source("Functions/spatial_data_updater.R")
source("Functions/load_spatial_data.R")

# Test 1: Check configuration
cat("Test 1: Configuration check\n")
cat("Available datasets:\n")
for (name in names(NEON_SPATIAL_CONFIG)) {
  config <- NEON_SPATIAL_CONFIG[[name]]
  cat(sprintf("  - %s: %s\n", name, config$name))
}
cat("\n")

# Test 2: Check data status
cat("Test 2: Current data status\n")
tryCatch({
  status <- get_spatial_data_status()
  for (name in names(status)) {
    info <- status[[name]]
    cat(sprintf("  - %s: %s (cached: %s)\n", 
                info$name, 
                ifelse(info$cached, "Available", "Not cached"),
                info$cached))
  }
}, error = function(e) {
  cat(sprintf("  Error getting status: %s\n", e$message))
})
cat("\n")

# Test 3: Test download of one small dataset (domain polygons)
cat("Test 3: Testing download of domain polygons\n")
tryCatch({
  # Force update of domain polygons (usually smallest dataset)
  result <- update_spatial_dataset("domain_polygons")
  
  if (result$domain_polygons$success) {
    cat("  ✓ Domain polygons downloaded successfully\n")
    
    # Try to load the data
    domains <- load_domain_polygons()
    if (!is.null(domains)) {
      cat(sprintf("  ✓ Domain polygons loaded: %d features\n", nrow(domains)))
    } else {
      cat("  ✗ Failed to load domain polygons\n")
    }
  } else {
    cat(sprintf("  ✗ Failed to download domain polygons: %s\n", 
                result$domain_polygons$error))
  }
}, error = function(e) {
  cat(sprintf("  Error testing download: %s\n", e$message))
})
cat("\n")

# Test 4: Check cache directory structure
cat("Test 4: Cache directory structure\n")
for (name in names(NEON_SPATIAL_CONFIG)) {
  config <- NEON_SPATIAL_CONFIG[[name]]
  cache_dir <- config$cache_dir
  
  if (dir.exists(cache_dir)) {
    files <- list.files(cache_dir, recursive = TRUE)
    cat(sprintf("  %s cache (%s): %d files\n", config$name, cache_dir, length(files)))
  } else {
    cat(sprintf("  %s cache (%s): Not created\n", config$name, cache_dir))
  }
}
cat("\n")

cat("Testing complete!\n")
cat("================\n")
cat("To test the full system:\n")
cat("1. Run this script: Rscript test_spatial_update.R\n")
cat("2. Launch the Shiny app and check the 'Spatial Data' tab\n")
cat("3. Use the update controls to manage spatial datasets\n")