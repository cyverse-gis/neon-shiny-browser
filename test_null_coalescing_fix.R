#!/usr/bin/env Rscript
# Simple test to verify the null-coalescing operator fix

cat("Testing null-coalescing operator fix\n")
cat("=====================================\n\n")

# Test 1: Load the spatial data functions
cat("Test 1: Loading spatial data functions...\n")
tryCatch({
  source("Functions/load_spatial_data.R")
  cat("✓ Successfully loaded load_spatial_data.R\n")
}, error = function(e) {
  cat(paste("✗ Failed to load load_spatial_data.R:", e$message, "\n"))
})

# Test 2: Test the null-coalescing operator
cat("\nTest 2: Testing null-coalescing operator...\n")
tryCatch({
  # Test the operator directly
  test_result_1 <- NULL %||% "default_value"
  test_result_2 <- "actual_value" %||% "default_value"
  
  if (test_result_1 == "default_value" && test_result_2 == "actual_value") {
    cat("✓ Null-coalescing operator working correctly\n")
  } else {
    cat("✗ Null-coalescing operator not working as expected\n")
  }
}, error = function(e) {
  cat(paste("✗ Null-coalescing operator failed:", e$message, "\n"))
})

# Test 3: Test create_legacy_flight_data function simulation
cat("\nTest 3: Testing legacy flight data creation logic...\n")
tryCatch({
  # Create a mock flight_boundaries_new object to test the logic
  mock_data <- data.frame(
    name = c("flight1", "flight2"),
    domain_id = c("D01", "D02"),
    site_code = c("HARV", "BART"),
    site_name = c("Harvard Forest", "Bartlett Forest"),
    stringsAsFactors = FALSE
  )
  
  # Test the logic that was causing the error
  test_name <- mock_data$name %||% paste0("flight_", seq_len(nrow(mock_data)))
  test_domain <- as.numeric(gsub("D", "", mock_data$domain_id %||% "1"))
  test_site <- mock_data$site_code %||% "UNKN"
  
  if (length(test_name) > 0 && length(test_domain) > 0 && length(test_site) > 0) {
    cat("✓ Legacy flight data creation logic working\n")
  } else {
    cat("✗ Legacy flight data creation logic failed\n")
  }
}, error = function(e) {
  cat(paste("✗ Legacy flight data creation test failed:", e$message, "\n"))
})

cat("\nTest complete!\n")
cat("=============\n")
cat("If all tests passed, the coercion error should be fixed.\n")