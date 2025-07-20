#!/usr/bin/env Rscript
# Test runner for NEON Shiny Browser
# Run with: Rscript run_tests.R

library(testthat)

cat("Running NEON Shiny Browser Tests...\n")
cat("====================================\n\n")

# Set working directory to app root
if (basename(getwd()) != "neon-shiny-browser") {
  if (file.exists("Global.R")) {
    # Already in app directory
  } else if (file.exists("neon-shiny-browser/Global.R")) {
    setwd("neon-shiny-browser")
  } else {
    stop("Cannot find NEON Shiny Browser app directory")
  }
}

# Run tests with detailed output
test_results <- test_dir("tests/testthat", reporter = "detailed")

# Summary
cat("\n====================================\n")
cat("Test Summary:\n")
print(test_results)

# Exit with appropriate code
if (any(test_results$failed > 0)) {
  cat("Some tests failed!\n")
  quit(status = 1)
} else {
  cat("All tests passed!\n")
  quit(status = 0)
}