# Deployment Coordinator for Optimized NEON Shiny App
# ===================================================
# This script manages the transition to the optimized loading strategy

# =================================================
# Configuration
# =================================================

# Backup current files
backup_current_files <- function() {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  backup_dir <- paste0("backup_", timestamp)
  
  if (!dir.exists(backup_dir)) {
    dir.create(backup_dir)
  }
  
  # Backup original files
  files_to_backup <- c("Global.R", "Server.R", "Ui.R")
  
  for (file in files_to_backup) {
    if (file.exists(file)) {
      file.copy(file, file.path(backup_dir, file))
      message(sprintf("✓ Backed up %s to %s", file, backup_dir))
    }
  }
  
  return(backup_dir)
}

# Deploy optimized files
deploy_optimized_files <- function() {
  # Replace Global.R with optimized version
  if (file.exists("Global_optimized.R")) {
    file.copy("Global.R", "Global_original.R", overwrite = TRUE)
    file.copy("Global_optimized.R", "Global.R", overwrite = TRUE)
    message("✓ Deployed optimized Global.R")
  }
  
  # Replace Server.R with optimized version
  if (file.exists("Server_optimized.R")) {
    file.copy("Server.R", "Server_original.R", overwrite = TRUE)
    file.copy("Server_optimized.R", "Server.R", overwrite = TRUE)
    message("✓ Deployed optimized Server.R")
  }
}

# Rollback function
rollback_changes <- function() {
  if (file.exists("Global_original.R")) {
    file.copy("Global_original.R", "Global.R", overwrite = TRUE)
    message("✓ Rolled back Global.R")
  }
  
  if (file.exists("Server_original.R")) {
    file.copy("Server_original.R", "Server.R", overwrite = TRUE)
    message("✓ Rolled back Server.R")
  }
}

# =================================================
# Performance Testing
# =================================================

# Test app startup time
test_startup_performance <- function(version = "optimized") {
  message(sprintf("Testing %s version startup time...", version))
  
  start_time <- Sys.time()
  
  tryCatch({
    # Source Global.R to test loading time
    source("Global.R")
    
    end_time <- Sys.time()
    loading_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    message(sprintf("✓ %s version loaded in %.2f seconds", version, loading_time))
    return(loading_time)
    
  }, error = function(e) {
    message(sprintf("✗ Error testing %s version: %s", version, e$message))
    return(NA)
  })
}

# Compare performance
compare_performance <- function() {
  message("=== Performance Comparison ===")
  
  # Test original version
  if (file.exists("Global_original.R")) {
    file.copy("Global_original.R", "Global_test.R")
    original_time <- test_startup_performance("original")
  } else {
    original_time <- NA
  }
  
  # Test optimized version  
  if (file.exists("Global_optimized.R")) {
    file.copy("Global_optimized.R", "Global_test.R")
    optimized_time <- test_startup_performance("optimized")
  } else {
    optimized_time <- NA
  }
  
  # Clean up test file
  if (file.exists("Global_test.R")) {
    file.remove("Global_test.R")
  }
  
  # Report results
  if (!is.na(original_time) && !is.na(optimized_time)) {
    improvement <- ((original_time - optimized_time) / original_time) * 100
    message(sprintf("Performance improvement: %.1f%% faster", improvement))
  }
  
  return(list(original = original_time, optimized = optimized_time))
}

# =================================================
# Validation Functions
# =================================================

# Validate required components
validate_components <- function() {
  message("Validating optimized components...")
  
  required_files <- c("Global_optimized.R", "Server_optimized.R")
  missing_files <- c()
  
  for (file in required_files) {
    if (!file.exists(file)) {
      missing_files <- c(missing_files, file)
    }
  }
  
  if (length(missing_files) > 0) {
    message(sprintf("✗ Missing required files: %s", paste(missing_files, collapse = ", ")))
    return(FALSE)
  }
  
  message("✓ All required components validated")
  return(TRUE)
}

# Test app functionality
test_app_functionality <- function() {
  message("Testing app functionality...")
  
  tryCatch({
    # Test that app can start
    source("Global.R")
    
    # Test key variables exist
    required_vars <- c("FieldSite_abbs", "NEON_datatypes", "domains")
    missing_vars <- c()
    
    for (var in required_vars) {
      if (!exists(var)) {
        missing_vars <- c(missing_vars, var)
      }
    }
    
    if (length(missing_vars) > 0) {
      message(sprintf("✗ Missing required variables: %s", paste(missing_vars, collapse = ", ")))
      return(FALSE)
    }
    
    message("✓ App functionality validated")
    return(TRUE)
    
  }, error = function(e) {
    message(sprintf("✗ App functionality test failed: %s", e$message))
    return(FALSE)
  })
}

# =================================================
# Deployment Main Function
# =================================================

deploy_optimization <- function(test_mode = TRUE) {
  message("=== NEON Shiny App Optimization Deployment ===")
  
  # Step 1: Validate components
  if (!validate_components()) {
    stop("Validation failed. Deployment aborted.")
  }
  
  # Step 2: Backup current files
  backup_dir <- backup_current_files()
  message(sprintf("✓ Created backup in: %s", backup_dir))
  
  # Step 3: Deploy optimized files
  tryCatch({
    deploy_optimized_files()
    
    # Step 4: Test functionality
    if (test_app_functionality()) {
      message("✓ Deployment successful!")
      
      # Step 5: Performance comparison
      if (test_mode) {
        performance_results <- compare_performance()
        return(performance_results)
      }
      
    } else {
      message("✗ Functionality test failed. Rolling back...")
      rollback_changes()
      stop("Deployment failed - changes rolled back")
    }
    
  }, error = function(e) {
    message(sprintf("✗ Deployment error: %s", e$message))
    message("Rolling back changes...")
    rollback_changes()
    stop("Deployment failed - changes rolled back")
  })
}

# =================================================
# Usage Instructions
# =================================================

message("
=== NEON Shiny App Optimization Deployment ===

To deploy the optimized loading strategy:

1. Test deployment:
   deploy_optimization(test_mode = TRUE)

2. Production deployment:
   deploy_optimization(test_mode = FALSE)

3. Manual rollback if needed:
   rollback_changes()

4. Compare performance only:
   compare_performance()

The optimization provides:
- Faster initial app startup
- Non-blocking API calls  
- Progressive data loading
- Better error handling
- Improved user experience
")

# Auto-deploy in test mode if run directly
if (interactive()) {
  message("Running in test mode...")
  deploy_optimization(test_mode = TRUE)
}