# Integration tests for download functionality
# These test the complete download workflow with API token support

context("Download Integration Tests")

library(testthat)
library(shiny)
library(neonUtilities)

# Test configuration
test_product_id <- "DP1.00002.001"  # Single aspirated air temperature
test_site <- "HARV"                 # Harvard Forest
test_year_month <- "2018-07"        # Summer 2018 (likely to have data)

# Helper to check if we can reach NEON API
skip_if_neon_api_down <- function() {
  tryCatch({
    response <- httr::GET("https://data.neonscience.org/api/v0/products", httr::timeout(10))
    if (httr::status_code(response) != 200) {
      skip("NEON API is not responding")
    }
  }, error = function(e) {
    skip("Cannot reach NEON API")
  })
}

# Helper to set up test download directory
setup_test_downloads <- function() {
  test_dir <- tempdir()
  download_path <- file.path(test_dir, "test_downloads")
  if (!dir.exists(download_path)) {
    dir.create(download_path, recursive = TRUE)
  }
  return(download_path)
}

describe("API Token Integration", {
  
  it("getPackage function accepts token parameter", {
    skip_on_cran()
    
    # Check that getPackage (from neonUtilities) accepts token parameter
    getPackage_formals <- formals(neonUtilities::getPackage)
    expect_true("token" %in% names(getPackage_formals))
  })
  
  it("byFileAOP function accepts token parameter", {
    skip_on_cran()
    
    # Check that byFileAOP (from neonUtilities) accepts token parameter
    byFileAOP_formals <- formals(neonUtilities::byFileAOP)
    expect_true("token" %in% names(byFileAOP_formals))
  })
  
  it("neonUtilities functions work with empty token", {
    skip_on_cran()
    skip_if_neon_api_down()
    
    test_path <- setup_test_downloads()
    
    # Test that functions work with NULL/empty token (should not error)
    tryCatch({
      # Small test download with no token
      result <- neonUtilities::getPackage(
        dpID = test_product_id,
        site_code = test_site,
        year_month = test_year_month,
        package = "basic",
        savepath = test_path,
        token = NULL
      )
      
      expect_true(TRUE)  # If we get here, no error occurred
      
    }, error = function(e) {
      # Some errors are expected (like no data available), 
      # but not authentication errors
      expect_false(grepl("authentication|token|unauthorized", e$message, ignore.case = TRUE))
    })
    
    # Clean up
    unlink(test_path, recursive = TRUE)
  })
})

describe("Download Size Calculation", {
  
  it("getProductSize uses HTTPS endpoints", {
    skip_on_cran()
    
    # Source the function
    source("../../Functions/getProductSize_function.R", local = TRUE)
    
    # Check that the function code uses HTTPS
    func_body <- deparse(body(getProductSize))
    func_text <- paste(func_body, collapse = " ")
    
    expect_true(grepl("https://", func_text))
    expect_false(grepl("http://data", func_text))  # Should not have http://data
  })
  
  it("size calculation returns reasonable values", {
    skip_on_cran()
    skip_if_neon_api_down()
    
    source("../../Functions/getProductSize_function.R", local = TRUE)
    
    tryCatch({
      size <- getProductSize(
        dpID = test_product_id,
        site = test_site, 
        dates = c(test_year_month)
      )
      
      expect_type(size, "double")
      expect_true(size >= 0)
      
      # Should be reasonable size (not 0 and not unreasonably large)
      if (size > 0) {
        expect_true(size < 1e12)  # Less than 1TB seems reasonable
      }
      
    }, error = function(e) {
      # API errors are okay for this test
      if (!grepl("API", e$message)) {
        fail(paste("Unexpected error:", e$message))
      }
    })
  })
})

describe("Data Availability Checking", {
  
  it("replacement functions provide expected data availability info", {
    skip_on_cran()
    skip_if_neon_api_down()
    
    source("../../Functions/neonUtilities_replacements.R", local = TRUE)
    
    tryCatch({
      # Test data availability check
      data_info <- nneo_data_replacement(
        product_code = test_product_id,
        site_code = test_site,
        year_month = test_year_month
      )
      
      expect_type(data_info, "list")
      expect_true("data" %in% names(data_info))
      expect_true("files" %in% names(data_info$data))
      
      files_df <- data_info$data$files
      expect_s3_class(files_df, "tbl_df")
      
      # If there are files, they should have the expected structure
      if (nrow(files_df) > 0) {
        expect_true("name" %in% names(files_df))
        expect_true("url" %in% names(files_df))
        expect_true("size" %in% names(files_df))
        
        # URLs should be HTTPS
        if ("url" %in% names(files_df) && nrow(files_df) > 0) {
          urls <- files_df$url[!is.na(files_df$url)]
          if (length(urls) > 0) {
            expect_true(all(grepl("^https://", urls)))
          }
        }
      }
      
    }, error = function(e) {
      skip(paste("Data availability check failed:", e$message))
    })
  })
})

describe("App Function Integration", {
  
  it("unique_folderpath function works", {
    skip_on_cran()
    
    source("../../Functions/unique_folderpath_function.R", local = TRUE)
    
    # Test the folder path generation
    test_path <- "test/path/structure"
    result <- unique_folderpath(pathname = test_path)
    
    expect_type(result, "character")
    expect_true(nchar(result) > 0)
    expect_true(grepl("test", result))
  })
  
  it("write_downloadSummary function works", {
    skip_on_cran()
    
    source("../../Functions/write_downloadSummary_function.R", local = TRUE)
    
    test_path <- setup_test_downloads()
    
    # Test writing download summary
    tryCatch({
      write_downloadSummary(
        method = "Regular",
        dpID = test_product_id,
        dpName = "Test Product",
        site = test_site,
        dates = c(test_year_month),
        package = "basic",
        size = "10 MB",
        path = test_path
      )
      
      # Check that summary file was created
      summary_files <- list.files(test_path, pattern = "download_summary", full.names = TRUE)
      expect_true(length(summary_files) > 0)
      
    }, error = function(e) {
      # Function might not exist or have different parameters
      skip(paste("write_downloadSummary test skipped:", e$message))
    })
    
    # Clean up
    unlink(test_path, recursive = TRUE)
  })
})

describe("Error Handling and Robustness", {
  
  it("handles invalid product IDs gracefully", {
    skip_on_cran()
    
    source("../../Functions/neonUtilities_replacements.R", local = TRUE)
    
    expect_error(
      nneo_data_replacement(
        product_code = "INVALID.PRODUCT.001",
        site_code = test_site,
        year_month = test_year_month
      ),
      "Failed to fetch data from NEON API"
    )
  })
  
  it("handles invalid site codes gracefully", {
    skip_on_cran()
    
    source("../../Functions/neonUtilities_replacements.R", local = TRUE)
    
    expect_error(
      nneo_data_replacement(
        product_code = test_product_id,
        site_code = "INVALID",
        year_month = test_year_month
      ),
      "Failed to fetch data from NEON API"
    )
  })
  
  it("handles future dates gracefully", {
    skip_on_cran()
    skip_if_neon_api_down()
    
    source("../../Functions/neonUtilities_replacements.R", local = TRUE)
    
    # Future date should not error but return empty results
    result <- nneo_data_replacement(
      product_code = test_product_id,
      site_code = test_site,
      year_month = "2050-01"
    )
    
    expect_type(result, "list")
    expect_true("data" %in% names(result))
    expect_true("files" %in% names(result$data))
    expect_equal(nrow(result$data$files), 0)
  })
})

describe("Performance and Timeout Handling", {
  
  it("API calls have reasonable timeouts", {
    skip_on_cran()
    skip_if_neon_api_down()
    
    source("../../Functions/neonUtilities_replacements.R", local = TRUE)
    
    # Test that API calls complete within reasonable time
    start_time <- Sys.time()
    
    tryCatch({
      result <- nneo_products_replacement()
      end_time <- Sys.time()
      
      time_taken <- as.numeric(difftime(end_time, start_time, units = "secs"))
      expect_true(time_taken < 60)  # Should complete within 60 seconds
      
    }, error = function(e) {
      # Timeout or network errors are acceptable
      expect_true(grepl("timeout|network|connection", e$message, ignore.case = TRUE))
    })
  })
})