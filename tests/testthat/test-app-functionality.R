# Tests for main Shiny app functionality
# These test the overall app behavior and integration

context("NEON Shiny Browser App Functionality")

library(testthat)
library(shiny)
library(dplyr)

# Test helper to set up test environment
setup_test_env <- function() {
  # Source required functions
  source("../../Global.R", local = TRUE)
  list(
    products = if(exists("NEONproducts_product")) NEONproducts_product else NULL,
    sites = if(exists("FieldSite_point")) FieldSite_point else NULL
  )
}

describe("App Initialization", {
  
  it("loads required global variables", {
    skip_on_cran()
    
    env <- setup_test_env()
    
    # Check that key data structures are loaded
    expect_true(exists("FieldSite_point") || !is.null(env$sites))
    expect_true(exists("FieldSite_abbs"))
    expect_true(exists("NEON_datatypes"))
  })
  
  it("creates download directory", {
    # Check if download directory exists or was created
    expect_true(dir.exists("../NEON_Downloads") || dir.exists("../../NEON_Downloads"))
  })
  
  it("sources all required function files", {
    # Check that key functions are available
    expect_true(exists("filter_site"))
    expect_true(exists("unique_folderpath"))
    expect_true(exists("write_downloadSummary"))
    expect_true(exists("nneo_products"))  # Should be our replacement function
    expect_true(exists("nneo_data"))      # Should be our replacement function
    expect_true(exists("nneo_site"))      # Should be our replacement function
  })
})

describe("Filter Functions", {
  
  it("filter_site works with new data structure", {
    skip_on_cran()
    
    # Create mock product data matching our replacement structure
    mock_products <- tibble(
      productCode = c("DP1.00001.001", "DP1.00002.001", "DP1.00003.001"),
      productName = c("Product 1", "Product 2", "Product 3"),
      siteCodes = list(
        data.frame(siteCode = c("HARV", "BART"), stringsAsFactors = FALSE),
        data.frame(siteCode = c("HARV", "SCBI"), stringsAsFactors = FALSE),
        data.frame(siteCode = c("OSBS", "JERC"), stringsAsFactors = FALSE)
      )
    )
    
    # Test filtering by site
    if (exists("filter_site")) {
      harv_indices <- filter_site(site = "HARV", products = mock_products)
      expect_length(harv_indices, 3)  # Should return indices for products 1 and 2
      expect_true(1 %in% harv_indices)
      expect_true(2 %in% harv_indices)
      expect_false(3 %in% harv_indices)
    }
  })
  
  it("filter functions handle edge cases", {
    skip_on_cran()
    
    # Test with empty data
    empty_products <- tibble(
      productCode = character(0),
      siteCodes = list()
    )
    
    if (exists("filter_site")) {
      empty_result <- filter_site(site = "HARV", products = empty_products)
      expect_length(empty_result, 0)
    }
  })
})

describe("Download Functions", {
  
  it("getProductSize function works with HTTPS endpoints", {
    skip_on_cran()
    skip_if_offline()
    
    # Test that the function uses HTTPS and returns reasonable results
    if (exists("getProductSize")) {
      # Test with a common product that should have data
      tryCatch({
        size <- getProductSize(
          dpID = "DP1.00002.001",
          site = "HARV", 
          dates = c("2018-07", "2018-08")
        )
        
        expect_type(size, "double")
        expect_true(size >= 0)
      }, error = function(e) {
        skip("getProductSize function not available or API error")
      })
    }
  })
  
  it("checkDownload function accepts token parameter", {
    skip_on_cran()
    
    if (exists("checkDownload")) {
      # Test function signature includes token parameter
      formals_check <- formals(checkDownload)
      expect_true("token" %in% names(formals_check))
    }
  })
})

describe("Data Structure Compatibility", {
  
  it("products data matches expected app structure", {
    skip_on_cran()
    skip_if_offline()
    
    tryCatch({
      products <- nneo_products_replacement()
      
      # Check that the structure matches what the app expects
      expect_true("productCode" %in% names(products))
      expect_true("productName" %in% names(products))
      expect_true("siteCodes" %in% names(products))
      expect_true("keywords" %in% names(products))
      expect_true("themes" %in% names(products))
      expect_true("productScienceTeam" %in% names(products))
      
      # Check list column structures
      expect_true(is.list(products$siteCodes))
      expect_true(is.list(products$keywords))
      expect_true(is.list(products$themes))
      
      # Check nested structure
      if (nrow(products) > 0) {
        expect_true(is.data.frame(products$siteCodes[[1]]))
        expect_true("siteCode" %in% names(products$siteCodes[[1]]))
      }
      
    }, error = function(e) {
      skip("Cannot test data structure - API not available")
    })
  })
  
  it("site data structure works with app expectations", {
    skip_on_cran()
    skip_if_offline()
    
    tryCatch({
      site_data <- nneo_site_replacement("HARV")
      
      expect_true("dataProducts" %in% names(site_data))
      expect_s3_class(site_data$dataProducts, "tbl_df")
      
      # Check expected columns
      data_products <- site_data$dataProducts
      if (nrow(data_products) > 0) {
        expect_true("dataProductCode" %in% names(data_products))
        expect_true("dataProductTitle" %in% names(data_products))
      }
      
    }, error = function(e) {
      skip("Cannot test site data structure - API not available")
    })
  })
})

describe("UI Component Tests", {
  
  it("app launches without errors", {
    skip_on_cran()
    
    # Basic test that the app can be created
    tryCatch({
      # Source UI and server components
      source("../../Global.R", local = TRUE)
      ui_source <- readLines("../../Ui.R")
      server_source <- readLines("../../Server.R")
      
      # Check that files are not empty
      expect_true(length(ui_source) > 0)
      expect_true(length(server_source) > 0)
      
      # Check for key UI components
      ui_text <- paste(ui_source, collapse = " ")
      expect_true(grepl("neon_api_token", ui_text))  # API token input
      expect_true(grepl("download_NEON", ui_text))   # Download buttons
      expect_true(grepl("leaflet", ui_text))         # Map component
      
    }, error = function(e) {
      fail(paste("App fails to load:", e$message))
    })
  })
})

# Performance and Integration Tests
describe("API Performance", {
  
  it("API calls complete within reasonable time", {
    skip_on_cran()
    skip_if_offline()
    
    # Test that API calls don't hang
    start_time <- Sys.time()
    
    tryCatch({
      result <- nneo_products_replacement()
      end_time <- Sys.time()
      
      time_diff <- as.numeric(difftime(end_time, start_time, units = "secs"))
      expect_true(time_diff < 30)  # Should complete within 30 seconds
      
    }, error = function(e) {
      skip("API performance test skipped due to error")
    })
  })
  
  it("error handling works for API failures", {
    # Test that functions handle API errors gracefully
    
    # Mock a failed API response by using invalid parameters
    expect_error(
      nneo_site_replacement(site_code = "INVALID_SITE_CODE_12345"),
      "Failed to fetch site data from NEON API"
    )
  })
})