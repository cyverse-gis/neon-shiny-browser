# Tests for neonUtilities replacement functions
# These test the new functions that replace deprecated nneo package

context("neonUtilities Replacement Functions")

# Setup test environment
library(testthat)
library(dplyr)
library(httr)

# Source the replacement functions
source("../../Functions/neonUtilities_replacements.R")

# Test helper - skip tests if NEON API is unavailable
skip_if_api_down <- function() {
  tryCatch({
    response <- httr::GET("https://data.neonscience.org/api/v0/products", 
                          httr::timeout(10))
    if (httr::status_code(response) != 200) {
      skip("NEON API is not responding")
    }
  }, error = function(e) {
    skip("NEON API is not available")
  })
}

describe("nneo_products_replacement", {
  
  it("returns a tibble with expected structure", {
    skip_if_api_down()
    
    result <- nneo_products_replacement()
    
    expect_s3_class(result, "tbl_df")
    expect_true(nrow(result) > 0)
    
    # Check required columns exist
    expected_cols <- c("productCode", "productName", "keywords", 
                      "themes", "siteCodes", "productScienceTeam")
    expect_true(all(expected_cols %in% names(result)))
  })
  
  it("returns list columns in correct format", {
    skip_if_api_down()
    
    result <- nneo_products_replacement()
    
    # Check that keywords is a list column
    expect_true(is.list(result$keywords))
    # Check that themes is a list column  
    expect_true(is.list(result$themes))
    # Check that siteCodes is a list column with data.frames
    expect_true(is.list(result$siteCodes))
    
    # Test first entry structure
    if (nrow(result) > 0) {
      expect_true(is.data.frame(result$siteCodes[[1]]))
      expect_true("siteCode" %in% names(result$siteCodes[[1]]))
    }
  })
  
  it("works with API token", {
    skip_if_api_down()
    skip_if_not(Sys.getenv("NEON_API_TOKEN") != "", "No API token available")
    
    token <- Sys.getenv("NEON_API_TOKEN")
    result <- nneo_products_replacement(token = token)
    
    expect_s3_class(result, "tbl_df")
    expect_true(nrow(result) > 0)
  })
  
  it("handles empty or invalid token gracefully", {
    skip_if_api_down()
    
    # Test with empty token
    result1 <- nneo_products_replacement(token = "")
    expect_s3_class(result1, "tbl_df")
    
    # Test with NULL token
    result2 <- nneo_products_replacement(token = NULL)
    expect_s3_class(result2, "tbl_df")
  })
})

describe("nneo_data_replacement", {
  
  it("returns nested list with data$files structure", {
    skip_if_api_down()
    
    # Use a common product/site combination that should have data
    result <- nneo_data_replacement(
      product_code = "DP1.00002.001",  # Single aspirated air temperature
      site_code = "HARV",              # Harvard Forest
      year_month = "2018-07"           # Summer 2018
    )
    
    expect_type(result, "list")
    expect_true("data" %in% names(result))
    expect_true("files" %in% names(result$data))
    expect_s3_class(result$data$files, "tbl_df")
  })
  
  it("returns empty data.frame when no files available", {
    skip_if_api_down()
    
    # Use a combination likely to have no data
    result <- nneo_data_replacement(
      product_code = "DP1.00002.001",
      site_code = "HARV", 
      year_month = "2050-01"  # Future date
    )
    
    expect_type(result, "list")
    expect_true("data" %in% names(result))
    expect_true("files" %in% names(result$data))
    expect_equal(nrow(result$data$files), 0)
  })
  
  it("includes required file information columns", {
    skip_if_api_down()
    
    result <- nneo_data_replacement(
      product_code = "DP1.00002.001",
      site_code = "HARV",
      year_month = "2018-07"
    )
    
    files_df <- result$data$files
    expected_cols <- c("name", "url", "size")
    
    # Check that expected columns exist (if there are files)
    if (nrow(files_df) > 0) {
      expect_true(all(expected_cols %in% names(files_df)))
    }
  })
  
  it("handles package parameter", {
    skip_if_api_down()
    
    result_basic <- nneo_data_replacement(
      product_code = "DP1.00002.001",
      site_code = "HARV",
      year_month = "2018-07",
      package = "basic"
    )
    
    result_expanded <- nneo_data_replacement(
      product_code = "DP1.00002.001", 
      site_code = "HARV",
      year_month = "2018-07",
      package = "expanded"
    )
    
    expect_type(result_basic, "list")
    expect_type(result_expanded, "list")
  })
})

describe("nneo_site_replacement", {
  
  it("returns list with dataProducts structure", {
    skip_if_api_down()
    
    result <- nneo_site_replacement(site_code = "HARV")
    
    expect_type(result, "list")
    expect_true("dataProducts" %in% names(result))
    expect_s3_class(result$dataProducts, "tbl_df")
  })
  
  it("includes required dataProducts columns", {
    skip_if_api_down()
    
    result <- nneo_site_replacement(site_code = "HARV")
    
    data_products <- result$dataProducts
    expected_cols <- c("dataProductCode", "dataProductTitle")
    
    if (nrow(data_products) > 0) {
      expect_true(all(expected_cols %in% names(data_products)))
    } else {
      # Even empty results should have the expected column structure
      expect_true(all(expected_cols %in% names(data_products)))
    }
  })
  
  it("returns valid site data for known sites", {
    skip_if_api_down()
    
    # Test with known NEON sites
    test_sites <- c("HARV", "BART", "SCBI", "OSBS")
    
    for (site in test_sites) {
      result <- nneo_site_replacement(site_code = site)
      
      expect_type(result, "list")
      expect_true("dataProducts" %in% names(result))
      
      # Harvard Forest should have many data products
      if (site == "HARV") {
        expect_true(nrow(result$dataProducts) > 0)
      }
    }
  })
  
  it("handles invalid site codes gracefully", {
    skip_if_api_down()
    
    expect_error(
      nneo_site_replacement(site_code = "INVALID"),
      "Failed to fetch site data from NEON API"
    )
  })
})

# Integration tests
describe("API Integration", {
  
  it("replacement functions return compatible data structures", {
    skip_if_api_down()
    
    # Test that the replacement functions return data that would work
    # with the existing filter functions in the app
    products <- nneo_products_replacement()
    
    # Test structure compatibility with filter functions
    expect_true(is.list(products$siteCodes))
    expect_true(is.list(products$keywords))
    expect_true(is.list(products$themes))
    
    # Test that siteCodes structure works with filter_site logic
    if (nrow(products) > 0) {
      first_site_codes <- products$siteCodes[[1]]
      expect_true(is.data.frame(first_site_codes))
      expect_true("siteCode" %in% names(first_site_codes))
    }
  })
  
  it("functions work together for common workflow", {
    skip_if_api_down()
    
    # Simulate a common app workflow: get products, find one with data at a site
    products <- nneo_products_replacement()
    
    if (nrow(products) > 0) {
      # Find a product available at HARV
      harv_products <- products[sapply(products$siteCodes, function(x) "HARV" %in% x$siteCode), ]
      
      if (nrow(harv_products) > 0) {
        test_product <- harv_products$productCode[1]
        
        # Try to get data for this product
        data_result <- nneo_data_replacement(
          product_code = test_product,
          site_code = "HARV", 
          year_month = "2018-07"
        )
        
        expect_type(data_result, "list")
        expect_true("data" %in% names(data_result))
        expect_true("files" %in% names(data_result$data))
      }
    }
  })
})