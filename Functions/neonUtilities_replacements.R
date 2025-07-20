# neonUtilities Replacement Functions for Deprecated nneo Functions
# These functions provide API-compatible replacements for the deprecated nneo package
# while using the current neonUtilities and direct NEON API calls

library(httr)
library(jsonlite)
library(dplyr)

# Helper function for NEON API base URL
neon_api_base <- function() {
  "https://data.neonscience.org/api/v0"
}

#' Replacement for nneo_products()
#' 
#' Gets all NEON data products from the API and formats them to match
#' the expected nneo_products() output structure
#' 
#' @param token Optional NEON API token for faster access
#' @return tibble with product information matching nneo format
nneo_products_replacement <- function(token = NULL) {
  
  # Set up headers for API request
  headers <- list()
  if (!is.null(token) && nchar(token) > 0) {
    headers[["X-API-Token"]] <- token
  }
  
  # Make API request
  url <- paste0(neon_api_base(), "/products")
  
  tryCatch({
    if (length(headers) > 0) {
      response <- httr::GET(url, httr::add_headers(.headers = headers))
    } else {
      response <- httr::GET(url)
    }
    
    if (httr::status_code(response) != 200) {
      stop("API request failed with status: ", httr::status_code(response))
    }
    
    # Parse JSON response
    content <- httr::content(response, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content, simplifyDataFrame = TRUE, flatten = TRUE)
    
    if (is.null(data$data)) {
      stop("No product data returned from API")
    }
    
    products <- data$data
    
    # Transform to match nneo structure
    result <- products %>%
      mutate(
        # Convert keywords to list column if not already
        keywords = if("keywords" %in% names(products)) {
          lapply(keywords, function(x) if(is.null(x)) character(0) else x)
        } else {
          list(character(0))
        },
        # Convert themes to list column if not already  
        themes = if("themes" %in% names(products)) {
          lapply(themes, function(x) if(is.null(x)) character(0) else x)
        } else {
          list(character(0))
        },
        # Convert siteCodes to list column with nested data.frame
        siteCodes = if("siteCodes" %in% names(products)) {
          lapply(siteCodes, function(x) {
            if(is.null(x) || length(x) == 0) {
              data.frame(siteCode = character(0), stringsAsFactors = FALSE)
            } else {
              data.frame(siteCode = x, stringsAsFactors = FALSE)
            }
          })
        } else {
          list(data.frame(siteCode = character(0), stringsAsFactors = FALSE))
        }
      ) %>%
      # Ensure all expected columns exist
      {
        df <- .
        required_cols <- c("productCode", "productName", "productDescription", 
                          "productScienceTeam", "productScienceTeamAbbr", 
                          "keywords", "themes", "siteCodes")
        
        for(col in required_cols) {
          if(!col %in% names(df)) {
            if(col %in% c("keywords", "themes")) {
              df[[col]] <- list(character(0))
            } else if(col == "siteCodes") {
              df[[col]] <- list(data.frame(siteCode = character(0), stringsAsFactors = FALSE))
            } else {
              df[[col]] <- NA_character_
            }
          }
        }
        df
      }
    
    return(as_tibble(result))
    
  }, error = function(e) {
    stop("Failed to fetch products from NEON API: ", e$message)
  })
}

#' Replacement for nneo_data()
#' 
#' Gets data file information for a specific product/site/date combination
#' 
#' @param product_code NEON data product ID (e.g., "DP1.00098.001")
#' @param site_code NEON site code (e.g., "HARV")
#' @param year_month Year and month in YYYY-MM format
#' @param package Optional package type ("basic" or "expanded")
#' @param token Optional NEON API token
#' @return List with nested structure matching nneo format: $data$files
nneo_data_replacement <- function(product_code, site_code, year_month, package = NULL, token = NULL) {
  
  # Set up headers for API request
  headers <- list()
  if (!is.null(token) && nchar(token) > 0) {
    headers[["X-API-Token"]] <- token
  }
  
  # Build URL
  url <- paste0(neon_api_base(), "/data/", product_code, "/", site_code, "/", year_month)
  
  # Add package parameter if specified
  if (!is.null(package)) {
    url <- paste0(url, "?package=", package)
  }
  
  tryCatch({
    if (length(headers) > 0) {
      response <- httr::GET(url, httr::add_headers(.headers = headers))
    } else {
      response <- httr::GET(url)
    }
    
    if (httr::status_code(response) != 200) {
      stop("API request failed with status: ", httr::status_code(response))
    }
    
    # Parse JSON response
    content <- httr::content(response, as = "text", encoding = "UTF-8")
    api_data <- jsonlite::fromJSON(content, simplifyDataFrame = TRUE, flatten = TRUE)
    
    if (is.null(api_data$data)) {
      stop("No data returned from API")
    }
    
    # Extract files information
    files_data <- api_data$data$files
    
    if (is.null(files_data)) {
      files_data <- data.frame(
        name = character(0),
        url = character(0), 
        size = numeric(0),
        stringsAsFactors = FALSE
      )
    }
    
    # Return in nneo-compatible structure
    result <- list(
      data = list(
        files = as_tibble(files_data)
      )
    )
    
    return(result)
    
  }, error = function(e) {
    stop("Failed to fetch data from NEON API: ", e$message)
  })
}

#' Replacement for nneo_site()
#' 
#' Gets site information including available data products
#' 
#' @param site_code NEON site code (e.g., "HARV")
#' @param token Optional NEON API token
#' @return List with site information matching nneo format: $dataProducts
nneo_site_replacement <- function(site_code, token = NULL) {
  
  # Set up headers for API request
  headers <- list()
  if (!is.null(token) && nchar(token) > 0) {
    headers[["X-API-Token"]] <- token
  }
  
  # Build URL
  url <- paste0(neon_api_base(), "/sites/", site_code)
  
  tryCatch({
    if (length(headers) > 0) {
      response <- httr::GET(url, httr::add_headers(.headers = headers))
    } else {
      response <- httr::GET(url)
    }
    
    if (httr::status_code(response) != 200) {
      stop("API request failed with status: ", httr::status_code(response))
    }
    
    # Parse JSON response
    content <- httr::content(response, as = "text", encoding = "UTF-8")
    site_data <- jsonlite::fromJSON(content, simplifyDataFrame = TRUE, flatten = TRUE)
    
    if (is.null(site_data$data)) {
      stop("No site data returned from API")
    }
    
    # Extract data products information
    data_products <- site_data$data$dataProducts
    
    if (is.null(data_products)) {
      data_products <- data.frame(
        dataProductCode = character(0),
        dataProductTitle = character(0),
        stringsAsFactors = FALSE
      )
    }
    
    # Ensure required columns exist
    if (!"dataProductCode" %in% names(data_products)) {
      data_products$dataProductCode <- character(0)
    }
    if (!"dataProductTitle" %in% names(data_products)) {
      data_products$dataProductTitle <- character(0)
    }
    
    # Return in nneo-compatible structure
    result <- list(
      dataProducts = as_tibble(data_products)
    )
    
    return(result)
    
  }, error = function(e) {
    stop("Failed to fetch site data from NEON API: ", e$message)
  })
}