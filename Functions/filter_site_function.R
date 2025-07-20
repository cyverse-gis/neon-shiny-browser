filter_site <- function(site) {
  message(sprintf("DEBUG: filter_site called for site: %s", paste(site, collapse=", ")))
  
  # Safe access to NEONproducts_product
  if (!exists("NEONproducts_product") || is.null(NEONproducts_product) || 
      !is.data.frame(NEONproducts_product) || nrow(NEONproducts_product) == 0) {
    message("DEBUG: NEONproducts_product not available in filter_site")
    return(logical(0))  # Return empty logical vector
  }
  
  n_products <- nrow(NEONproducts_product)
  message(sprintf("DEBUG: filter_site processing %d products", n_products))
  
  # Additional safety check for the loop
  if (is.null(n_products) || n_products <= 0) {
    message("DEBUG: n_products is NULL or <= 0, returning empty logical")
    return(logical(0))
  }
  
  boolean_list <- NULL
  for (i in seq_len(n_products)) {
    # Add bounds checking to prevent runaway loops
    if (i > 1000) {
      message(sprintf("WARNING: filter_site loop exceeded 1000 iterations, breaking at i=%d", i))
      break
    }
    
    tryCatch({
      # Safe access to siteCodes with validation
      if (i <= length(NEONproducts_product$siteCodes) && 
          !is.null(NEONproducts_product$siteCodes[[i]]) &&
          is.data.frame(NEONproducts_product$siteCodes[[i]]) &&
          "siteCode" %in% names(NEONproducts_product$siteCodes[[i]])) {
        
        site_codes <- NEONproducts_product$siteCodes[[i]]$siteCode
        if (!is.function(site_codes) && any(site_codes %in% site)) {
          boolean_list <- c(boolean_list, TRUE)
        } else {
          boolean_list <- c(boolean_list, FALSE)
        }
      } else {
        boolean_list <- c(boolean_list, FALSE)
      }
    }, error = function(e) {
      boolean_list <<- c(boolean_list, FALSE)
    })
  }
  return(boolean_list)
}