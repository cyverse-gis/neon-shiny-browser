filter_site <- function(site) {
  # Safe access to NEONproducts_product
  if (!exists("NEONproducts_product") || is.null(NEONproducts_product) || 
      !is.data.frame(NEONproducts_product) || nrow(NEONproducts_product) == 0) {
    return(logical(0))
  }
  
  # Vectorized approach for much better performance
  tryCatch({
    # Use sapply with simplified logic for vectorized processing
    has_site <- sapply(NEONproducts_product$siteCodes, function(site_codes_list) {
      if (is.null(site_codes_list) || !is.data.frame(site_codes_list) || 
          !"siteCode" %in% names(site_codes_list)) {
        return(FALSE)
      }
      
      site_codes <- site_codes_list$siteCode
      if (is.null(site_codes) || is.function(site_codes)) {
        return(FALSE)
      }
      
      return(any(site_codes %in% site))
    })
    
    return(has_site)
    
  }, error = function(e) {
    # If vectorized approach fails, return all FALSE
    return(rep(FALSE, nrow(NEONproducts_product)))
  })
}