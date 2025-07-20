keyword_lists <- function(list) {
  .NEON_keywords <<- new.env()
  
  # Check if NEONproducts_product exists and has data
  if (!exists("NEONproducts_product") || is.null(NEONproducts_product) || nrow(NEONproducts_product) == 0) {
    warning("NEONproducts_product not loaded yet, skipping keyword list generation")
    return(NULL)
  }
  
  for (site in list) {
    products_list <- NEONproducts_product[filter_site(site = site),]
    keywords <- NULL
    
    # Check if products_list has data and keywords column exists
    if (nrow(products_list) > 0 && "keywords" %in% names(products_list)) {
      # Safely iterate through keywords
      for (i in seq_len(nrow(products_list))) {
        if (i <= length(products_list$keywords) && !is.null(products_list$keywords[[i]])) {
          keywords <- c(keywords, products_list$keywords[[i]])
        }
      }
    }
    
    # Process keywords if any were found
    if (!is.null(keywords)) {
      keywords <- unique(keywords)
      keywords <- sort(keywords)
    } else {
      keywords <- character(0)
    }
    
    assign(x = site, value = keywords, envir = .NEON_keywords)
  }
}