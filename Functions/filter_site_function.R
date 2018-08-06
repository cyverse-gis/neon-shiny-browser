filter_site <- function(site) {
  boolean_list <- NULL
  for (i in 1:nrow(NEONproducts_product)) {
    if (any(NEONproducts_product$siteCodes[[i]]$siteCode %in% site)) {
      boolean_list <- c(boolean_list, TRUE)
    } else {
      boolean_list <- c(boolean_list, FALSE)
    }
  }
  return(boolean_list)
}