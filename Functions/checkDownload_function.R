checkDownload <- function(dpID, site, year_month) {
  if (regexpr("DP[1-4]{1}.[0-9]{5}.001", dpID) != 1) {
    return(paste0(dpID, "is not a properly formatted data product ID. The correct format is DP#.#####.001", 
               sep = " "))
  }
  if (length(year_month) == 0) {
    return("Please enter a month.")
  }
  data <- try(nneo_data(product_code = dpID, site_code = site, year_month = year_month)$data$files, silent = TRUE)
  if (class(data) == "try-error") {
    return(data)
  } else {
    if (length(data) == 0) {
      return("There is no data at the selected site.")
    } else {
      return(TRUE)
    }
  }
}
