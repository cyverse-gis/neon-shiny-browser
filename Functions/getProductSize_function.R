getProductSize <- function (dpID, site, package, dates) 
{
  messages <- NA
  if (regexpr("DP[1-4]{1}.[0-9]{5}.001", dpID) != 1) {
    stop(paste(dpID, "is not a properly formatted data product ID. The correct format is DP#.#####.001", 
               sep = " "))
  }
  productUrl <- paste0("http://data.neonscience.org/api/v0/products/", 
                       dpID)
  req <- httr::GET(productUrl)
  avail <- jsonlite::fromJSON(httr::content(req, as = "text"), 
                              simplifyDataFrame = TRUE, flatten = TRUE)
  if (!is.null(avail$error$status)) {
    return(0)
  }
  month.urls <- unlist(avail$data$siteCodes$availableDataUrls)
  month.urls <- month.urls[grep(site, month.urls)]
  month_boolean <- logical(length(month.urls))
  for (i in 1:length(dates)) {
    bool <- grepl(pattern = dates[i], x = month.urls)
    month_boolean <- month_boolean | bool
  }
  month.urls <- month.urls[month_boolean]
  if (length(month.urls) == 0) {
    return(0)
  }
  zip.urls <- c(NA, NA, NA)
  incProgress(amount = 0.1)
  for (i in 1:length(month.urls)) {
    tmp <- httr::GET(month.urls[i])
    tmp.files <- jsonlite::fromJSON(httr::content(tmp, as = "text"), 
                                    simplifyDataFrame = T, flatten = T)
    if (length(tmp.files$data$files) == 0) {
      messages <- c(messages, paste("No files found for site", 
                                    tmp.files$data$siteCode, "and month", tmp.files$data$month, 
                                    sep = " "))
      next
    }
    all.zip <- grep(".zip", tmp.files$data$files$name, 
                    fixed = T)
    if (length(all.zip) == 0) {
      messages <- c(messages, paste("No zip files found for site", 
                                    tmp.files$data$siteCode, "and month", tmp.files$data$month, 
                                    sep = " "))
      next
    }
    pk <- package
    if (pk == "expanded") {
      if (length(grep(pk, tmp.files$data$files$name)) == 
          0) {
        pk <- "basic"
        messages <- c(messages, paste("No expanded package found for site ", 
                                      tmp.files$data$siteCode, " and month ", tmp.files$data$month, 
                                      ". Basic package downloaded instead.", sep = ""))
      }
    }
    which.zip <- intersect(grep(pk, tmp.files$data$files$name, 
                                fixed = T), grep(".zip", tmp.files$data$files$name, 
                                                 fixed = T))
    if (length(which.zip) == 0) {
      messages <- c(messages, paste("No basic package files found for site", 
                                    tmp.files$data$siteCode, "and month", tmp.files$data$month, 
                                    sep = " "))
      next
    }
    zip.urls <- rbind(zip.urls, cbind(tmp.files$data$files$name[which.zip], 
                                      tmp.files$data$files$url[which.zip], tmp.files$data$files$size[which.zip]))
    incProgress(amount = 1/length(month.urls))
  }
  zip.urls <- data.frame(zip.urls, row.names = NULL)
  colnames(zip.urls) <- c("name", "URL", "size")
  downld.size <- sum(as.numeric(as.character(zip.urls$size)), 
                     na.rm = T)/1e+06
  return(downld.size)
}
