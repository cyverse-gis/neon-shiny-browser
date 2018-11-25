datesTable <- function(dates, process, selected, target) {
  years <- NULL
  for (date in dates) {
    years <- c(years, strsplit(date, "-")[[1]][1])
  }
  years <- unique(years)
  years <- years[order(years)]
  # For creating data table with date availabilities
  if (process == "table") {
    date_tables <- character(12)
    for (year in years) {
      dates_year <- dates[grepl(year, dates)]
      if (sum(dates %in% paste0(year, "-01"))) {
        Jan <- "✅"
      } else {
        Jan <- NA
      }
      if (sum(dates %in% paste0(year, "-02"))) {
        Feb <- "✅"
      } else {
        Feb <- NA
      }
      if (sum(dates %in% paste0(year, "-03"))) {
        Mar <- "✅"
      } else {
        Mar <- NA
      }
      if (sum(dates %in% paste0(year, "-04"))) {
        Apr <- "✅"
      } else {
        Apr <- NA
      }
      if (sum(dates %in% paste0(year, "-05"))) {
        May <- "✅"
      } else {
        May <- NA
      }
      if (sum(dates %in% paste0(year, "-06"))) {
        Jun <- "✅"
      } else {
        Jun <- NA
      }
      if (sum(dates %in% paste0(year, "-07"))) {
        Jul <- "✅"
      } else {
        Jul <- NA
      }
      if (sum(dates %in% paste0(year, "-08"))) {
        Aug <- "✅"
      } else {
        Aug <- NA
      }
      if (sum(dates %in% paste0(year, "-09"))) {
        Sep <- "✅"
      } else {
        Sep <- NA
      }
      if (sum(dates %in% paste0(year, "-10"))) {
        Oct <- "✅"
      } else {
        Oct <- NA
      }
      if (sum(dates %in% paste0(year, "-11"))) {
        Nov <- "✅"
      } else {
        Nov <- NA
      }
      if (sum(dates %in% paste0(year, "-12"))) {
        Dec <- "✅"
      } else {
        Dec <- NA
      }
      year_table <- data.frame(year = c(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec))
      date_tables <- cbind(date_tables, year_table)
    }
    return(date_tables[-1])
    
  } else if (process == "regular") {
    if (target == "cell") {
      year <- years[selected[2]]
      month <- selected[1]
      if (month %in% 1:9) {
        month <- paste0("0", month)
      }
      return(paste(year, month, sep = "-")) 
    } else if (target == "row") {
      month <- selected
      if (month %in% 1:9) {
        month <- paste0("0", month)
      }
      return(dates[grepl(pattern = paste0("-", month), x = dates)])
    } else if (target == "column") {
      year <- years[selected]
      return(dates[grepl(pattern = paste0(year), x = dates)])
    }
  } else if (process == "AOP") {
    return(years[selected])
  }
}
