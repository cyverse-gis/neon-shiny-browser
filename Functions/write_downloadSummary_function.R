write_downloadSummary <- function(method, dpID, dpName, site, dates, package, size, path) {
  one <- paste0("NEON Shiny Browser ", method, " Download Summary")
  two <- paste0("Product ID: ", dpID)
  three <- paste0("Product Name: ", dpName)
  four <- paste0("Site: ", site)
  if (length(dates) == 1 & !grepl(pattern = "-", x = dates[1])) {
    five <- paste0("Year: ", dates)
  } else {
    five <- paste0("Dates: ", paste0(dates, collapse = ", "))
  }
  six <- paste0("Package: ", package)
  seven <- paste0("Download Size: ", strsplit(size, "\t")[[1]][1])
  eight <- paste0("Date and Time: ", Sys.time(), " ", Sys.timezone())
  nine <- "The NEON Shiny Browser is a multifunctional R Shiny map tool deployed locally and designed to make NEON data accessible, visualized, and easy to interact with. Check it out at https://github.com/cyverse-gis/NEON-Shiny-Browser."
  message <- c(one, "", two, three, four, five, six, seven, eight, "", nine)
  writeLines(message, paste0("../", path,"/downloadSummary"))
}