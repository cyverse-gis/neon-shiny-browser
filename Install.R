# Check R version requirement
if (getRversion() < "4.0.0") {
  stop("This application requires R version 4.0.0 or higher. Current version: ", getRversion())
}

# Required packages with minimum versions where applicable
packages <- c('shiny','leaflet','leaflet.extras','neonUtilities','shinythemes',
              'shinyWidgets','shinyBS','shinyjs','sf','geosphere','jsonlite', 
              'dplyr', 'DT', 'crul', 'httr', 'testthat', 'markdown')

# Install missing packages
packages_needed <- packages[!(packages %in% installed.packages()[,"Package"])]
if (length(packages_needed) > 0) {
  install.packages(packages_needed, repos = "https://cloud.r-project.org/")
}

# Check neonUtilities version (requires 2.0+)
if (packageVersion("neonUtilities") < "2.0.0") {
  message("Updating neonUtilities to latest version...")
  install.packages("neonUtilities", repos = "https://cloud.r-project.org/")
}