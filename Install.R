packages <- c('shiny','leaflet','leaflet.extras','neonUtilities','shinythemes','shinyWidgets','shinyBS','shinyjs','sf','geosphere','jsonlite', 'dplyr', 'DT', 'crul')
packages_needed <- packages[!(packages %in% installed.packages()[,"Package"])]
for (package in packages_needed) {
  install.packages(package)
}