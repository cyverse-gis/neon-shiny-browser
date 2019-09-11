FROM cyversevice/shiny-geospatial

RUN R -e "install.packages(c('shiny','leaflet','leaflet.extras','neonUtilities','shinythemes','shinyWidgets','shinyBS','shinyjs','sf','geosphere','jsonlite', 'dplyr', 'DT', 'crul'))"
