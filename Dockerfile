FROM cyversevice/shiny-geospatial:latest

RUN R -e "install.packages(c('shiny','leaflet','leaflet.extras','neonUtilities','shinythemes','shinyWidgets','shinyBS','shinyjs','sf','geosphere','jsonlite', 'dplyr', 'DT', 'crul'))"

RUN cd /srv/shiny-server && git clone https://github.com/cyverse-gis/Neon-Shiny-Browser

# change permission of the shiny folder where the app resides
RUN chmod -R 777 /srv/shiny-server

WORKDIR /srv/shiny-server/Neon-Shiny-Browser

# Start the server
CMD ["/usr/bin/shiny-server.sh"]
