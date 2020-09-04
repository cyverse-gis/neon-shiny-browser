FROM cyversevice/shiny-geospatial:3.6.3

RUN R -e "install.packages(c('shiny','leaflet','leaflet.extras','neonUtilities','shinythemes','shinyWidgets','shinyBS','shinyjs','sf','geosphere','jsonlite', 'dplyr', 'DT', 'crul'))"

RUN cd /srv/shiny-server && git clone https://github.com/cyverse-gis/NEON-Shiny-Browser

# change permission of the shiny folder where the app resides
RUN chmod -R 777 /srv/shiny-server

WORKDIR /srv/shiny-server/NEON-Shiny-Browser/

# Add shiny user to docker group to allow writing back onto host

RUN groupadd docker && usermod -aG docker shiny

# Start the server
CMD ["/usr/bin/shiny-server.sh"]
