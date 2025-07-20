FROM rocker/shiny-verse:4.4

# Install system dependencies for spatial packages
RUN apt-get update && apt-get install -y \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('shiny','leaflet','leaflet.extras','neonUtilities','shinythemes','shinyWidgets','shinyBS','shinyjs','sf','geosphere','jsonlite', 'dplyr', 'DT', 'crul', 'httr'), repos='https://cloud.r-project.org/')"

RUN cd /srv/shiny-server && git clone -b v1.2 https://github.com/cyverse-gis/NEON-Shiny-Browser

# change permission of the shiny folder where the app resides
RUN chmod -R 777 /srv/shiny-server

WORKDIR /srv/shiny-server/NEON-Shiny-Browser/

# Add shiny user to docker group to allow writing back onto host

RUN usermod -u 1000 shiny

RUN groupadd --gid 10013 iplant-everyone
RUN usermod -aG 10013 shiny

# Start the server
CMD ["/usr/bin/shiny-server.sh"]
