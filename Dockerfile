FROM rocker/shiny-verse:4.4

# Install system dependencies for spatial packages and git
RUN apt-get update && apt-get install -y \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    git \
    && rm -rf /var/lib/apt/lists/*

# Clone the NEON Shiny Browser app (v1.1 branch)
RUN cd /srv/shiny-server && \
    git clone -b v1.1 https://github.com/cyverse-gis/NEON-Shiny-Browser && \
    chmod -R 755 /srv/shiny-server/NEON-Shiny-Browser

# Install R packages using the app's Install.R script
WORKDIR /srv/shiny-server/NEON-Shiny-Browser/
RUN R -e "source('Install.R')" && \
    rm -rf /tmp/downloaded_packages

# Create environment variable to skip Install.R at runtime
ENV DOCKER_ENV=true

# Set up the app directory and permissions
WORKDIR /srv/shiny-server/NEON-Shiny-Browser/

# Create NEON_Downloads directory
RUN mkdir -p /srv/shiny-server/NEON_Downloads && \
    chown -R shiny:shiny /srv/shiny-server/NEON_Downloads

# Configure shiny-server for single app
RUN echo 'run_as shiny;\n\
server {\n\
  listen 3838;\n\
  location / {\n\
    app_dir /srv/shiny-server/NEON-Shiny-Browser;\n\
    log_dir /var/log/shiny-server;\n\
    directory_index on;\n\
  }\n\
}' > /etc/shiny-server/shiny-server.conf

# Expose the port
EXPOSE 3838

# Start shiny-server
CMD ["/usr/bin/shiny-server"]
