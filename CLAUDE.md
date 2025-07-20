# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a NEON Shiny Browser - an R Shiny application for browsing, visualizing, and downloading ecological data from the National Ecological Observatory Network (NEON). The app provides an interactive map interface and data portal for accessing NEON's scientific datasets.

## Development Commands

### Running the Application

**Local Development:**
```r
# In R console
setwd('~/neon-shiny-browser')
library(shiny)
runApp()
```

**Background Process (Recommended):**
```r
# Create background.R script and run as RStudio Job
# After starting, connect via: rstudioapi::viewer("http://localhost:[PORT]")
```

**Docker:**
```bash
# Pull and run container
docker pull cyversevice/shiny-neon-browser:3.6.3
mkdir ~/NEON_Downloads
docker run -it --rm -p 3838:3838 -e REDIRECT_URL=http://localhost:3838 -v ${HOME}/NEON_Downloads:/srv/shiny-server/NEON_Downloads cyversevice/shiny-neon-browser:3.6.3
```

### Package Management

**Install Dependencies:**
```r
# Run the install script
source("Install.R")
```

Required packages are defined in `Install.R` and include: shiny, leaflet, leaflet.extras, neonUtilities, shinythemes, shinyWidgets, shinyBS, shinyjs, sf, geosphere, jsonlite, dplyr, DT, crul.

## Application Architecture

### Core Structure

- **Global.R**: Application initialization, package loading, data preprocessing
- **Ui.R**: User interface definition using Shiny's fluidPage layout
- **Server.R**: Server-side logic handling user interactions and data processing

### Key Components

**Map Interface:**
- Interactive leaflet map with multiple base layers (OpenStreetMap, Satellite, Topo)
- NEON field sites, domains, and flight path overlays
- Site filtering and selection capabilities

**Data Browser:**
- Product catalog browsing (by site or by product)
- Data product filtering by keywords, data team, and theme
- Download functionality with automatic stacking/unzipping

**Custom Functions (`Functions/` directory):**
- `flight_function.R`: Handle AOP flight boundary data
- `filter_*_function.R`: Data filtering utilities
- `neonUtilities.R`: Custom NEON data utilities
- `nneo/`: Functions from deprecated nneo package for NEON API access

### Data Sources

- **NEON-data/**: Static JSON files with field site metadata and domain information
- **Img/**: Application assets and help documentation images
- **Rmd/**: R Markdown files for help documentation and tutorials

### Deployment

The application can be deployed locally, in RStudio, or via Docker container. Downloads are stored in `../NEON_Downloads` directory created automatically on first run.

## File Organization

- Root level contains main Shiny files (Global.R, Ui.R, Server.R)
- `Functions/` contains modular R functions
- `NEON-data/` contains static data files and flight boundaries
- `Rmd/` contains help documentation
- Docker configuration supports deployment via cyversevice/shiny-neon-browser image