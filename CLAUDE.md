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
docker pull cyversevice/shiny-neon-browser:latest
mkdir ~/NEON_Downloads
docker run -it --rm -p 3838:3838 -e REDIRECT_URL=http://localhost:3838 -v ${HOME}/NEON_Downloads:/srv/shiny-server/NEON_Downloads cyversevice/shiny-neon-browser:latest
```

### Testing

```r
# Run full test suite
source("run_tests.R")

# Run specific tests
library(testthat)
test_file("tests/testthat/test-neonUtilities-replacements.R")
```

### Package Management

**Install Dependencies:**
```r
# Run the install script
source("Install.R")
```

Required packages are defined in `Install.R` and include: shiny, leaflet, leaflet.extras, neonUtilities (2.0+), shinythemes, shinyWidgets, shinyBS, shinyjs, sf, geosphere, jsonlite, dplyr, DT, crul.

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
- TOS sampling location overlays

**Data Browser:**
- Product catalog browsing (by site or by product)
- Data product filtering by keywords, data team, and theme
- Download functionality with automatic stacking/unzipping
- NEON API token support for faster downloads

**Custom Functions (`Functions/` directory):**
- `flight_function.R`: Handle AOP flight boundary data
- `filter_*_function.R`: Data filtering utilities
- `neonUtilities_replacements.R`: Modern API functions replacing deprecated nneo package
- `spatial_data_*.R`: Automated spatial data caching and updates
- `checkDownload_function.R`, `getProductSize_function.R`: Download management

### Data Sources

- **NEON-data/**: Static JSON files with field site metadata and domain information
- **Spatial Data**: Automatically downloaded and cached from NEON's spatial data repository
- **API Endpoints**: https://data.neonscience.org/api/v0/ (products, sites, data)
- **Img/**: Application assets and help documentation images
- **Rmd/**: R Markdown files for help documentation and tutorials

### Important Development Notes

1. **API Security**: All NEON API calls use HTTPS (upgraded from HTTP in v1.1)
2. **Authentication**: Support for NEON API tokens via `NEON_TOKEN` environment variable
3. **Spatial Data Management**: Automated download system with version tracking - check `spatial_data_versions.json`
4. **Testing**: Comprehensive test suite in `tests/testthat/` - run before committing changes
5. **Downloads**: User downloads are stored in `../NEON_Downloads` directory created automatically

### Deployment

The application can be deployed locally, in RStudio, or via Docker container. Docker image is available at cyversevice/shiny-neon-browser:latest and includes all dependencies.

## File Organization

- Root level contains main Shiny files (Global.R, Ui.R, Server.R)
- `Functions/` contains modular R functions organized by purpose
- `NEON-data/` contains static data files and cached spatial datasets
- `tests/` contains comprehensive test suite
- `Rmd/` contains help documentation
- Docker configuration supports deployment via cyversevice/shiny-neon-browser image