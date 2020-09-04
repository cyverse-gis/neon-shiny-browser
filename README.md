[![CircleCI](https://circleci.com/gh/cyverse-gis/neon-shiny-browser.svg?style=svg)](https://circleci.com/gh/cyverse-gis/neon-shiny-browser) [![license](https://img.shields.io/badge/license-GPLv3-blue.svg)](https://opensource.org/licenses/GPL-3.0) [![Project Supported by CyVerse](https://img.shields.io/badge/Supported%20by-CyVerse-blue.svg)](https://www.cyverse.org) [![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)  [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3405600.svg)](https://doi.org/10.5281/zenodo.3405600)

[![DockerHub](https://img.shields.io/badge/DockerHub-brightgreen.svg?style=popout&logo=Docker)](https://hub.docker.com/r/cyversevice/shiny-geospatial/neon-shiny-browser) [![](https://img.shields.io/docker/pulls/cyversevice/shiny-neon-browser.svg?label=pulls&logo=docker&logoColor=white)](https://hub.docker.com/r/cyversevice/shiny-geospatial) [![](https://img.shields.io/docker/cloud/automated/cyversevice/shiny-geospatial.svg?label=build&logo=docker&logoColor=white)](https://hub.docker.com/r/cyversevice/shiny-neon-browser/builds) 

# NEON-Shiny-Browser

A multifunctional R Shiny map and data API download tool. Designed to make NEON API data accessible in RStuduio. This version is meant to be deployed locally, on your own computer or virtual machine. The [CyVerse NEON Browser](https://github.com/cyverse-gis/CyVerse-NEON-Browser) can be used in the CyVerse [Discover Environment](https://de.cyverse.org) to download NEON data to your CyVerse Data Store. 

The app can be run in RStudio or RStudio-Server (online). 

## Overview

The NEON Shiny Browser is an interactive tool to browse, pull, and manipulate data collected by [NEON](https://www.neonscience.org/). This R Shiny app uses [leaflet](https://leafletjs.com/), [neonUtilities](https://github.com/NEONScience/NEON-utilities/tree/master/neonUtilities), and [nneo](https://github.com/ropensci/nneo) to create a comprehensive tool that allows users to do everything from browsing NEON sites, to finding and downloading and unzipping data products.

### Features

First, this app displays relevant features of NEON and their research on a map. Users can view and filter map features such as NEON [sites](https://www.neonscience.org/field-sites), NEON domains, [AOP](https://www.neonscience.org/data-collection/airborne-remote-sensing) flightpaths, and [TOS](https://www.neonscience.org/data-collection/terrestrial-organismal-sampling) locations.

<p align="center"><img src='https://github.com/cyverse-gis/neon-shiny-browser/blob/master/Img/Map.gif?raw=true' width='750'></p>


Additionally, this app provides an easy, in-app alternative to NEON's [data portal](http://data.neonscience.org/browse-data). Users can browse data products, view their details (e.g. description, abstract, availability), and easily download them to their computer.

<p align="center"><img src='https://github.com/cyverse-gis/neon-shiny-browser/blob/master/Img/Browse.gif?raw=true' width='750'></p>

### Goal

The goal of this app is to simplify the NEON experience and introduce NEON's services in a simple platform that can be useful for newcomers and experienced users alike. While the <a href='https://www.neonscience.org/' target='_blank'>NEON website</a> will always be the ultimate destination for information, opportunities, or more advanced requests, this application is a functional tool meant to satisfy basic interactions with NEON services. Specifically, the app's unified platform hopes to simplify the NEON experience by aggregating many of the more advanced features and making them accesible to those without the time or the programming experience. For example, all downloads come stacked, meaning that the data products arrive already unzipped, joined, and grouped by table type. While one could download from the <a href='http://data.neonscience.org/home' target='_blank'>NEON Data Portal</a>, and then use an R package to apply the same process to their downloads, that requires multiple steps and some basic knowledge of programming and R; the CyVerse NEON Browser, on the other hand, does this automatically, saving time and learning for those who want it. Similarly, a visit to the NEON website yields a data browser and interactive map in different locations (and entirely separate domains), making it potentially confusing for a newcomer to grasp the basics of NEON and be able to find all the services that augment NEON data. This tool, conversely, combines basic approximations of the map and data browser, offering similar information and capabilities from one contained platform. Through these measures, the CyVerse NEON Browser hopes to act as a complement to the structure that NEON has already created, increasing its reach and impact in the world of ecology and beyond.

## NEON

The National Ecological Observatory Network <a href="https://www.neonscience.org/"><img src = "Img/NEON.png" width=30/></a> is a "continental-scale ecological observation facility" that provides open data on our ecosystems. The envisioned 30-year project collects environmental data like precipitation, soil temperature, humidity, and pressure across 81 field sites (47 terrestrial and 34 aquatic) to measure the patterns and changes in our environment. With over 180 data products describing the characteristics of a diverse range of ecosystems, their data will be crucial to future studies of biology and climate change over time.

## Installation

To install the tool, clone from this git repository:

```
cd
git clone https://github.com/cyverse-gis/NEON-Shiny-Browser
```

### Run App in RStudio or RStudio-Server 

You can start the app directly from the folder by running Shiny:

```
setwd('~/neon-shiny-browser')
library(shiny)
runApp()
```
The app should install any missing dependencies in R. You may have to install additional system dependencies, see [RStudio Geospatial]( ) for examples 

**Important:** You must allow pop-ups in your Browser for the app to open

### Run App as a background process (preferred method)

Running a Shiny App in your R console will lock the console and prevent you from doing other work in RStudio while the app is running. You can run this app as a background process using the RStudio "Jobs" tab

Create an `background.R` script or use the one in this repo. Start a new Job running the script. After the app downloads its dependencies and starts, you'll see that it is running and listening on a randomly assigned local port: ```Listening on http://127.0.0.1:4199```
In this example, the app is on port `4199`
In the R Console, type:```rstudioapi::viewer("http://localhost:4199")```
The App will open in the lower right corner of RStudio in the Viewer pane.

You can pop-out the viewer and it will open as its own browser tab.

## Run with Docker

Run Docker locally or on a Virtual Machine

### Pull Container from Docker Hub

To run the Shiny-Server, you must first `pull` the container from the Docker Hub

```
docker pull cyversevice/shiny-neon-browser:3.6.3
```
Create a directory called `NEON_Downloads`, suggest in your user's home directory e.g. `mkdir ~/NEON_Downloads`

Run the container image: 
```
docker run -it --rm -p 3838:3838 -e REDIRECT_URL=http://localhost:3838 -v ${HOME}/NEON_Downloads:/srv/shiny-server/NEON_Downloads cyversevice/shiny-neon-browser:3.6.3
```

The app will open in your browser at `http://localhost:3838`

**Important:** The data are not downloaded to your computer if you do not mount a volume `-v` into the Docker container when it is run

If you're running on are remote server, you can change `localhost` to your IP address or DNS. 

### Build Container yourself

To build the Docker container locally:

```
git clone https://github.com/cyverse-gis/NEON-Shiny-Browser

cd NEON-Shiny-Browser

sudo docker build -t shiny-neon-browser:3.6.3
```

## Requirements

## Linux

We suggest using our [CyVerse](https://hub.docker.com/r/cyversevice/rstudio-geospatial] docker image or the original [Rocker Project Geospatial](https://hub.docker.com/r/rocker/geospatial) image, but if you want to attempt a local installation in linux, you can install the following:

```
sudo apt-get update 
sudo apt-get install -y --no-install-recommends \
    lbzip2 \
    libfftw3-dev \
    libgdal-dev \
    libgeos-dev \
    libgsl0-dev \
    libgl1-mesa-dev \
    libglu1-mesa-dev \
    libhdf4-alt-dev \
    libhdf5-dev \
    libjq-dev \
    liblwgeom-dev \
    libpq-dev \
    libproj-dev \
    libprotobuf-dev \
    libnetcdf-dev \
    libsqlite3-dev \
    libssl-dev \
    libudunits2-dev \
    netcdf-bin \
    postgis \
    protobuf-compiler \
    sqlite3 \
    tk-dev \
    unixodbc-dev
 
install2.r --error \
    RColorBrewer \
    RandomFields \
    RNetCDF \
    classInt \
    deldir \
    gstat \
    hdf5r \
    lidR \
    mapdata \
    maptools \
    mapview \
    ncdf4 \
    proj4 \
    raster \
    rgdal \
    rgeos \
    rlas \
    sf \
    sp \
    spacetime \
    spatstat \
    spdep \
    geoR \
    geosphere \
    ## from bioconductor
    && R -e "BiocManager::install('rhdf5', update=FALSE, ask=FALSE)"   
 ```
 
## R Packages

Download the latest version of [R](https://cran.r-project.org/) and [RStudio](https://www.rstudio.com/).

In addition to shiny itself, a few packages need to be downloaded: <br>
* [`leaflet`](https://github.com/rstudio/leaflet) and [`leaflet.extras`](https://github.com/bhaskarvk/leaflet.extras): These are responsible for the map and its features.
* [`neonUtilities`](https://github.com/NEONScience/NEON-utilities/tree/master/neonUtilities): Used to download and stack NEON data.
* [`shinythemes`](https://github.com/rstudio/shinythemes), [`shinyWidgets`](https://github.com/dreamRs/shinyWidgets), [`shinyBS`](https://github.com/ebailey78/shinyBS), and [`shinyjs`](https://github.com/daattali/shinyjs): Adds themes, "pimped-up" widgets, boostrap, and JavaScript functions to the app.
* [`sf`](https://github.com/r-spatial/sf) and [`geosphere`](https://github.com/cran/geosphere): Deal with geometries and coordinates necesary for the interactive map.
* [`jsonlite`](https://github.com/cran/jsonlite): Deals with JSON structures.
* [`DT`](https://github.com/rstudio/DT) and [`dplyr`](https://github.com/tidyverse/dplyr): Help with data table manipulation.
* [`crul`](https://cran.r-project.org/web/packages/crul/index.html): Used for making HTTP requests.

Install in R console: ```install.packages(c('shiny','leaflet','leaflet.extras','neonUtilities','shinythemes','shinyWidgets','shinyBS','shinyjs','sf','geosphere','jsonlite', 'dplyr', 'DT', 'crul'))```

**Note: [Mac OS X](https://cran.r-project.org/bin/macosx/tools/) currently requires that `gfortran` and `clang` be installed in addition to the latest version of R (> v3.5.1 "Feather Spray")** 

## FEEDBACK
This is a message from the main developer of this app, [Daniel Lee](https://github.com/Danielslee51). I am an intern at [CyVerse](http://www.cyverse.org/). If anyone notices anything they see is wrong or want changed and improved, please create an Issue here. You can also contact my advisor, [Tyson L. Swetnam](https://github.com/tyson-swetnam).

And of course, any other feedback or suggestions would be nice. I'd love to hear reactions from anyone who would potentially use the app in the future, as ultimately the app is here to help scientists who want to use it.
