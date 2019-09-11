[![CircleCI](https://circleci.com/gh/cyverse-gis/neon-shiny-browser.svg?style=svg)](https://circleci.com/gh/cyverse-gis/neon-shiny-browser) [![license](https://img.shields.io/badge/license-GPLv3-blue.svg)](https://opensource.org/licenses/GPL-3.0) [![Project Supported by CyVerse](https://img.shields.io/badge/Supported%20by-CyVerse-blue.svg)](https://www.cyverse.org) [![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip) 

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3405600.svg)](https://doi.org/10.5281/zenodo.3405600)

image | tag | size | metrics | build | 
----- | --- | ---- | ------- | ------|
[![DockerHub](https://img.shields.io/badge/DockerHub-brightgreen.svg?style=popout&logo=Docker)](https://hub.docker.com/r/cyversevice/shiny-geospatial/neon-shiny-browser) | [![](https://images.microbadger.com/badges/version/cyversevice/shiny-geospatial.svg)](https://microbadger.com/images/cyversevice/shiny-geospatial "neon-shiny-browser") |  [![](https://images.microbadger.com/badges/image/cyversevice/jupyterlab-scipy.svg)](https://microbadger.com/images/cyversevice/shiny-geospatial "neon-shiny-browser") | [![](https://img.shields.io/docker/pulls/cyversevice/shiny-geospatial.svg?label=pulls&logo=docker&logoColor=white)](https://hub.docker.com/r/cyversevice/shiny-geospatial)  |  [![](https://img.shields.io/docker/cloud/automated/cyversevice/shiny-geospatial.svg?label=build&logo=docker&logoColor=white)](https://hub.docker.com/r/cyversevice/shiny-geospatial/builds) 

# NEON-Shiny-Browser

A multifunctional R Shiny map tool deployed locally and designed to make NEON data accessible, visualized, and easy to interact with. This is a version of the [CyVerse NEON Browser](https://github.com/cyverse-gis/CyVerse-NEON-Browser) meant to be deployed locally. The CyVerse NEON Browser is an equivalent app, except hosted by [CyVerse](https://cyverse.org); acessible to users online, it can be found on the CyVerse [Discovery Environment](https://www.cyverse.org/discovery-environment).


## Overview

The NEON Shiny Browser is an interactive tool to browse, pull, and manipulate data collected by [NEON](https://www.neonscience.org/). This R Shiny app uses [leaflet](https://leafletjs.com/), [neonUtilities](https://github.com/NEONScience/NEON-utilities/tree/master/neonUtilities), and [nneo](https://github.com/ropensci/nneo) to create a comprehensive tool that allows users to do everything from browsing NEON sites, to finding and downloading and unzipping data products.

### Features

First, this app displays relevant features of NEON and their research on a map. Users can view and filter map features such as NEON [sites](https://www.neonscience.org/field-sites), NEON domains, [AOP](https://www.neonscience.org/data-collection/airborne-remote-sensing) flightpaths, and [TOS](https://www.neonscience.org/data-collection/terrestrial-organismal-sampling) locations.

<p align="center"><img src='https://github.com/cyverse-gis/CyVerse-NEON-Browser/blob/master/Img/Map.gif?raw=true' width='750'></p>


Additionally, this app provides an easy, in-app alternative to NEON's [data portal](http://data.neonscience.org/browse-data). Users can browse data products, view their details (e.g. description, abstract, availability), and easily download them to their computer.

<p align="center"><img src='https://github.com/cyverse-gis/CyVerse-NEON-Browser/blob/master/Img/Browse.gif?raw=true' width='750'></p>
<p align="center"><img src='https://github.com/cyverse-gis/NEON-Shiny-Browser/blob/master/Img/Download_Local.gif?raw=true' width='750'></p>

### Goal

The goal of this app is to simplify the NEON experience and introduce NEON's services in a simple platform that can be useful for newcomers and experienced users alike. While the <a href='https://www.neonscience.org/' target='_blank'>NEON website</a> will always be the ultimate destination for information, opportunities, or more advanced requests, this application is a functional tool meant to satisfy basic interactions with NEON services. Specifically, the app's unified platform hopes to simplify the NEON experience by aggregating many of the more advanced features and making them accesible to those without the time or the programming experience. For example, all downloads come stacked, meaning that the data products arrive already unzipped, joined, and grouped by table type. While one could download from the <a href='http://data.neonscience.org/home' target='_blank'>NEON Data Portal</a>, and then use an R package to apply the same process to their downloads, that requires multiple steps and some basic knowledge of programming and R; the CyVerse NEON Browser, on the other hand, does this automatically, saving time and learning for those who want it. Similarly, a visit to the NEON website yields a data browser and interactive map in different locations (and entirely separate domains), making it potentially confusing for a newcomer to grasp the basics of NEON and be able to find all the services that augment NEON data. This tool, conversely, combines basic approximations of the map and data browser, offering similar information and capabilities from one contained platform. Through these measures, the CyVerse NEON Browser hopes to act as a complement to the structure that NEON has already created, increasing its reach and impact in the world of ecology and beyond.

## NEON

The National Ecological Observatory Network <a href="https://www.neonscience.org/"><img src = "Img/NEON.png" width=30/></a> is a "continental-scale ecological observation facility" that provides open data on our ecosystems. The envisioned 30-year project collects environmental data like precipitation, soil temperature, humidity, and pressure across 81 field sites (47 terrestrial and 34 aquatic) to measure the patterns and changes in our environment. With over 180 data products describing the characteristics of a diverse range of ecosystems, their data will be crucial to future studies of biology and climate change over time.

## Install and Run

To install, change the working directory on your shell to the desired directory, and clone from git:

```
cd ~/Desktop
git clone https://github.com/cyverse-gis/NEON-Shiny-Browser
```

Open an instance of RStudio and navigate to the `/NEON-Shiny-Browser` folder. Click and run `server.R`.

## Package Requirements

Download the latest version of [R](https://cran.r-project.org/) and [RStudio](https://www.rstudio.com/) for your local or virtual machine.

In addition to shiny itself, a few packages need to be downloaded: <br>
* [`leaflet`](https://github.com/rstudio/leaflet) and [`leaflet.extras`](https://github.com/bhaskarvk/leaflet.extras): These are responsible for the map and its features.
* [`neonUtilities`](https://github.com/NEONScience/NEON-utilities/tree/master/neonUtilities): Used to download and stack NEON data.
* [`shinythemes`](https://github.com/rstudio/shinythemes), [`shinyWidgets`](https://github.com/dreamRs/shinyWidgets), [`shinyBS`](https://github.com/ebailey78/shinyBS), and [`shinyjs`](https://github.com/daattali/shinyjs): Adds themes, "pimped-up" widgets, boostrap, and JavaScript functions to the app.
* [`sf`](https://github.com/r-spatial/sf) and [`geosphere`](https://github.com/cran/geosphere): Deal with geometries and coordinates necesary for the interactive map.
* [`jsonlite`](https://github.com/cran/jsonlite): Deals with JSON structures.
* [`DT`](https://github.com/rstudio/DT) and [`dplyr`](https://github.com/tidyverse/dplyr): Help with data table manipulation.
* [`crul`](https://cran.r-project.org/web/packages/crul/index.html): Used for making HTTP requests.

```
install.packages(c('shiny','leaflet','leaflet.extras','neonUtilities','shinythemes','shinyWidgets','shinyBS','shinyjs','sf','geosphere','jsonlite', 'dplyr', 'DT', 'crul'))
```

**Note: [Mac OS X](https://cran.r-project.org/bin/macosx/tools/) currently requires that `gfortran` and `clang` be installed in addition to the latest version of R (v3.5.1 "Feather Spray")** 

## Docker

Run Docker locally or on a Virtual Machine

### Build Container locally


To build the Docker container locally:

```
git clone https://github.com/cyverse-gis/NEON-Shiny-Browser

cd NEON-Shiny-Browser

sudo docker build -t your-container-name:tag .
```

### Pull Container from Docker Hub

To run the Shiny-Server, you must first `pull` the container from the Docker Hub

```
docker pull cyversevice/shiny-geospatial:neon-shiny-browser
```

### Run locally

```
docker run -it --rm -p 3838:3838 -e REDIRECT_URL=http://localhost:3838 -v /home/<your-username-here>/Downloads:/home/root/NEON_Downloads cyversevice/shiny-geospatial:neon-shiny-browser
```

The app will open in your browser at `http://localhost:3838`

You also need to mount a local volume to the container in order to save the downloaded data when the container expires.


## FEEDBACK
This is a message from the main developer of this app, [Daniel Lee](https://github.com/Danielslee51). I am an intern at [CyVerse](http://www.cyverse.org/). If anyone notices anything they see is wrong or want changed and improved, please create an Issue here. You can also contact my advisor, [Tyson L. Swetnam](https://github.com/tyson-swetnam).

And of course, any other feedback or suggestions would be nice. I'd love to hear reactions from anyone who would potentially use the app in the future, as ultimately the app is here to help scientists who want to use it.
