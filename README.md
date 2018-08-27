# NEON-Shiny-Browser

A multifunctional R Shiny tool designed to make NEON data accessible, visible, and easy to interact with.

## Overview

The NEON Shiny Browser is an interactive tool to browse, pull, and manipulate data collected by [NEON](https://www.neonscience.org/). This R Shiny app uses [leaflet](https://leafletjs.com/), [neonUtilities](https://github.com/NEONScience/NEON-utilities/tree/master/neonUtilities), and [nneo](https://github.com/ropensci/nneo) to create a comprehensive tool that allows users to do everything from browsing NEON sites, to finding and downloading and unziping data products.

### Features

First, this app displays relevant features of NEON and their research on a map. Users can view and filter map features such as NEON [sites](https://www.neonscience.org/field-sites), NEON domains, [AOP](https://www.neonscience.org/data-collection/airborne-remote-sensing) flightpaths, and [TOS](https://www.neonscience.org/data-collection/terrestrial-organismal-sampling) locations.

<**image/gif**>

Additionally, this app provides an easy, in-app alternative to NEON's [data portal](http://data.neonscience.org/browse-data). Users can view data products and their availabilities, download them in various formats, and then stack those downloads to make them more organized and accessible.

<img src = "Img/data_browse.gif" width = "420"/> <img src = "Img/data_download:unzip.gif" width = "420" align = "right"/>

### NEON

The National Ecological Observatory Network <a href="https://www.neonscience.org/"><img src = "Img/NEON.png" width=30/></a> is a "continental-scale ecological observation facility" that provides open data on our ecosystems. The envisioned 30-year project collects environmental data like precipitation, soil temperature, humidity, and pressure across 81 field sites (47 terrestrial and 34 aquatic) to measure the patterns and changes in our environment. With over 180 data products describing the characteristics of a diverse range of ecosystems, their data will be crucial to future studies of biology and climate change over time.

## Install and Run

To install, change the working directory on your shell to the desired directory, and clone from git:

```
cd /Desktop
git clone https://github.com/Danielslee51/NEON-Shiny-Browser
```

Open an instance of RStudio and navigate to the `/NEON-Shiny-Browser` folder. Click and run `server.R`.

## Package Requirements

Download the latest version of [R](https://cran.r-project.org/) and [RStudio](https://www.rstudio.com/) for your local or virtual machine.

A few packages need to be downloaded: <br>
* [`leaflet`](https://github.com/rstudio/leaflet) and [`leaflet.extras`](https://github.com/bhaskarvk/leaflet.extras): These are responsible for the map and its features.
* [`devtools`](https://cran.r-project.org/web/packages/devtools/index.html): For installing packages from Github.
* [`neonUtilities`](https://github.com/NEONScience/NEON-utilities/tree/master/neonUtilities) and [`nneo`](https://github.com/ropensci/nneo): Used to pull datasets from NEON.
* [`shinythemes`](https://github.com/rstudio/shinythemes), [`shinyWidgets`](https://github.com/dreamRs/shinyWidgets) and [`shinyBS`](https://github.com/ebailey78/shinyBS): Creates a shiny theme, provides shiny widgets with increased capabilities and aesthetics, and adds boostrap components to shiny.
* [`sf`](https://github.com/r-spatial/sf) and [`geosphere`](https://github.com/cran/geosphere): Deal with geometries and coordinates necesary for the interactive map.
* [`jsonlite`](https://github.com/cran/jsonlite): deals with JSON structures.

 ```
install.packages(c('leaflet','leaflet.extras','devtools','nneo','shinythemes','shinyWidgets','sf','geosphere','jsonlite'))
devtools::install_github("NEONScience/NEON-utilities/neonUtilities", dependencies=TRUE)
```

**Note: [Mac OS X](https://cran.r-project.org/bin/macosx/tools/) currently requires that `gfortran` and `clang` be installed in addition to the latest version of R (v3.5.1 "Feather Spray")** 
