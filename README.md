# NEON-Shiny-Browser

A multifunctional R Shiny map tool deployed locally and designed to make NEON data accessible, visualized, and easy to interact with. This is a version of the [CyVerse NEON Browser](https://github.com/Danielslee51/CyVerse-NEON-Browser) meant to be deployed locally. The CyVerse NEON Browser is an equivalent app, except hosted by [CyVerse](https://cyverse.org); acessible to CyVerse users online, it can be found here: .


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

In addition to shiny itself, a few packages need to be downloaded: <br>
* [`leaflet`](https://github.com/rstudio/leaflet) and [`leaflet.extras`](https://github.com/bhaskarvk/leaflet.extras): These are responsible for the map and its features.
* [`neonUtilities`](https://github.com/NEONScience/NEON-utilities/tree/master/neonUtilities) and [`nneo`](https://github.com/ropensci/nneo): Used to pull datasets from NEON.
* [`shinythemes`](https://github.com/rstudio/shinythemes), [`shinyWidgets`](https://github.com/dreamRs/shinyWidgets), [`shinyBS`](https://github.com/ebailey78/shinyBS), and [`shinyjs`](https://github.com/daattali/shinyjs): Adds themes, "pimped-up" widgets, boostrap, and JavaScript functions to the app.
* [`sf`](https://github.com/r-spatial/sf) and [`geosphere`](https://github.com/cran/geosphere): Deal with geometries and coordinates necesary for the interactive map.
* [`jsonlite`](https://github.com/cran/jsonlite): Deals with JSON structures.
* [`DT`](https://github.com/rstudio/DT) and [`dplyr`](https://github.com/tidyverse/dplyr): Help with data table manipulation.

 ```
install.packages(c('leaflet','leaflet.extras','devtools','neonUtilities','nneo','shinythemes','shinyWidgets','shinyBS','shinyjs','sf','geosphere','jsonlite', 'dplyr', 'DT'))
```

**Note: [Mac OS X](https://cran.r-project.org/bin/macosx/tools/) currently requires that `gfortran` and `clang` be installed in addition to the latest version of R (v3.5.1 "Feather Spray")** 

## FEEDBACK
This is a message from the main developer of this app, [Daniel Lee](https://github.com/Danielslee51). I am an intern at [CyVerse](http://www.cyverse.org/). I don't personally know much about NEON data, sensors, or AOP tiles. Due to this, when displaying NEON data I put relevant attributes to the best of my ability, but sometimes I do not know what is actually useful to a scientist. For example, on the popups for the NEON field sites I included their site types and habitats, but maybe what really matters is the elevation (I have no clue). If anyone notices anything like this on any feature, please email me at dantheman6100@gmail.com.

And of course, any other feedback or suggestions would be nice. I'd love to hear reactions from anyone who would potentially use the app in the future, as ultimately the app is here to help scientists who want to use it.
