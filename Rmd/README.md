# Calliope-View
An interactive leaflet interface designed to display ecological data alongside drone imaging.
## Overview
This R Shiny app uses leaflet to display ecological data provided by [NEON](https://www.neonscience.org/) alongside drone imaging under one map. Users will then be able to compare biological statistics such as precipitation, soil temperature, humidity, and pressure with their own data to generate analyses ranging from the effects of climate change to the relationship between geographical factors and the ecosystem.

[NEON](https://www.neonscience.org/) is a "continental-scale ecological observation facility" that provides open data on our ecosystems. [NEON](https://www.neonscience.org/) is the source that this app pulls from to get ecological data.

The drone data will be pulled from a global index in [Cyverse](http://www.cyverse.org/)'s [discovery environment](https://www.cyverse.org/discovery-environment) (DE). This includes a variety of sources, such as the DE drive, S3, and private servers.
## Features
The app offers a map, which displays items such as NEON sites and domains, alongside custom data which can be filtered and displayed based on multiple variables. Here is an example of Calliope View’s display of NEON sites and their boundaries:
<br><br>
<img src="Img/Calliope-View1.gif" height="500"/>
<br><br>
Here is a display of the user's ability to filter datasets via multiple varibales:
<br><br>
<img src="Img/Calliope-View2.gif" height="500"/>
> The dataset being queried and shown describes animal locations because we originally used an animal dataset as placeholder for the drone data while it was being developed. The app no longer includes this data, but it is still useful in demonstrating the app, functionality.
## Use and Installation
### Git
To install, change the working directory on your shell to the desired directory, and clone from git:
``` bash
cd /Desktop
git clone https://github.com/Danielslee51/Calliope-View/
```
Then, run server.R in an R IDE (such as Rstudio). This app has to be run locally due to features that require the user's local filesystem.

<img src="Img/RStudio.png" width="500"/>

### Packages
This app requires a few packages that might need to be downloaded: <br>
* [`leaflet`](https://github.com/rstudio/leaflet) and [`leaflet.extras`](https://github.com/bhaskarvk/leaflet.extras): These are responsible for the map and its features.
* [`neonUtilities`](https://github.com/NEONScience/NEON-utilities/tree/master/neonUtilities) and [`nneo`](https://github.com/ropensci/nneo): used to pull datasets from NEON.
* [`shinythemes`](https://github.com/rstudio/shinythemes): alter the appearance of the Shiny app. 
* [`shinyWidgets`](https://github.com/dreamRs/shinyWidgets): provides costumized and "pimp[ed]" up widgets for shiny.
* [`sf`](https://github.com/r-spatial/sf) and [`rgdal`](https://github.com/cran/rgdal): provide access to simple feature geometries.
* [`jsonlite`](https://github.com/cran/jsonlite): helps deal with JSON structures.
## FEEDBACK
This is a message from the main developer of this app, [Daniel Lee](https://github.com/Danielslee51). I am an intern`?` (my internship ended, but I'm not paid, so what am I? Exploited?) at the [CyVerse](http://www.cyverse.org/) lab at the University of Arizona. While I am doing the coding for this app, I don't personally know much about the different NEON data, sensors, and plans, and I rely on [Tyson](https://github.com/tyson-swetnam) to direct me. Due to this, when displaying NEON data I put relevant attributes to the best of my ability, but sometimes I do not know what is actually useful to a scientist. For example, on the popup for my TOS location polygons, I included the dimensions of the area, but maybe what really matters is the elevation (I have no clue). Or, when searching for NEON data products in-app, maybe people would like to see the methodology of collecting that product, which I did not include. If anyone notices anything like this on any feature, pls email me at dantheman6100@gmail.com.

And of course, any other feedback or suggestions would be nice. I'd love to hear reactions from anyone who would potentially use the app in the future, as ultimately the app is here to help scientists who want to use it.
