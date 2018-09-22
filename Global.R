library(shiny)
library(DT)
library(shinythemes)
library(shinyWidgets)
library(shinyBS)
library(shinyjs)
library(leaflet)
library(leaflet.extras)
library(sf)
library(dplyr)
library(jsonlite)
library(neonUtilities)
library(nneo)
source('Functions/directoryWidget/directoryInput.R')
source('Functions/flight_function.R')
source('Functions/filter_keyword_function.R')
source('Functions/filter_site_function.R')
source('Functions/keyword_lists_function.R')
source('Functions/getProductSize_function.R')

if (!dir.exists("../NEON Downloads")) {
  dir.create("../NEON Downloads")
  dir_created <- TRUE
} else {
  dir_created <- FALSE
}
if (sum(as.data.frame(installed.packages())$Package %in% 'geosphere') < 1) {
  print("Please install the package 'geosphere'. Run this line: 'install.packages('geosphere')'")
  stop(call. = TRUE)
}

####———MAP DATA———####


####——NEON——####

###NEON Field Sites####

## Retrieve point data for NEON Field Sites in JSON format
FieldSite_point_JSON <- fromJSON('http://data.neonscience.org/api/v0/sites')
FieldSite_point <- FieldSite_point_JSON$data
FieldSite_point$domainCode <- as.numeric(gsub(pattern = "D", replacement = "", x = FieldSite_point$domainCode))
FieldSite_extra <- read.csv('NEON-data/Fieldsites_extrainfo.csv', colClasses = "character")
FieldSite_extra <- FieldSite_extra[order(FieldSite_extra$Site.ID),]
for (i in 1:nrow(FieldSite_extra)) {
  FieldSite_extra$Site.Type[i] <- strsplit(FieldSite_extra$Site.Type[i], " ")[[1]][2]
  if (FieldSite_extra$Site.Type[i] == "Aquatic") {
    FieldSite_extra$Site.Subtype[i] <- paste0(FieldSite_extra$Site.Type[i], " - ", FieldSite_extra$Site.Subtype[i]) 
  } else {
    FieldSite_extra$Site.Subtype[i] <- FieldSite_extra$Site.Type[i]
  }
}
FieldSite_point$Habitat <- FieldSite_extra$Site.Type
FieldSite_point$`Habitat Specific` <- FieldSite_extra$Site.Subtype
FieldSite_point$Host <- FieldSite_extra$Site.Host

# List of field site abbreviations
FieldSite_abbs <- FieldSite_point$siteCode
FieldSite_Tes <- FieldSite_point$siteCode[FieldSite_point$Habitat %in% "Terrestrial"]
FieldSite_Aqu <- FieldSite_point$siteCode[FieldSite_point$Habitat %in% "Aquatic"]

## Retrieve polygon data for NEON Field Sites
#Fieldsite_poly_JSON <- fromJSON('http://guest:guest@128.196.38.73:9200/sites/_search?size=500')
# Unhashtag when index is down:
Fieldsite_poly_JSON <- fromJSON('NEON-data/Fieldsites.json')
FieldSite_poly <- cbind(Fieldsite_poly_JSON$hits$hits[-5], Fieldsite_poly_JSON$hits$hits$`_source`[-4], Fieldsite_poly_JSON$hits$hits$`_source`$boundary)
names(FieldSite_poly)[9] <- "geo_type"
FieldSite_poly <- FieldSite_poly %>% filter(type %in% "NEON")
for (i in 1:nrow(FieldSite_poly)) {
  FieldSite_poly$code[i] <- strsplit(FieldSite_poly$code[i], "-")[[1]][2]
  FieldSite_poly$siteType[i] <- strsplit(FieldSite_poly$name[i], ", ")[[1]][2]
  FieldSite_poly$name[i] <- strsplit(FieldSite_poly$name[i], ", ")[[1]][1]
  FieldSite_poly$domainName[i] <- strsplit(FieldSite_poly$details[[i]][1], ":")[[1]][2]
  FieldSite_poly$domainCode[i] <- strsplit(FieldSite_poly$details[[i]][2], ":")[[1]][2]
  FieldSite_poly$domainCode[i] <- strsplit(FieldSite_poly$domainCode[i], "D")[[1]][2]  
  FieldSite_poly$stateCode[i] <- strsplit(FieldSite_poly$details[[i]][5], ":")[[1]][2]
  FieldSite_poly$stateName[i] <- strsplit(FieldSite_poly$details[[i]][6], ":")[[1]][2]
}
FieldSite_poly$domainCode <- as.numeric(FieldSite_poly$domainCode)

## Retrive Fieldsite Locations
FieldSite_locations_tes <- read.csv("NEON-data/Fieldsites_locations_tes", stringsAsFactors = FALSE)
FieldSite_plots_tes <- read.csv("NEON-data/Fieldsites_plots_tes", stringsAsFactors = FALSE)[-1]
FieldSite_locations_aqu <- read.csv("NEON-data/Fieldsites_locations_aqu", stringsAsFactors = FALSE)
aqu_location_filter <- startsWith(FieldSite_locations_aqu$Name, "WELL") | startsWith(FieldSite_locations_aqu$Name, "METSTN") | grepl("S.LOC", FieldSite_locations_aqu$Name) |startsWith(FieldSite_locations_aqu$Name, "INLET") | startsWith(FieldSite_locations_aqu$Name, "OUTLET") | startsWith(FieldSite_locations_aqu$Name, "BUOY") | startsWith(FieldSite_locations_aqu$Name, "SGAUGE") |
endsWith(FieldSite_locations_aqu$Name, "reach.bottom") | endsWith(FieldSite_locations_aqu$Name, "reach.top") | grepl("riparian[.]point", FieldSite_locations_aqu$Name) | grepl("riparian[.]transect", FieldSite_locations_aqu$Name)
FieldSite_locations_aqu <- FieldSite_locations_aqu[aqu_location_filter,]
for (i in 1:nrow(FieldSite_locations_aqu)) {
  FieldSite_locations_aqu$`General Type`[i] <- if (startsWith(FieldSite_locations_aqu$Type[i], "GROUND")) {
    "Groundwater Well"
  } else if (startsWith(FieldSite_locations_aqu$Type[i], "MET")) {
    "Met. Station"
  } else if (grepl( "_LOC", FieldSite_locations_aqu$Type[i]) | grepl("BUOY", FieldSite_locations_aqu$Type[i])) {
    "Sensor Station"
  } else if (startsWith(FieldSite_locations_aqu$Type[i], "STAFF")) {
    "Staff gauge/camera"
  } else if (grepl("reach", FieldSite_locations_aqu$Type[i])) {
    "Sampling Reach Boundary"
  } else if (grepl("riparian", FieldSite_locations_aqu$Type[i])) {
    "Riparian Assessment"
  }
}

####NEON Domains####
## Retrive data from NEON Domains in JSON format
domains <- fromJSON('NEON-data/NEON_Domains.json')
# Retrieve just the DomainID and Domain Name
domains <- cbind("DomainID" = domains$features$properties$DomainID,"Domain"=domains$features$properties$DomainName)
# Remove Duplicates, make data frame
domains <- as.data.frame(unique(domains))
domains$Domain <- as.character(domains$Domain)
# Retrieve geometry data using st_read()
domain_data <- st_read('NEON-data/NEON_Domains.json')

####NEON Flight Boxes####
## Retrieve info for NEON flight boxes
# Get human info about flight boxes
FieldSite_table <- data.frame("Abb"=c("BART","HARV","BLAN","SCBI","SERC","DSNY","JERC","OSBS","STEI-CHEQ","STEI-TREE","UNDE","KONZ-KONA","GRSM","MLBS","ORNL","DELA","LENO","TALL","DCFS-WOOD","NOGP","CLBJ","OAES","CHEQ", "BARO"),
                              "Site"=c("Bartlett Experimental Forest North-South flight box", "Harvard Forest flight box","Blandy Experimental Farm flight box","Smithsonian Conservation Biology Institute flight box","Smithsonian Ecological Research Center flight box","Disney Wilderness Preserve flight box","Jones Ecological Research Center Priority 1 flight box","Ordway-Swisher Biological Station Priority 1 flight box","Chequamegon-Nicolet National Forest flight box","Steigerwaldt-Treehaven Priority 2 flight box","UNDERC flight box","Konza Prairie Biological Station and KONA agricultural site flight box","Great Smoky Mountains National Park priority 2 flight box","Mountain Lake Biological Station flight box","Oak Ridge National Laboratory flight box","Dead Lake flight box","Lenoir Landing flight box","Talladega National Forest flight box","Woodworth and Dakota Coteau Field School flight box","Northern Great Plains flight box","LBJ Grasslands flight box","Klemme Range Research Station flight box",
                                       "Chequamegon-Nicolet National Forest", "Barrow"))
FieldSite_table <- bind_rows(FieldSite_table, as.data.frame(cbind(Abb = FieldSite_point$siteCode, Site =FieldSite_point$siteDescription)))
FieldSite_table <- FieldSite_table[c(-29, -31, -37, -44, -45, -47, -50, -53, -60, -67, -70, -71, -74, -75, -83, -84, -92, -100),]
CR_table <- data.frame("Abb" = c("C", "R", "A"),"Actual" = c("Core", "Relocatable", "Aquatic"),
                       stringsAsFactors = FALSE)
# filesnames needed for loops
flight_filenames_all_2016 <- Sys.glob('NEON-data/Flightdata/Flight_boundaries_2016/D*')
flight_filenames_2016 <- Sys.glob('NEON-data/Flightdata/Flight_boundaries_2016/D*.geojson')
flight_data(flightlist_info = flight_filenames_all_2016, flightlist_geo = flight_filenames_2016, year = "2016", name = "flight_data_2016")
flight_filenames_all_2017 <- Sys.glob('NEON-data/Flightdata/Flight_boundaries_2017/D*')
flight_filenames_2017 <- Sys.glob('NEON-data/Flightdata/Flight_boundaries_2017/D*.geojson')
flight_data(flightlist_info = flight_filenames_all_2017, flightlist_geo = flight_filenames_2017, year = "2017", name = "flight_data_2017")
flight_data <- rbind(flight_data_2016, flight_data_2017)

#### Miscellaneous Variables ####

NEON_datatypes <- c("Airborne Observation Platform (AOP)", "Aquatic Instrument System (AIS)", "Aquatic Observation System (AOS)","Terrestrial Instrument System (TIS)", "Terrestrial Observation System (TOS)")
baseplot_text <- "30/site: Distributed Base Plots support a variety of plant productivity, plant diversity, soil, biogeochemistry, microbe and beetle sampling. Distributed Base Plots are 40m x 40m."
birdgrid_text <- "5-15/site: Bird Grids consist of 9 sampling points within a 500m x 500m square. Each point is 250m apart. Where possible, Bird Grids are colocated with Distributed Base Plots by placing the Bird Grid center in close proximity to the center of the Base Plot. At smaller sites, a single point count is done at the south-west corner of the Distributed Base Plot."
mammalgrid_text <- "6-8/site: Mammal Grids are 90m x 90m and include 10m spacing. Where possible, these grids are colocated with Distributed Base Plots by placing them a specified distance (150m +/- 50m) and random direction from the center of the Base Plot."
mosquitoplot_text <- "10/site: At each Mosquito Point, one CO2 trap is established. Due to the frequency of sampling and the temporal sampling constraints, Mosquito Points are located within 45m of roads."
tickplot_text <- "6/site: Tick Plots are sampled by conducting cloth dragging or flagging around the perimeter of a 40m x 40m plot. Tick plots are colocated with Distributed Base Plots by placing them a specified distance (150m +/- 15m) and random direction from the center of the Base Plot."
phenologyplot_text <- "1-2/site: Plant phenology observations are made along a transect loop or plot in or around the primaru airshed. When possible, one plot is established north of the tower to calibrate phenology camera images captured from sensors on the tower. If there is insufficient space north of the tower for a 200m x 200m plot or if the vegetation does not match the primary airshed an additional plot is established."

well_text <- "Each site has up to eight groundwater wells outfitted with sensors that measure high temporal resolution groundwater elevation (pressure transducer-based), temperature, and specific conductance."
metstn_text <- "A met. station is located on the shore of the most aquatic sites and collects data comparable with flux tower measurements at terrestrial sites. Lake and wadeable rivers also have an above water met. station buoy. These data are unique with different sensors and data frequencies due to power and data storage constraints."
sensor_text <- "Wadeable streams have a sensor station near the top of the reach and the bottom of the reach; non-wadeable rivers have a sensor station on a buoy and one near the bank; Lakes have an inlet sensor stations, and outlet sensor station and a sensor station sensor on a buoy. Data collection varies by type of sensor station. Click on sensor station on the map to learn more."
gauge_text <- "The staff gauge measures gauge height, in meters, measured at lakes, wadeable rivers and non-wadeable streams. A phenocam is installed near most gauges. It collects RGB and IR images of the lake, river, or stream vegetation, stream surface, and stream gauge every 15 minutes."
reach_text <- "These icons mark the top and bottom of the observational sampling reach at wadeable streams and non-wadeable rivers. The reach for lake sites is the shape of the lake itself. Observational sampling activities may include: Reaeration sampling; water chemistry, isotopes, dissolved gas; zooplankton, phytoplankton; secchi depth profile; bathymetric and morphologic mapping; discharge; sediment chemistry; riparian assessment; macroinvertebrates, plants, algae, and microbes; and fish."
riparian_text <- "Number of locations for assessment of riparian vegetation composition and physical structure vary by site type. Lakes and non-wadeable rivers have ten locations. Wadeable streams have 20 locations and also include assessment of riparian vegetation percent cover in wadeable streams."

####———MAP ICONS———####
NEON_icon <- makeIcon(iconUrl = "Img/NEON.png",
                      iconWidth = 30, iconHeight = 30,
                      iconAnchorX = 15, iconAnchorY = 15,
                      popupAnchorX = -1, popupAnchorY = -15)
NEON_locations_tes <- iconList(
  `Distributed Base Plot` = makeIcon(iconUrl = "Img/distributedBaseplot.png", iconWidth = 15, iconHeight = 15,
                                     iconAnchorX = 7.5, iconAnchorY = 7.5, popupAnchorX = -1, popupAnchorY = -7.5),
  `Distributed Bird Grid` = makeIcon(iconUrl = "Img/birdGrid.png", iconWidth = 15, iconHeight = 15,
                                     iconAnchorX = 7.5, iconAnchorY = 7.5, popupAnchorX = -1, popupAnchorY = -7.5),
  `Distributed Mosquito Plot` = makeIcon(iconUrl = "Img/mosquito.png", iconWidth = 15, iconHeight = 15,
                                         iconAnchorX = 7.5, iconAnchorY = 7.5, popupAnchorX = -1, popupAnchorY = -7.5),
  `Distributed Mammal Grid` = makeIcon(iconUrl = "Img/mammal.png", iconWidth = 15, iconHeight = 15,
                                       iconAnchorX = 7.5, iconAnchorY = 7.5, popupAnchorX = -1, popupAnchorY = -7.5),
  `Distributed Tick Plot` = makeIcon(iconUrl = "Img/tick.png", iconWidth = 15, iconHeight = 15,
                                     iconAnchorX = 7.5, iconAnchorY = 7.5, popupAnchorX = -1, popupAnchorY = -7.5),
  `Tower Phenology Plot` = makeIcon(iconUrl = "Img/phenology.png", iconWidth = 15, iconHeight = 15,
                                    iconAnchorX = 7.5, iconAnchorY = 7.5, popupAnchorX = -1, popupAnchorY = -7.5)
)
NEON_locations_aqu <- iconList(
  `Groundwater Well` = makeIcon(iconUrl = "Img/groundwaterWell.png", iconWidth = 15, iconHeight = 15,
                                iconAnchorX = 7.5, iconAnchorY = 7.5, popupAnchorX = -1, popupAnchorY = -7.5),
  `Met. Station` = makeIcon(iconUrl = "Img/metStation.png", iconWidth = 15, iconHeight = 15,
                            iconAnchorX = 7.5, iconAnchorY = 7.5, popupAnchorX = -1, popupAnchorY = -7.5),
  `Sensor Station` = makeIcon(iconUrl = "Img/sensorStation.png", iconWidth = 15, iconHeight = 15,
                              iconAnchorX = 7.5, iconAnchorY = 7.5, popupAnchorX = -1, popupAnchorY = -7.5),
  `Staff gauge/camera` = makeIcon(iconUrl = "Img/staffGaugeCamera.png", iconWidth = 15, iconHeight = 15,
                                  iconAnchorX = 7.5, iconAnchorY = 7.5, popupAnchorX = -1, popupAnchorY = -7.5),
  `Sampling Reach Boundary` = makeIcon(iconUrl = "Img/reach-icon.png", iconWidth = 15, iconHeight = 15,
                                       iconAnchorX = 7.5, iconAnchorY = 7.5, popupAnchorX = -1, popupAnchorY = -7.5),
  `Riparian Assessment` = makeIcon(iconUrl = "Img/riparianAssessment.png", iconWidth = 15, iconHeight = 15,
                                   iconAnchorX = 7.5, iconAnchorY = 7.5, popupAnchorX = -1, popupAnchorY = -7.5) 
)
