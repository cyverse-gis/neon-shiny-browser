library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(jsonlite)
library(sf)
library(rgdal)
library(neonUtilities)
library(shinyWidgets)
library(nneo)
library(elasticsearchr)
source('Functions/directoryWidget/directoryInput.R')
source('Functions/flight_function.R')
source('Functions/filter_keyword_function.R')
source('Functions/filter_site_function.R')
source('Functions/keyword_lists_function.R')

if (!dir.exists("../NEON Downloads")) {
  dir.create("../NEON Downloads")
  dir_created <- TRUE
} else {
  dir_created <- FALSE
}

####———MAP DATA———####


####——NEON——####

###NEON Field Sites####
## Retrieve point data for NEON Field Sites in JSON format

FieldSite_point_JSON <- fromJSON('http://data.neonscience.org/api/v0/sites')
# Create a data frame using cbind()
FieldSite_point <- FieldSite_point_JSON$data
FieldSite_point$domainCode <- as.numeric(gsub(pattern = "D", replacement = "", x = FieldSite_point$domainCode))
FieldSite_abbs <- FieldSite_point$siteCode
## Retrieve polygon data for NEON Field Sites
# Unhashtag when index is down:
Fieldsite_poly_JSON <- fromJSON('http://guest:guest@128.196.38.73:9200/sites/_search?size=500')
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

####NEON Flightpaths####
## Retrieve info for NEON flightpaths
# Get human info about flightpaths
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

#### TOS ####
# Point markers
TOS_data <- st_read('TOS/NEON_TOS_Polygon.json')
for (i in 1:length(TOS_data$siteID)) {
  TOS_data$siteType[i] <- FieldSite_point$siteType[FieldSite_point$siteCode %in% TOS_data$siteID[i]]
}
TOS_data$domanID <- as.numeric(gsub(pattern = "D", replacement = "", x = TOS_data$domanID))

#### Miscellaneous Variables ####

NEON_datatypes <- c("Airborne Observation Platform (AOP)", "Aquatic Instrument System (AIS)", "Aquatic Observation System (AOS)","Terrestrial Instrument System (TIS)", "Terrestrial Observation System (TOS)")

####———MAP ICONS———####
NEON_icon <- makeIcon(iconUrl = "Img/NEON.png",
                      iconWidth = 30, iconHeight = 30,
                      iconAnchorX = 15, iconAnchorY = 15,
                      popupAnchorX = -1, popupAnchorY = -15)