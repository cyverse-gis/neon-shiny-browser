# Load only the packages that are actually available
# Core packages (required)
library(shiny)
library(DT)
library(jsonlite)
library(dplyr)

# Optional packages with error handling
optional_packages <- c("shinythemes", "shinyWidgets", "shinyBS", "shinyjs", 
                      "leaflet", "leaflet.extras", "sf", "neonUtilities", "geosphere")

for (pkg in optional_packages) {
  tryCatch({
    library(pkg, character.only = TRUE)
    assign(paste0(pkg, "_available"), TRUE, envir = .GlobalEnv)
  }, error = function(e) {
    message(sprintf("Optional package '%s' not available, some features may be limited", pkg))
    assign(paste0(pkg, "_available"), FALSE, envir = .GlobalEnv)
  })
}
# Source the custom functions needed for the app
source('Functions/flight_function.R')
source('Functions/filter_keyword_function.R')
source('Functions/filter_site_function.R')
source('Functions/keyword_lists_function.R')
source('Functions/spatial_data_ui.R')

# Source functions that require additional packages only if available
tryCatch(source('Functions/getProductSize_function.R'), error = function(e) {
  message("getProductSize_function.R not loaded (missing dependencies)")
})
tryCatch(source('Functions/unzipEddy_function.R'), error = function(e) {
  message("unzipEddy_function.R not loaded (missing dependencies)")
})
tryCatch(source('Functions/datesTable_function.R'), error = function(e) {
  message("datesTable_function.R not loaded (missing dependencies)")
})
tryCatch(source('Functions/checkDownload_function.R'), error = function(e) {
  message("checkDownload_function.R not loaded (missing dependencies)")
})
tryCatch(source('Functions/unique_folderpath_function.R'), error = function(e) {
  message("unique_folderpath_function.R not loaded (missing dependencies)")
})
tryCatch(source('Functions/write_downloadSummary_function.R'), error = function(e) {
  message("write_downloadSummary_function.R not loaded (missing dependencies)")
})

# Source neonUtilities functions only if neonUtilities is available
if (exists("neonUtilities_available") && neonUtilities_available) {
  tryCatch({
    source("Functions/neonUtilities.R")
    source('Functions/neonUtilities_replacements.R')
  }, error = function(e) {
    message("neonUtilities functions not loaded (missing dependencies)")
  })
} else {
  message("neonUtilities package not available, skipping related functions")
}

# Create aliases for backward compatibility with existing code (if functions exist)
if (exists("nneo_products_replacement")) {
  nneo_products <- nneo_products_replacement
} else {
  nneo_products <- function(...) stop("neonUtilities not available")
}
if (exists("nneo_data_replacement")) {
  nneo_data <- nneo_data_replacement
} else {
  nneo_data <- function(...) stop("neonUtilities not available")
}
if (exists("nneo_site_replacement")) {
  nneo_site <- nneo_site_replacement
} else {
  nneo_site <- function(...) stop("neonUtilities not available")
}

if (!dir.exists("../NEON_Downloads")) {
  dir.create("../NEON_Downloads")
  dir_created <- TRUE
} else {
  dir_created <- FALSE
}
if (!('geosphere' %in% as.data.frame(installed.packages())$Package)) {
  print("Please install the package 'geosphere'. Run this line: 'install.packages('geosphere')'")
  stop(call. = TRUE)
}

####———MAP DATA———####


####——NEON——####

###NEON Field Sites####

## Retrieve point data for NEON Field Sites in JSON format
FieldSite_point_JSON <- fromJSON('https://data.neonscience.org/api/v0/sites')
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

# Check if stateCode exists, if not create it from stateName or set a default
if (!"stateCode" %in% names(FieldSite_point)) {
  message("WARNING: stateCode not found in FieldSite_point, creating from available data...")
  if ("stateName" %in% names(FieldSite_point)) {
    # Create state codes from state names if available
    FieldSite_point$stateCode <- substr(FieldSite_point$stateName, 1, 2)
  } else {
    # Default to empty if no state information available
    FieldSite_point$stateCode <- rep("", nrow(FieldSite_point))
  }
}

# List of field site abbreviations
message("DEBUG: Creating FieldSite_abbs...")
message(sprintf("  - FieldSite_point$siteCode class: %s", class(FieldSite_point$siteCode)[1]))
if (is.function(FieldSite_point$siteCode)) {
  message("  ERROR: FieldSite_point$siteCode is a function!")
  FieldSite_abbs <- character(0)  # Create empty character vector as fallback
} else {
  FieldSite_abbs <- FieldSite_point$siteCode
  message(sprintf("  - FieldSite_abbs created with %d elements", length(FieldSite_abbs)))
}
FieldSite_Tes <- FieldSite_point$siteCode[FieldSite_point$Habitat %in% "Terrestrial"]
FieldSite_Aqu <- FieldSite_point$siteCode[FieldSite_point$Habitat %in% "Aquatic"]

## Retrieve polygon data for NEON Field Sites
# Use local JSON file directly (API is unreliable)
message("Loading field site polygons from local file...")
Fieldsite_poly_JSON <- fromJSON('NEON-data/Fieldsites.json')
message(sprintf("Loaded %d of %d total field site records from local file", 
                length(Fieldsite_poly_JSON$hits$hits), 
                Fieldsite_poly_JSON$hits$total))

# Check if we have valid data
if (length(Fieldsite_poly_JSON$hits$hits) == 0) {
  message("Warning: No field site polygon data available")
  FieldSite_poly <- data.frame()
} else {
  # Process the available data
  FieldSite_poly <- cbind(Fieldsite_poly_JSON$hits$hits[-5], 
                         Fieldsite_poly_JSON$hits$hits$`_source`[-4], 
                         Fieldsite_poly_JSON$hits$hits$`_source`$boundary)
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
  
  message(sprintf("✓ Processed %d field site polygons", nrow(FieldSite_poly)))
}

## Retrive Fieldsite Locations
FieldSite_locations_tes <- read.csv("NEON-data/Fieldsites_locations_tes", stringsAsFactors = FALSE)
FieldSite_plots_tes <- read.csv("NEON-data/Fieldsites_plots_tes", stringsAsFactors = FALSE)[-1]
FieldSite_locations_aqu <- read.csv("NEON-data/Fieldsites_locations_aqu", stringsAsFactors = FALSE)

# The new aquatic locations file has a simplified structure with just main sites
# Add General Type based on the simplified Type field
for (i in 1:nrow(FieldSite_locations_aqu)) {
  # Check if Type field exists and is not NA
  if ("Type" %in% names(FieldSite_locations_aqu) && !is.na(FieldSite_locations_aqu$Type[i])) {
    FieldSite_locations_aqu$`General Type`[i] <- if (FieldSite_locations_aqu$Type[i] == "STREAM") {
      "Stream Site"
    } else if (FieldSite_locations_aqu$Type[i] == "LAKE") {
      "Lake Site"
    } else {
      "Aquatic Site"
    }
  } else {
    FieldSite_locations_aqu$`General Type`[i] <- "Aquatic Site"
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
# Retrieve geometry data using st_read() if sf is available
if (sf_available) {
  domain_data <- st_read('NEON-data/NEON_Domains.json')
  domain_data <- as.data.frame(domain_data)
} else {
  # Create a placeholder when sf is not available
  domain_data <- data.frame(DomainID = character(), DomainName = character())
}

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
# Load modern spatial data with automatic caching and updates (if sf is available)
if (sf_available) {
  tryCatch({
    source('Functions/load_spatial_data.R')
    # Initialize flight_data as NULL to avoid function/variable conflicts
    flight_data <- NULL
    # Initialize spatial data - will check for updates and cache locally
    load_neon_spatial_data(force_update = FALSE, check_updates = TRUE)
  }, error = function(e) {
    message("Modern spatial data loading failed, using legacy methods...")
    flight_data <<- NULL
  })
} else {
  message("sf package not available, skipping modern spatial data loading...")
  flight_data <- NULL
}

# Debug: Check what flight_data contains after spatial data loading
if (exists("flight_data")) {
  if (is.function(flight_data)) {
    message("ERROR: flight_data is still a function after spatial loading!")
  } else if (is.data.frame(flight_data)) {
    message(sprintf("✓ flight_data is a data frame with %d rows", nrow(flight_data)))
  } else {
    message(sprintf("WARNING: flight_data is of type: %s", class(flight_data)))
  }
}

message("DEBUG: Global.R initialization complete - about to start Shiny server")
message("DEBUG: Checking key variables before server start:")
message(sprintf("  - FieldSite_point: %s with %d rows", class(FieldSite_point)[1], if(exists("FieldSite_point") && is.data.frame(FieldSite_point)) nrow(FieldSite_point) else 0))
message(sprintf("  - FieldSite_poly: %s with %d rows", class(FieldSite_poly)[1], if(exists("FieldSite_poly") && is.data.frame(FieldSite_poly)) nrow(FieldSite_poly) else 0))
message(sprintf("  - FieldSite_plots_tes: %s with %d rows", class(FieldSite_plots_tes)[1], if(exists("FieldSite_plots_tes") && is.data.frame(FieldSite_plots_tes)) nrow(FieldSite_plots_tes) else 0))
message(sprintf("  - domains: %s with %d rows", class(domains)[1], if(exists("domains") && is.data.frame(domains)) nrow(domains) else 0))
message(sprintf("  - flight_data: %s with %d rows", class(flight_data)[1], if(exists("flight_data") && is.data.frame(flight_data)) nrow(flight_data) else 0))

#### Miscellaneous Variables ####

NEON_datatypes <- c("Airborne Observation Platform (AOP)", "Aquatic Instrument System (AIS)", "Aquatic Observation System (AOS)","Terrestrial Instrument System (TIS)", "Terrestrial Observation System (TOS)")

# Debug: Check critical UI variables before they're used in Ui.R
message("DEBUG: Checking UI variables that might cause coercion errors:")
message(sprintf("  - FieldSite_abbs: %s with length %d", class(FieldSite_abbs)[1], if(exists("FieldSite_abbs")) length(FieldSite_abbs) else 0))
if (exists("FieldSite_abbs") && is.function(FieldSite_abbs)) {
  message("  ERROR: FieldSite_abbs is a function! This will cause UI coercion error!")
}
message(sprintf("  - NEON_datatypes: %s with length %d", class(NEON_datatypes)[1], length(NEON_datatypes)))
if (is.function(NEON_datatypes)) {
  message("  ERROR: NEON_datatypes is a function! This will cause UI coercion error!")
}
message(sprintf("  - domains$Domain: %s with length %d", class(domains$Domain)[1], if(exists("domains") && "Domain" %in% names(domains)) length(domains$Domain) else 0))
if (exists("domains") && "Domain" %in% names(domains) && is.function(domains$Domain)) {
  message("  ERROR: domains$Domain is a function! This will cause UI coercion error!")
  # Fix the domains$Domain if it's a function
  domains$Domain <<- as.character(domains$Domain)
  message("  Fixed: converted domains$Domain to character")
}

# Check the unique() calls on stateCode
message("  - Checking FieldSite_point$stateCode for UI usage:")
if ("stateCode" %in% names(FieldSite_point)) {
  message(sprintf("    - FieldSite_point$stateCode class: %s, length: %d", 
                  class(FieldSite_point$stateCode)[1], 
                  length(FieldSite_point$stateCode)))
  message(sprintf("    - unique(FieldSite_point$stateCode) length: %d", 
                  length(unique(FieldSite_point$stateCode))))
} else {
  message("    - ERROR: stateCode column not found in FieldSite_point!")
}
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
if (leaflet_available) {
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
} else {
  # Create placeholder icons when leaflet is not available
  NEON_icon <- NULL
  NEON_locations_tes <- NULL
  NEON_locations_aqu <- NULL
}
