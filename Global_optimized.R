# Optimized Global.R with improved loading strategy
# =================================================
# Phase 1: Critical Dependencies Only
# =================================================

# Load only essential packages first
library(shiny)
library(DT)
library(jsonlite)
library(dplyr)

# Initialize global variables early
dir_created <- FALSE
NEON_datatypes <- c("Airborne Observation Platform (AOP)", "Aquatic Instrument System (AIS)", 
                   "Aquatic Observation System (AOS)", "Terrestrial Instrument System (TIS)", 
                   "Terrestrial Observation System (TOS)")

# =================================================
# Phase 2: Package Manager with Parallel Loading
# =================================================

# Function to load packages in parallel
load_packages_async <- function() {
  optional_packages <- c("shinythemes", "shinyWidgets", "shinyBS", "shinyjs", 
                        "leaflet", "leaflet.extras", "sf", "neonUtilities", "geosphere")
  
  # Create availability tracker
  package_status <- setNames(rep(FALSE, length(optional_packages)), optional_packages)
  
  # Load packages with error handling
  for (pkg in optional_packages) {
    tryCatch({
      library(pkg, character.only = TRUE)
      package_status[[pkg]] <- TRUE
      assign(paste0(pkg, "_available"), TRUE, envir = .GlobalEnv)
    }, error = function(e) {
      message(sprintf("Optional package '%s' not available, some features may be limited", pkg))
      assign(paste0(pkg, "_available"), FALSE, envir = .GlobalEnv)
    })
  }
  
  return(package_status)
}

# Load packages
package_status <- load_packages_async()

# =================================================
# Phase 3: Core Functions (Non-API Dependent)
# =================================================

# Load essential functions that don't require API data
core_functions <- c(
  'Functions/filter_keyword_function.R',
  'Functions/filter_site_function.R', 
  'Functions/keyword_lists_function.R',
  'Functions/spatial_data_ui.R'
)

for (func_file in core_functions) {
  tryCatch({
    source(func_file)
  }, error = function(e) {
    message(sprintf("Warning: %s not loaded - %s", func_file, e$message))
  })
}

# =================================================
# Phase 4: Lightweight Data Initialization
# =================================================

# Create download directory
if (!dir.exists("../NEON_Downloads")) {
  dir.create("../NEON_Downloads")
  dir_created <- TRUE
}

# Initialize environment for NEON keywords
if (!exists(".NEON_keywords")) {
  .NEON_keywords <<- new.env()
}

# Initialize placeholder data structures
FieldSite_point <- data.frame()
FieldSite_poly <- data.frame()
FieldSite_abbs <- character(0)
domains <- data.frame(DomainID = character(), Domain = character())
flight_data <- NULL

# =================================================
# Phase 5: Reactive Data Loading System
# =================================================

# Function to load API data asynchronously after app starts
load_neon_data_async <- function() {
  future::future({
    # Load field site data
    tryCatch({
      FieldSite_point_JSON <- fromJSON('https://data.neonscience.org/api/v0/sites')
      FieldSite_point <<- FieldSite_point_JSON$data
      FieldSite_point$domainCode <<- as.numeric(gsub("D", "", FieldSite_point$domainCode))
      
      # Add extra info
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
      
      FieldSite_point$Habitat <<- FieldSite_extra$Site.Type
      FieldSite_point$`Habitat Specific` <<- FieldSite_extra$Site.Subtype
      FieldSite_point$Host <<- FieldSite_extra$Site.Host
      
      # Create state codes if missing
      if (!"stateCode" %in% names(FieldSite_point)) {
        if ("stateName" %in% names(FieldSite_point)) {
          FieldSite_point$stateCode <<- substr(FieldSite_point$stateName, 1, 2)
        } else {
          FieldSite_point$stateCode <<- rep("", nrow(FieldSite_point))
        }
      }
      
      FieldSite_abbs <<- FieldSite_point$siteCode
      message("✓ Field site data loaded successfully")
      
    }, error = function(e) {
      message(sprintf("Error loading field site data: %s", e$message))
    })
    
    # Load polygon data
    tryCatch({
      Fieldsite_poly_JSON <- fromJSON('NEON-data/Fieldsites.json')
      if (length(Fieldsite_poly_JSON$hits$hits) > 0) {
        FieldSite_poly <<- cbind(Fieldsite_poly_JSON$hits$hits[-5], 
                               Fieldsite_poly_JSON$hits$hits$`_source`[-4], 
                               Fieldsite_poly_JSON$hits$hits$`_source`$boundary)
        names(FieldSite_poly)[9] <<- "geo_type"
        FieldSite_poly <<- FieldSite_poly %>% filter(type %in% "NEON")
        
        # Process polygon data
        for (i in 1:nrow(FieldSite_poly)) {
          FieldSite_poly$code[i] <<- strsplit(FieldSite_poly$code[i], "-")[[1]][2]
          FieldSite_poly$siteType[i] <<- strsplit(FieldSite_poly$name[i], ", ")[[1]][2]
          FieldSite_poly$name[i] <<- strsplit(FieldSite_poly$name[i], ", ")[[1]][1]
          FieldSite_poly$domainName[i] <<- strsplit(FieldSite_poly$details[[i]][1], ":")[[1]][2]
          FieldSite_poly$domainCode[i] <<- strsplit(FieldSite_poly$details[[i]][2], ":")[[1]][2]
          FieldSite_poly$domainCode[i] <<- strsplit(FieldSite_poly$domainCode[i], "D")[[1]][2]  
          FieldSite_poly$stateCode[i] <<- strsplit(FieldSite_poly$details[[i]][5], ":")[[1]][2]
          FieldSite_poly$stateName[i] <<- strsplit(FieldSite_poly$details[[i]][6], ":")[[1]][2]
        }
        FieldSite_poly$domainCode <<- as.numeric(FieldSite_poly$domainCode)
        message("✓ Field site polygon data loaded successfully")
      }
    }, error = function(e) {
      message(sprintf("Error loading polygon data: %s", e$message))
    })
  })
}

# =================================================
# Phase 6: Conditional Heavy Component Loading
# =================================================

# Function to load heavy components only when needed
load_heavy_components <- function() {
  # Load location files
  tryCatch({
    FieldSite_locations_tes <<- read.csv("NEON-data/Fieldsites_locations_tes", stringsAsFactors = FALSE)
    FieldSite_plots_tes <<- read.csv("NEON-data/Fieldsites_plots_tes", stringsAsFactors = FALSE)[-1]
    FieldSite_locations_aqu <<- read.csv("NEON-data/Fieldsites_locations_aqu", stringsAsFactors = FALSE)
    
    # Process aquatic locations
    for (i in 1:nrow(FieldSite_locations_aqu)) {
      if ("Type" %in% names(FieldSite_locations_aqu) && !is.na(FieldSite_locations_aqu$Type[i])) {
        FieldSite_locations_aqu$`General Type`[i] <<- if (FieldSite_locations_aqu$Type[i] == "STREAM") {
          "Stream Site"
        } else if (FieldSite_locations_aqu$Type[i] == "LAKE") {
          "Lake Site"
        } else {
          "Aquatic Site"
        }
      } else {
        FieldSite_locations_aqu$`General Type`[i] <<- "Aquatic Site"
      }
    }
    message("✓ Location data loaded successfully")
  }, error = function(e) {
    message(sprintf("Error loading location data: %s", e$message))
  })
  
  # Load domain data
  tryCatch({
    domains_json <- fromJSON('NEON-data/NEON_Domains.json')
    domains <<- cbind("DomainID" = domains_json$features$properties$DomainID,
                     "Domain" = domains_json$features$properties$DomainName)
    domains <<- as.data.frame(unique(domains))
    domains$Domain <<- as.character(domains$Domain)
    
    if (sf_available) {
      domain_data <<- st_read('NEON-data/NEON_Domains.json')
      domain_data <<- as.data.frame(domain_data)
    }
    message("✓ Domain data loaded successfully")
  }, error = function(e) {
    message(sprintf("Error loading domain data: %s", e$message))
  })
}

# =================================================
# Phase 7: Deferred Function Loading
# =================================================

# Load API-dependent functions only when needed
load_api_functions <- function() {
  api_functions <- c(
    'Functions/getProductSize_function.R',
    'Functions/unzipEddy_function.R',
    'Functions/datesTable_function.R',
    'Functions/checkDownload_function.R',
    'Functions/unique_folderpath_function.R',
    'Functions/write_downloadSummary_function.R'
  )
  
  for (func_file in api_functions) {
    tryCatch({
      source(func_file)
    }, error = function(e) {
      message(sprintf("%s not loaded (missing dependencies): %s", func_file, e$message))
    })
  }
  
  # Load neonUtilities functions if available
  if (exists("neonUtilities_available") && neonUtilities_available) {
    tryCatch({
      source("Functions/neonUtilities.R")
      source('Functions/neonUtilities_replacements.R')
      
      # Create backward compatibility aliases
      if (exists("nneo_products_replacement")) {
        nneo_products <<- nneo_products_replacement
      } else {
        nneo_products <<- function(...) stop("neonUtilities not available")
      }
      if (exists("nneo_data_replacement")) {
        nneo_data <<- nneo_data_replacement
      } else {
        nneo_data <<- function(...) stop("neonUtilities not available")
      }
      if (exists("nneo_site_replacement")) {
        nneo_site <<- nneo_site_replacement
      } else {
        nneo_site <<- function(...) stop("neonUtilities not available")
      }
      message("✓ neonUtilities functions loaded successfully")
    }, error = function(e) {
      message("neonUtilities functions not loaded (missing dependencies)")
    })
  }
}

# =================================================
# Phase 8: UI Components Preparation
# =================================================

# Prepare UI components that require data
prepare_ui_components <- function() {
  # Flight box table
  FieldSite_table <<- data.frame(
    "Abb" = c("BART","HARV","BLAN","SCBI","SERC","DSNY","JERC","OSBS","STEI-CHEQ","STEI-TREE","UNDE","KONZ-KONA","GRSM","MLBS","ORNL","DELA","LENO","TALL","DCFS-WOOD","NOGP","CLBJ","OAES","CHEQ", "BARO"),
    "Site" = c("Bartlett Experimental Forest North-South flight box", "Harvard Forest flight box","Blandy Experimental Farm flight box","Smithsonian Conservation Biology Institute flight box","Smithsonian Ecological Research Center flight box","Disney Wilderness Preserve flight box","Jones Ecological Research Center Priority 1 flight box","Ordway-Swisher Biological Station Priority 1 flight box","Chequamegon-Nicolet National Forest flight box","Steigerwaldt-Treehaven Priority 2 flight box","UNDERC flight box","Konza Prairie Biological Station and KONA agricultural site flight box","Great Smoky Mountains National Park priority 2 flight box","Mountain Lake Biological Station flight box","Oak Ridge National Laboratory flight box","Dead Lake flight box","Lenoir Landing flight box","Talladega National Forest flight box","Woodworth and Dakota Coteau Field School flight box","Northern Great Plains flight box","LBJ Grasslands flight box","Klemme Range Research Station flight box", "Chequamegon-Nicolet National Forest", "Barrow")
  )
  
  CR_table <<- data.frame("Abb" = c("C", "R", "A"), "Actual" = c("Core", "Relocatable", "Aquatic"), stringsAsFactors = FALSE)
  
  # Map icons
  if (leaflet_available) {
    NEON_icon <<- makeIcon(iconUrl = "Img/NEON.png", iconWidth = 30, iconHeight = 30,
                          iconAnchorX = 15, iconAnchorY = 15, popupAnchorX = -1, popupAnchorY = -15)
    
    NEON_locations_tes <<- iconList(
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
    
    NEON_locations_aqu <<- iconList(
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
    NEON_icon <<- NULL
    NEON_locations_tes <<- NULL
    NEON_locations_aqu <<- NULL
  }
  
  # Text definitions
  baseplot_text <<- "30/site: Distributed Base Plots support a variety of plant productivity, plant diversity, soil, biogeochemistry, microbe and beetle sampling. Distributed Base Plots are 40m x 40m."
  birdgrid_text <<- "5-15/site: Bird Grids consist of 9 sampling points within a 500m x 500m square. Each point is 250m apart. Where possible, Bird Grids are colocated with Distributed Base Plots by placing the Bird Grid center in close proximity to the center of the Base Plot. At smaller sites, a single point count is done at the south-west corner of the Distributed Base Plot."
  mammalgrid_text <<- "6-8/site: Mammal Grids are 90m x 90m and include 10m spacing. Where possible, these grids are colocated with Distributed Base Plots by placing them a specified distance (150m +/- 50m) and random direction from the center of the Base Plot."
  mosquitoplot_text <<- "10/site: At each Mosquito Point, one CO2 trap is established. Due to the frequency of sampling and the temporal sampling constraints, Mosquito Points are located within 45m of roads."
  tickplot_text <<- "6/site: Tick Plots are sampled by conducting cloth dragging or flagging around the perimeter of a 40m x 40m plot. Tick plots are colocated with Distributed Base Plots by placing them a specified distance (150m +/- 15m) and random direction from the center of the Base Plot."
  phenologyplot_text <<- "1-2/site: Plant phenology observations are made along a transect loop or plot in or around the primaru airshed. When possible, one plot is established north of the tower to calibrate phenology camera images captured from sensors on the tower. If there is insufficient space north of the tower for a 200m x 200m plot or if the vegetation does not match the primary airshed an additional plot is established."
  well_text <<- "Each site has up to eight groundwater wells outfitted with sensors that measure high temporal resolution groundwater elevation (pressure transducer-based), temperature, and specific conductance."
  metstn_text <<- "A met. station is located on the shore of the most aquatic sites and collects data comparable with flux tower measurements at terrestrial sites. Lake and wadeable rivers also have an above water met. station buoy. These data are unique with different sensors and data frequencies due to power and data storage constraints."
  sensor_text <<- "Wadeable streams have a sensor station near the top of the reach and the bottom of the reach; non-wadeable rivers have a sensor station on a buoy and one near the bank; Lakes have an inlet sensor stations, and outlet sensor station and a sensor station sensor on a buoy. Data collection varies by type of sensor station. Click on sensor station on the map to learn more."
  gauge_text <<- "The staff gauge measures gauge height, in meters, measured at lakes, wadeable rivers and non-wadeable streams. A phenocam is installed near most gauges. It collects RGB and IR images of the lake, river, or stream vegetation, stream surface, and stream gauge every 15 minutes."
  reach_text <<- "These icons mark the top and bottom of the observational sampling reach at wadeable streams and non-wadeable rivers. The reach for lake sites is the shape of the lake itself. Observational sampling activities may include: Reaeration sampling; water chemistry, isotopes, dissolved gas; zooplankton, phytoplankton; secchi depth profile; bathymetric and morphologic mapping; discharge; sediment chemistry; riparian assessment; macroinvertebrates, plants, algae, and microbes; and fish."
  riparian_text <<- "Number of locations for assessment of riparian vegetation composition and physical structure vary by site type. Lakes and non-wadeable rivers have ten locations. Wadeable streams have 20 locations and also include assessment of riparian vegetation percent cover in wadeable streams."
}

# =================================================
# Phase 9: Startup Coordinator
# =================================================

# Track loading progress
loading_progress <- reactiveValues(
  packages = FALSE,
  core_functions = FALSE,
  api_data = FALSE,
  heavy_components = FALSE,
  ui_components = FALSE
)

# Startup sequence manager
startup_manager <- function() {
  # Phase 1: Immediate needs
  loading_progress$packages <- TRUE
  loading_progress$core_functions <- TRUE
  
  # Phase 2: Deferred loading (after UI renders)
  shiny::later(function() {
    load_heavy_components()
    loading_progress$heavy_components <- TRUE
    
    prepare_ui_components()
    loading_progress$ui_components <- TRUE
    
    load_api_functions()
    
    # Start async data loading
    load_neon_data_async()
    loading_progress$api_data <- TRUE
    
    message("✓ All components loaded successfully")
  }, delay = 0.1)
}

# Initialize the startup manager
startup_manager()

message("✓ Optimized Global.R initialization complete")