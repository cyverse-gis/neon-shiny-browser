#### Part 1 ####
for (i in FieldSite_Tes) {
  # Get top heirarchy for site
  data <- httr::GET(paste0("http://data.neonscience.org/api/v0/locations/", i))
  data <- fromJSON(httr::content(data, as = "text"))$data
  children_names <- data$locationChildren
  children_urls <- data$locationChildrenUrls
  # Find locations (everything that's not a plot)
  location_filter <- !startsWith(x = children_names, prefix = i)
  # Find plots of desired type
  plot_filter <- endsWith(x = children_names, suffix = "all") | endsWith(x = children_names, suffix = "brd") | endsWith(x = children_names, suffix = "mam") | endsWith(x = children_names, suffix = "mos") | endsWith(x = children_names, suffix = "tck") | endsWith(x = children_names, suffix = "phe")
  # Create locations and plots data frames
  assign(x = paste0(i, "_locations"), value = data.frame("Site" = NA, "Description" = NA, "Name" = NA, "Type" = NA, "Latitude" = NA, "Longitude" = NA, "Properties" = NA, "Values" = NA, stringsAsFactors = FALSE), envir = .GlobalEnv)
  assign(x = paste0(i, "_plots"), value = data.frame("Site" = NA, "Description" = NA, "Name" = NA, "Type" = NA, "Latitude" = NA, "Longitude" = NA, "Properties" = NA, "Values" = NA, stringsAsFactors = FALSE), envir = .GlobalEnv)  
  # get locations
  for (url in children_urls[location_filter]) {
    child_data <- httr::GET(url)
    child_data <- fromJSON(httr::content(child_data, as = "text"))$data
    latitude <- ifelse(is.null(child_data$locationDecimalLatitude), NA, child_data$locationDecimalLatitude)
    longitude <- ifelse(is.null(child_data$locationDecimalLongitude), NA, child_data$locationDecimalLongitude)
    properties <- list(child_data$locationProperties$locationPropertyName)
    values <- list(child_data$locationProperties$locationPropertyValue)
    child_data <- cbind("Site" = i, "Description" = child_data$locationDescription, "Name" = child_data$locationName, "Type" = child_data$locationType, "Latitude" = latitude, "Longitude" = longitude, "Properties" = properties, "Values" = values)
    child_data <- as.data.frame(child_data, stringsAsFactors = FALSE)
    assign(x = paste0(i, "_locations"), value = rbind(get(paste0(i, "_locations")), child_data))
  }
  # get plots
  for (url in children_urls[plot_filter]) {
    #child_data <- fromJSON(url)$data
    child_data <- httr::GET(url)
    child_data <- fromJSON(httr::content(child_data, as = "text"))$data
    latitude <- ifelse(is.null(child_data$locationDecimalLatitude), NA, child_data$locationDecimalLatitude)
    longitude <- ifelse(is.null(child_data$locationDecimalLongitude), NA, child_data$locationDecimalLongitude)
    properties <- list(child_data$locationProperties$locationPropertyName)
    values <- list(child_data$locationProperties$locationPropertyValue)
    child_data <- cbind("Site" = i, "Description" = child_data$locationDescription, "Name" = child_data$locationName, "Type" = child_data$locationType, "Latitude" = latitude, "Longitude" = longitude, "Properties" = properties, "Values" = values)
    child_data <- as.data.frame(child_data, stringsAsFactors = FALSE)
    assign(x = paste0(i, "_plots"), value = rbind(get(paste0(i, "_plots")), child_data))
  }
  assign(x = paste0(i, "_locations"), value = get(paste0(i, "_locations"))[-1,], envir = .GlobalEnv)
  assign(x = paste0(i, "_plots"), value = get(paste0(i, "_plots"))[-1,], envir = .GlobalEnv)
  # for (n in 1:ncol(get(paste0(i, "_locations")))) {
  #   assign(paste0(i, "_locations"), `[[<-`(get(paste0(i, "_locations")), n, value = as.character(get(paste0(i, "_locations"))[[n]]))) 
  # }
  # assign(x = paste0(i, "_locations"), value = get(paste0(i, "_locations"))[-1,], envir = .GlobalEnv)
  # for (n in 1:ncol(get(paste0(i, "_plots")))) {
  #   assign(paste0(i, "_plots"), `[[<-`(get(paste0(i, "_plots")), n, value = as.character(get(paste0(i, "_plots"))[[n]]))) 
  # }
  # assign(x = paste0(i, "_plots"), value = get(paste0(i, "_plots"))[order(get(paste0(i, "_plots"))$Description),], envir = .GlobalEnv)
}

#### Part 1.5- bind all variables ####
FieldSites_locations <- data.frame("Site" = NA, "Description" = NA, "Name" = NA, "Type" = NA, "Latitude" = NA, "Longitude" = NA, "Properties" = NA, "Values" = NA)
for (i in FieldSite_Tes) {
  location <- get(paste0(i, "_locations"))
  assign(x = "FieldSites_locations", value = rbind(FieldSites_locations, location))
}

for (i in 1:ncol(FieldSites_locations)) {
  FieldSites_locations[[i]] <- as.character(FieldSites_locations[[i]])
}

#### Part 2- plots ####
for (i in 1:nrow(FieldSite_plots_tes)) {
  FieldSite_plots_tes$`Plot Size`[i] <- if (FieldSite_plots_tes$Type[i] == "OS Plot - all") {
    "40m x 40m"
  } else if (FieldSite_plots_tes$Type[i] == "OS Plot - brd") {
    "500m x 500m"
  } else if (FieldSite_plots_tes$Type[i] == "OS Plot - mos") {
    "NA"
  } else if (FieldSite_plots_tes$Type[i] == "OS Plot - mam") {
    "90m x 90m"
  } else if (FieldSite_plots_tes$Type[i] =="OS Plot - tck") {
    "40m x 40m"
  }
  FieldSite_plots_tes$Type[i] <- if (FieldSite_plots_tes$Type[i] == "OS Plot - all") {
    "Distributed Base Plot"
  } else if (FieldSite_plots_tes$Type[i] == "OS Plot - brd") {
    "Distributed Bird Grid"
  } else if (FieldSite_plots_tes$Type[i] == "OS Plot - mos") {
    "Distributed Mosquito Plot"
  } else if (FieldSite_plots_tes$Type[i] == "OS Plot - mam") {
    "Distributed Mammal Grid"
  } else if (FieldSite_plots_tes$Type[i] =="OS Plot - tck") {
    "Distributed Tick Plot"
  }
  FieldSite_plots_tes$Description[i] <- strsplit(FieldSite_plots_tes$Name[i], "[.]")[[1]][1]
}

