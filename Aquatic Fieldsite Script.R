for (i in FieldSite_Aqu) {
  # Get top heirarchy for site
  data <- httr::GET(paste0("http://data.neonscience.org/api/v0/locations/", i))
  data <- fromJSON(httr::content(data, as = "text"))$data
  children_names <- data$locationChildren
  children_urls <- data$locationChildrenUrls
  # Create locations and plots data frames
  assign(x = paste0(i, "_locations"), value = data.frame("Site" = i, "Description" = NA, "Name" = NA, "Type" = NA, "Latitude" = NA, "Longitude" = NA, "Properties" = NA, "Values" = NA, "Parent" = NA, "Children" = NA, stringsAsFactors = FALSE), envir = .GlobalEnv)
  # get locations
  for (url in children_urls) {
    #child_data <- fromJSON(url)$data
    child_table <- httr::GET(url)
    child_table <- fromJSON(httr::content(child_table, as = "text"))$data
    latitude <- ifelse(is.null(child_table$locationDecimalLatitude), NA, child_table$locationDecimalLatitude)
    longitude <- ifelse(is.null(child_table$locationDecimalLongitude), NA, child_table$locationDecimalLongitude)
    properties <- list(child_table$locationProperties$locationPropertyName)
    values <- list(child_table$locationProperties$locationPropertyValue)
    parent <- TRUE
    children <- list(child_table$locationChildren)
    child_data <- cbind("Site" = i, "Description" = child_table$locationDescription, "Name" = child_table$locationName, "Type" = child_table$locationType, "Latitude" = latitude, "Longitude" = longitude, "Properties" = properties, "Values" = values, "Parent" = parent, "Children" = children)
    child_data <- as.data.frame(child_data, stringsAsFactors = FALSE)
    assign(x = paste0(i, "_locations"), value = rbind(get(paste0(i, "_locations")), child_data))
    
    for (URL in child_table$locationChildrenUrls) {
      child_data_2 <- httr::GET(URL)
      child_data_2 <- fromJSON(httr::content(child_data_2, as = "text"))$data
      latitude_2 <- ifelse(is.null(child_data_2$locationDecimalLatitude), NA, child_data_2$locationDecimalLatitude)
      longitude_2 <- ifelse(is.null(child_data_2$locationDecimalLongitude), NA, child_data_2$locationDecimalLongitude)
      properties_2 <- list(child_data_2$locationProperties$locationPropertyName)
      values_2 <- list(child_data_2$locationProperties$locationPropertyValue)
      parent_2 <- child_data_2$locationParent
      children_2 <- ifelse(is.null(child_data_2$locationChildren), NA, child_data_2$locationChildren)
      child_data_2 <- cbind("Site" = i,"Description" = child_data_2$locationDescription, "Name" = child_data_2$locationName, "Type" = child_data_2$locationType, "Latitude" = latitude_2, "Longitude" = longitude_2, "Properties" = properties_2, "Values" = values_2, "Parent" = parent_2, "Children" = children_2)
      child_data_2 <- as.data.frame(child_data_2, stringsAsFactors = FALSE)
      assign(x = paste0(i, "_locations"), value = rbind(get(paste0(i, "_locations")), child_data_2))
    }
  }
  assign(x = paste0(i, "_locations"), value = get(paste0(i, "_locations"))[-1,], envir = .GlobalEnv)
}
