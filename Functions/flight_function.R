process_flight_data <- function(flightlist_info, flightlist_geo, year, name) {
  # flight info
  flight_info <- data.frame()
  for (file in flightlist_info) {
    parts <- strsplit(file, "_")
    #EX: "Flightdata/Flight_boundaries_2016/D01_BART_R1_P1_v1.geojson"
    name_part <- strsplit(file, "/")[[1]][4]
    # D01_BART_R1_P1_v1.geojson
    domain_part <- strsplit(parts[[1]][3],"D")[[1]][2]
    # 1
    site_part <- parts[[1]][4]
    #BART
    RC_part_type <- strsplit(parts[[1]][5],"")[[1]][1]
    # R
    RC_part_num <- strsplit(parts[[1]][5],"")[[1]][2]
    # 1
    priority_part <- strsplit(parts[[1]][6],"")[[1]][2]
    # 1
    version_part <- strsplit(parts[[1]][7],"")[[1]][2]
    # 1
    file_info <- cbind("Name" = name_part,
                       "DomainID" = domain_part,
                       "SiteAbb" = site_part,
                       "Site" = {
                         # Safe site name lookup
                         if (exists("FieldSite_table") && is.data.frame(FieldSite_table) && 
                             "Abb" %in% names(FieldSite_table) && "Site" %in% names(FieldSite_table)) {
                           matches <- FieldSite_table$Abb %in% site_part
                           if (any(matches)) {
                             site_name <- FieldSite_table$Site[matches]
                             if (length(site_name) > 0 && !is.function(site_name)) {
                               as.character(site_name[1])
                             } else {
                               "Unknown Site"
                             }
                           } else {
                             "Unknown Site"
                           }
                         } else {
                           "Unknown Site"
                         }
                       },
                       "SiteType" = toupper(CR_table[grep(RC_part_type,CR_table$Abb),2]),
                       "SiteType_number" = RC_part_num,
                       "Priority" = priority_part,
                       "Version" = version_part,
                       "Year" = as.character(year))
    flight_info <- rbind(flight_info, file_info)
  }
  # Safe DomainID conversion
  tryCatch({
    if ("DomainID" %in% names(flight_info) && !is.function(flight_info$DomainID)) {
      flight_info$DomainID <- as.numeric(as.character(flight_info$DomainID))
    }
  }, error = function(e) {
    message(sprintf("Warning: Could not convert DomainID to numeric: %s", e$message))
    flight_info$DomainID <<- 1  # Default domain ID
  })
  # flight geo
  flight_geo <- st_read(flightlist_geo[1])
  flight_geo <- flight_geo["geometry"]
  for (file in flightlist_geo[-1]) {
    file_geo <- st_read(file)
    file_geo <- file_geo["geometry"]
    flight_geo <- rbind(flight_geo,file_geo)
  }
  # final data frame
  flight_data <- data.frame(flight_info, flight_geo)
  # assign to global env
  assign(x = name, value = flight_data, pos = .GlobalEnv)
}
