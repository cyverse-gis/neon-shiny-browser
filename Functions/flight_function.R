flight_data <- function(flightlist_info, flightlist_geo, year, name) {
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
                       "Site" = as.character(FieldSite_table$Site[FieldSite_table$Abb %in% site_part]),
                       "SiteType" = toupper(CR_table[grep(RC_part_type,CR_table$Abb),2]),
                       "SiteType_number" = RC_part_num,
                       "Priority" = priority_part,
                       "Version" = version_part,
                       "Year" = as.character(year))
    flight_info <- rbind(flight_info, file_info)
  }
  flight_info$DomainID <- as.numeric(as.character(flight_info$DomainID))
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
