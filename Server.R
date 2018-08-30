# Shiny server
function(input, output, session) {
  
  # Initialization
  if (dir_created == TRUE) {
    showNotification(ui = "'NEON Downloads' folder created in the directory containing this app. All downloads will go to this folder.", duration = 20, type = "message")
  } else {
    showNotification(ui = "Welcome back!", duration = 10, type = "message")
  }
  
  ####INTERACTIVE MAP TAB####
  
  # Reactive value for layer control
  legend <- reactiveValues(group = c("Field Sites", "Domains", "Flightpaths", "TOS", "Sub Locations"))
  
  #### Map ####
  output$map <- renderLeaflet({
    
    map <- (
      leaflet() %>%
        addProviderTiles(provider = providers$OpenStreetMap.Mapnik,
                         group = "Basic") %>%
        addProviderTiles(provider = providers$Esri.NatGeoWorldMap,
                         group = "Nat geo") %>%
        addProviderTiles(provider = providers$OpenTopoMap,
                         group = "Topo") %>%
        addProviderTiles(provider = providers$Esri.WorldImagery,
                         group = "Satellite") %>%
        # Add measuring tool
        addMeasure(position = "topleft",
                   primaryLengthUnit = "kilometers",
                   primaryAreaUnit = "sqmeters",
                   activeColor = "#3D535D",
                   completedColor = "#7D4479"
        ) %>%
        # Add layer control
        addLayersControl(baseGroups = c("Basic", "Satellite", "Nat geo", "Topo"),
                         overlayGroups = legend$group,
                         options = layersControlOptions(collapsed = FALSE)
        ) %>%
        # Add option for fullscreen
        leaflet.extras::addFullscreenControl(pseudoFullscreen = TRUE)
    )
    map %>% setView(lng = -98.5795, lat = 39.8283, zoom = 2.5)
  })
  #### — Filter Map Features ####
  #### —— Filtered Features ####
  Domain_IDs <- reactive(domains$DomainID[domains$Domain %in% input$fieldsite_domain])
  Field_sites_point_filtered <- reactive(FieldSite_point %>% filter(siteType %in% input$fieldsite_type) %>%
                                           filter(domainCode %in% Domain_IDs()) %>%
                                           filter(Habitat %in% input$fieldsite_habitat))
  Field_sites_poly_filtered <- reactive(FieldSite_poly %>% filter(code %in% Field_sites_point_filtered()$siteCode))
  Domain_included <- reactive(domain_data %>% filter(DomainName %in% input$fieldsite_domain))
  Domain_unincluded <- reactive(domain_data %>% filter(!(DomainName %in% input$fieldsite_domain)))
  TOS_data_filtered <- reactive(TOS_data %>% filter(siteID %in% Field_sites_point_filtered()$siteCode))
  Flight_data_filtered <- reactive(flight_data %>% filter(SiteAbb %in% Field_sites_point_filtered()$siteCode) %>%
                                     filter(Year %in% input$flightpath_year))
 
  Subloc_tes_plots_base <- reactive(FieldSite_plots_tes %>% filter(Type %in% "Distributed Base Plot") %>%
                                      filter(Site %in% input$fieldsite_sublocs))
  Subloc_tes_plots_bird <- reactive(FieldSite_plots_tes %>% filter(Type %in% "Distributed Bird Grid") %>%
                                      filter(Site %in% input$fieldsite_sublocs))
  Subloc_tes_plots_mam <- reactive(FieldSite_plots_tes %>% filter(Type %in% "Distributed Mammal Grid") %>%
                                     filter(Site %in% input$fieldsite_sublocs))
  Subloc_tes_plots_mos <- reactive(FieldSite_plots_tes %>% filter(Type %in% "Distributed Mosquito Plot") %>%
                                     filter(Site %in% input$fieldsite_sublocs))
  Subloc_tes_plots_tick <- reactive(FieldSite_plots_tes %>% filter(Type %in% "Distributed Tick Plot") %>%
                                          filter(Site %in% input$fieldsite_sublocs))
  Subloc_tes_plots_phe <- reactive(FieldSite_plots_tes %>% filter(Type %in% "Tower Phenology Plot") %>%
                                     filter(Site %in% input$fieldsite_sublocs))
  
  Subloc_aqu_plots_well <- reactive(FieldSite_locations_aqu %>% filter(`General Type` %in% "Groundwater Well") %>%
                                      filter(Site %in% input$fieldsite_sublocs))
  Subloc_aqu_plots_metstn <- reactive(FieldSite_locations_aqu %>% filter(`General Type` %in% "Met. Station") %>%
                                      filter(Site %in% input$fieldsite_sublocs))
  Subloc_aqu_plots_sensor <- reactive(FieldSite_locations_aqu %>% filter(`General Type` %in% "Sensor Station") %>%
                                        filter(Site %in% input$fieldsite_sublocs))
  Subloc_aqu_plots_gauge <- reactive(FieldSite_locations_aqu %>% filter(`General Type` %in% "Staff gauge/camera") %>%
                                        filter(Site %in% input$fieldsite_sublocs))
  Subloc_aqu_plots_reach <- reactive(FieldSite_locations_aqu %>% filter(`General Type` %in% "Sampling Reach Boundary") %>%
                                        filter(Site %in% input$fieldsite_sublocs))
  Subloc_aqu_plots_riparian <- reactive(FieldSite_locations_aqu %>% filter(`General Type` %in% "Riparian Assessment") %>%
                                       filter(Site %in% input$fieldsite_sublocs))
  
  #### —— Plot Domains #### 
  observe({
    proxy <- leafletProxy("map")
    proxy %>%
      clearGroup(group = "Domains") %>%
      addPolygons(data = Domain_unincluded(),
                  weight = 2,
                  fillOpacity = '0.18',
                  group = "Domains",
                  popup = paste0(Domain_unincluded()$DomainName),
                  color = "gray") %>%
      addPolygons(data = Domain_included(),
                  weight = 2,
                  fillOpacity = '0.18',
                  group = "Domains",
                  popup = paste0(Domain_included()$DomainName),
                  color = "blue")
  })
  
  #### —— Plot Flightpaths ####
  observe({
    proxy <- leafletProxy("map")
    # pal <- colorFactor(palette = c("#FFFFFF", "#0000FF"), domain = Flight_data_filtered()$Year)
    if (nrow(Flight_data_filtered()) == 0) {
      proxy %>% clearGroup(group = "Flightpaths")
    } else {
      proxy %>% clearGroup(group = "Flightpaths") %>%
        # Areas for NEON flight paths (red)
        addPolygons(data = Flight_data_filtered()$geometry,
                    color = "Red",
                    group = "Flightpaths",
                    popup = paste0("<b>Year: </b>",
                                   Flight_data_filtered()$Year,
                                   "<br><b>Site: </b><br>",
                                   Flight_data_filtered()$Site,
                                   "<br><b>Domain: </b>",
                                   domains[Flight_data_filtered()$DomainID,2],
                                   "<br><b>Core/Relocatable: </b>",
                                   Flight_data_filtered()$SiteType,
                                   "<br><b>Flight Priority: </b>",
                                   Flight_data_filtered()$Priority,
                                   "<br><b>Version: </b>",
                                   Flight_data_filtered()$Version),
                    opacity = 0.3, 
                    fillOpacity = 0.06
        )
    }
  })
  #### —— Plot TOS ####
  observe({
    proxy <- leafletProxy("map")
    if (nrow(TOS_data_filtered()) == 0) {
      proxy %>% clearGroup(group = "TOS")
    } else {
      proxy %>% clearGroup(group = "TOS") %>%
        addMarkers(data = TOS_data_filtered(),
                   lng = TOS_data_filtered()$longitd,
                   lat = TOS_data_filtered()$latitud,
                   popup = paste0("<b>Plot ID: </b>",
                                  TOS_data_filtered()$plotID,
                                  "<br><b>Dimensions: </b>",
                                  TOS_data_filtered()$plotDim,
                                  "<br><b>Plot Type: </b>",
                                  TOS_data_filtered()$plotTyp, "/",
                                  TOS_data_filtered()$subtype),
                   group = "TOS",
                   clusterOptions = markerClusterOptions()
        ) %>%
        addPolygons(data = TOS_data_filtered(),
                    popup = paste0("Area of ", TOS_data_filtered()$plotID),
                    group = "TOS",
                    color = "gray")
    }
  })
  #### —— Plot Fieldsites ####
  # Markers
  observe({
    proxy <- leafletProxy("map")
    if (nrow(Field_sites_point_filtered()) == 0) {
      proxy %>% clearGroup(group = "Field Sites")
    } else {
      proxy %>% clearGroup(group = "Field Sites") %>%
        addMarkers(data = Field_sites_point_filtered(),
                   lng = Field_sites_point_filtered()$siteLongitude,
                   lat = Field_sites_point_filtered()$siteLatitude,
                   group = "Field Sites",
                   popup = paste0("<b>Site Name: </b>",
                                  Field_sites_point_filtered()$siteDescription, " (",
                                  Field_sites_point_filtered()$siteCode, ")",
                                  "<br><b>Region: </b>",
                                  Field_sites_point_filtered()$domainName,
                                  "<br><b>State: </b>",
                                  Field_sites_point_filtered()$stateName,
                                  "<br><b>Site Type: </b>",
                                  Field_sites_point_filtered()$siteType,
                                  "<br><b>Habitat: </b>",
                                  Field_sites_point_filtered()$`Habitat Specific`,
                                  "<br><b>Host: </b>",
                                  Field_sites_point_filtered()$Host),
                   clusterOptions = markerClusterOptions(),
                   label = paste0(Field_sites_point_filtered()$siteDescription),
                   icon = NEON_icon
        )
    }
  })
  # Boundaries
  observe({
    proxy <- leafletProxy("map")
    proxy %>% removeShape(layerId = unique(FieldSite_poly$code))
    if (nrow(Field_sites_poly_filtered()) == 0) {
      proxy %>% clearGroup(group = "Field Sites")
    } else {
      for (i in 1:length(Field_sites_poly_filtered()$coordinates)) {
        if (is.array(Field_sites_poly_filtered()$coordinates[[i]])) {
          proxy %>%
            addPolygons(lng = Field_sites_poly_filtered()$coordinates[[i]][1,,1],
                        lat = Field_sites_poly_filtered()$coordinates[[i]][1,,2],
                        group = "Field Sites",
                        color = "#49E2BD",
                        layerId = Field_sites_poly_filtered()$code[i],
                        popup = paste0("Boundaries for ",
                                       Field_sites_poly_filtered()$name[i]),
                        opacity = 1,
                        fillOpacity = 0,
                        highlightOptions = highlightOptions(stroke = TRUE, color = "#39ff14", weight = 7)
            )
        } else if (is.list(Field_sites_poly_filtered()$coordinates[[i]])) {
          proxy %>%
            addPolylines(lng = Field_sites_poly_filtered()$coordinates[[i]][[1]][,1],
                         lat = Field_sites_poly_filtered()$coordinates[[i]][[1]][,2],
                         group = "Field Sites",
                         color = "#49E2BD",
                         layerId = Field_sites_poly_filtered()$code[i],
                         popup = paste0("Boundaries for ",
                                        Field_sites_poly_filtered()$name[i]),
                         opacity = 1,
                         fillOpacity = 0.4,
                         highlightOptions = highlightOptions(stroke = TRUE, color = "#39ff14", weight = 7)
            )
        }
      }
    }
  })
  #### —— Plot Sub Locations ####
  #### ——— Terrestrial ####
  # Base
  observe({
    proxy <- leafletProxy('map')
    if (input$sublocs_baseplot & nrow(Subloc_tes_plots_base()) != 0) {
      for (i in 1:nrow(Subloc_tes_plots_base())) {
        proxy %>%
          addMarkers(data = Subloc_tes_plots_base()[i,],
                     icon = ~NEON_locations_tes[Subloc_tes_plots_base()$Type[i]],
                     popup = paste0("<b>Plot: </b>",
                                    Subloc_tes_plots_base()$Description[i],
                                    "<br><b>Type: </b>",
                                    Subloc_tes_plots_base()$Type[i],
                                    "<br><b>Dimensions: </b>",
                                    Subloc_tes_plots_base()$Plot.Size[i]),
                     group = "Sub Locations",
                     layerId = Subloc_tes_plots_base()$Name[i]) %>%
          addRectangles(lng1 = geosphere::destPoint(p = c(Subloc_tes_plots_base()$Longitude[i], Subloc_tes_plots_base()$Latitude[i]), b = 225, d = 20*sqrt(2))[1],
                        lat1 = geosphere::destPoint(p = c(Subloc_tes_plots_base()$Longitude[i], Subloc_tes_plots_base()$Latitude[i]), b = 225, d = 20*sqrt(2))[2],
                        lng2 = geosphere::destPoint(p = c(Subloc_tes_plots_base()$Longitude[i], Subloc_tes_plots_base()$Latitude[i]), b = 45, d = 20*sqrt(2))[1],
                        lat2 = geosphere::destPoint(p = c(Subloc_tes_plots_base()$Longitude[i], Subloc_tes_plots_base()$Latitude[i]), b = 45, d = 20*sqrt(2))[2],
                        color = "#49E2BD",
                        fillOpacity = 0,
                        weight = 3,
                        layerId = Subloc_tes_plots_base()$Name[i],
                        popup = paste0("Boundaries for ",
                                       Subloc_tes_plots_base()$Description[i])
          )
      }
    }
    else if (!input$sublocs_baseplot | (nrow(Subloc_tes_plots_base()) == 0)) {
      proxy %>% removeMarker(layerId = (FieldSite_plots_tes %>% filter(Type %in% "Distributed Base Plot"))$Name) %>% 
        removeShape(layerId = (FieldSite_plots_tes %>% filter(Type %in% "Distributed Base Plot"))$Name)
    }
  })
  # Bird
  observe({
    proxy <- leafletProxy('map')
    if (input$sublocs_birdgrid & nrow(Subloc_tes_plots_bird()) != 0) {
      for (i in 1:nrow(Subloc_tes_plots_bird())) {
        proxy %>%
          addRectangles(lng1 = geosphere::destPoint(p = c(Subloc_tes_plots_bird()$Longitude[i], Subloc_tes_plots_bird()$Latitude[i]), b = 225, d = 250*sqrt(2))[1],
                        lat1 = geosphere::destPoint(p = c(Subloc_tes_plots_bird()$Longitude[i], Subloc_tes_plots_bird()$Latitude[i]), b = 225, d = 250*sqrt(2))[2],
                        lng2 = geosphere::destPoint(p = c(Subloc_tes_plots_bird()$Longitude[i], Subloc_tes_plots_bird()$Latitude[i]), b = 45, d = 250*sqrt(2))[1],
                        lat2 = geosphere::destPoint(p = c(Subloc_tes_plots_bird()$Longitude[i], Subloc_tes_plots_bird()$Latitude[i]), b = 45, d = 250*sqrt(2))[2],
                        group = "Sub Locations",
                        color = "#155AA8",
                        opacity = 1,
                        fillOpacity = 0,
                        weight = 3,
                        layerId = Subloc_tes_plots_bird()$Name[i],
                        popup = paste0("<b>Plot: </b>",
                                       Subloc_tes_plots_bird()$Description[i],
                                       "<br><b>Type: </b>",
                                       Subloc_tes_plots_bird()$Type[i],
                                       "<br><b>Dimensions: </b>",
                                       Subloc_tes_plots_bird()$Plot.Size[i])
          )
      }
    }
    else if (!input$sublocs_birdgrid | (nrow(Subloc_tes_plots_bird()) == 0)) {
      proxy %>% removeShape(layerId = (FieldSite_plots_tes %>% filter(Type %in% "Distributed Bird Grid"))$Name)
    }
  })
  # Mammal
  observe({
    proxy <- leafletProxy('map')
    if (input$sublocs_mammalgrid & nrow(Subloc_tes_plots_mam()) != 0) {
      for (i in 1:nrow(Subloc_tes_plots_mam())) {
        proxy %>%
          addMarkers(data = Subloc_tes_plots_mam()[i,],
                     icon = ~NEON_locations_tes[Subloc_tes_plots_mam()$Type[i]],
                     popup = paste0("<b>Plot: </b>",
                                    Subloc_tes_plots_mam()$Description[i],
                                    "<br><b>Type: </b>",
                                    Subloc_tes_plots_mam()$Type[i],
                                    "<br><b>Dimensions: </b>",
                                    Subloc_tes_plots_mam()$Plot.Size[i]),
                     group = "Sub Locations",
                     layerId = Subloc_tes_plots_mam()$Name[i]) %>%
          addRectangles(lng1 = geosphere::destPoint(p = c(Subloc_tes_plots_mam()$Longitude[i], Subloc_tes_plots_mam()$Latitude[i]), b = 225, d = 45*sqrt(2))[1],
                        lat1 = geosphere::destPoint(p = c(Subloc_tes_plots_mam()$Longitude[i], Subloc_tes_plots_mam()$Latitude[i]), b = 225, d = 45*sqrt(2))[2],
                        lng2 = geosphere::destPoint(p = c(Subloc_tes_plots_mam()$Longitude[i], Subloc_tes_plots_mam()$Latitude[i]), b = 45, d = 45*sqrt(2))[1],
                        lat2 = geosphere::destPoint(p = c(Subloc_tes_plots_mam()$Longitude[i], Subloc_tes_plots_mam()$Latitude[i]), b = 45, d = 45*sqrt(2))[2],
                        color = "#49E2BD",
                        fillOpacity = 0,
                        weight = 3,
                        layerId = Subloc_tes_plots_mam()$Name[i],
                        popup = paste0("Boundaries for ",
                                       Subloc_tes_plots_mam()$Description[i])
          )
      }
    }
    else if (!input$sublocs_mammalgrid | (nrow(Subloc_tes_plots_mam()) == 0)) {
      proxy %>% removeMarker(layerId = (FieldSite_plots_tes %>% filter(Type %in% "Distributed Mammal Grid"))$Name) %>%
        removeShape(layerId = (FieldSite_plots_tes %>% filter(Type %in% "Distributed Mammal Grid"))$Name)
    }
  })
  # Mosquito
  observe({
    proxy <- leafletProxy('map')
    if (input$sublocs_mosquitoplot & nrow(Subloc_tes_plots_mos()) != 0) {
      for (i in 1:nrow(Subloc_tes_plots_mos())) {
        proxy %>%
          addMarkers(data = Subloc_tes_plots_mos()[i,],
                     icon = ~NEON_locations_tes[Subloc_tes_plots_mos()$Type[i]],
                     popup = paste0("<b>Plot: </b>",
                                    Subloc_tes_plots_mos()$Description[i],
                                    "<br><b>Type: </b>",
                                    Subloc_tes_plots_mos()$Type[i],
                                    "<br><b>Dimensions: </b>",
                                    Subloc_tes_plots_mos()$Plot.Size[i]),
                     group = "Sub Locations",
                     layerId = Subloc_tes_plots_mos()$Name[i])
      }
    }
    else if (!input$sublocs_mosquitoplot | (nrow(Subloc_tes_plots_mos()) == 0)) {
      proxy %>% removeMarker(layerId = (FieldSite_plots_tes %>% filter(Type %in% "Distributed Mosquito Plot"))$Name)
    }
  })
  # Tick
  observe({
    proxy <- leafletProxy('map')
    if (input$sublocs_tickplot & nrow(Subloc_tes_plots_tick()) != 0) {
      for (i in 1:nrow(Subloc_tes_plots_tick())) {
        proxy %>%
          addMarkers(data = Subloc_tes_plots_tick()[i,],
                     icon = ~NEON_locations_tes[Subloc_tes_plots_tick()$Type[i]],
                     popup = paste0("<b>Plot: </b>",
                                    Subloc_tes_plots_tick()$Description[i],
                                    "<br><b>Type: </b>",
                                    Subloc_tes_plots_tick()$Type[i],
                                    "<br><b>Dimensions: </b>",
                                    Subloc_tes_plots_tick()$Plot.Size[i]),
                     group = "Sub Locations",
                     layerId = Subloc_tes_plots_tick()$Name[i]) %>%
          addRectangles(lng1 = geosphere::destPoint(p = c(Subloc_tes_plots_tick()$Longitude[i], Subloc_tes_plots_tick()$Latitude[i]), b = 225, d = 20*sqrt(2))[1],
                        lat1 = geosphere::destPoint(p = c(Subloc_tes_plots_tick()$Longitude[i], Subloc_tes_plots_tick()$Latitude[i]), b = 225, d = 20*sqrt(2))[2],
                        lng2 = geosphere::destPoint(p = c(Subloc_tes_plots_tick()$Longitude[i], Subloc_tes_plots_tick()$Latitude[i]), b = 45, d = 20*sqrt(2))[1],
                        lat2 = geosphere::destPoint(p = c(Subloc_tes_plots_tick()$Longitude[i], Subloc_tes_plots_tick()$Latitude[i]), b = 45, d = 20*sqrt(2))[2],
                        color = "#49E2BD",
                        fillOpacity = 0,
                        weight = 3,
                        layerId = Subloc_tes_plots_tick()$Name[i],
                        popup = paste0("Boundaries for ",
                                       Subloc_tes_plots_tick()$Description[i])
          )
      }
    }
    else if (!input$sublocs_tickplot | (nrow(Subloc_tes_plots_tick()) == 0)) {
      proxy %>% removeMarker(layerId = (FieldSite_plots_tes %>% filter(Type %in% "Distributed Tick Plot"))$Name) %>%
        removeShape(layerId = (FieldSite_plots_tes %>% filter(Type %in% "Distributed Tick Plot"))$Name)
    }
  })
  # Phenology
  observe({
    proxy <- leafletProxy('map')
    if (input$sublocs_phenologyplot & nrow(Subloc_tes_plots_phe()) != 0) {
      for (i in 1:nrow(Subloc_tes_plots_phe())) {
        proxy %>%
          addMarkers(data = Subloc_tes_plots_phe()[i,],
                     icon = ~NEON_locations_tes[Subloc_tes_plots_phe()$Type[i]],
                     popup = paste0("<b>Plot: </b>",
                                    Subloc_tes_plots_phe()$Description[i],
                                    "<br><b>Type: </b>",
                                    Subloc_tes_plots_phe()$Type[i],
                                    "<br><b>Dimensions: </b>",
                                    Subloc_tes_plots_phe()$Plot.Size[i]),
                     group = "Sub Locations",
                     layerId = Subloc_tes_plots_phe()$Name[i]) %>%
          addRectangles(lng1 = geosphere::destPoint(p = c(Subloc_tes_plots_phe()$Longitude[i], Subloc_tes_plots_phe()$Latitude[i]), b = 225, d = 100*sqrt(2))[1],
                        lat1 = geosphere::destPoint(p = c(Subloc_tes_plots_phe()$Longitude[i], Subloc_tes_plots_phe()$Latitude[i]), b = 225, d = 100*sqrt(2))[2],
                        lng2 = geosphere::destPoint(p = c(Subloc_tes_plots_phe()$Longitude[i], Subloc_tes_plots_phe()$Latitude[i]), b = 45, d = 100*sqrt(2))[1],
                        lat2 = geosphere::destPoint(p = c(Subloc_tes_plots_phe()$Longitude[i], Subloc_tes_plots_phe()$Latitude[i]), b = 45, d = 100*sqrt(2))[2],
                        color = "#49E2BD",
                        fillOpacity = 0,
                        weight = 3,
                        layerId = Subloc_tes_plots_phe()$Name[i],
                        popup = paste0("Boundaries for ",
                                       Subloc_tes_plots_phe()$Description[i])
          )
      }
    }
    else if (!input$sublocs_tickplot | (nrow(Subloc_tes_plots_phe()) == 0)) {
      proxy %>% removeMarker(layerId = (FieldSite_plots_tes %>% filter(Type %in% "Tower Phenology Plot"))$Name) %>%
        removeShape(layerId = (FieldSite_plots_tes %>% filter(Type %in% "Tower Phenology Plot"))$Name)
    }
  })
  #### ——— Aquatic ####
  # Groundwater Well
  observe({
    proxy <- leafletProxy('map')
    if (input$sublocs_well & nrow(Subloc_aqu_plots_well()) != 0) {
      for (i in 1:nrow(Subloc_aqu_plots_well())) {
        proxy %>%
          addMarkers(data = Subloc_aqu_plots_well()[i,],
                     icon = ~NEON_locations_aqu[Subloc_aqu_plots_well()$`General Type`[i]],
                     popup = paste0("<b>Station/Location: </b>",
                                    Subloc_aqu_plots_well()$Description[i],
                                    "<br><b>Type: </b>",
                                    Subloc_aqu_plots_well()$`General Type`[i]),
                     group = "Sub Locations",
                     layerId = Subloc_aqu_plots_well()$Name[i])
      }
    } else if (!input$sublocs_well | (nrow(Subloc_aqu_plots_well()) == 0)) {
      proxy %>% removeMarker(layerId = (FieldSite_locations_aqu %>% filter(`General Type` %in% "Groundwater Well"))$Name)
    }
  })
  # Met. Station
  observe({
    proxy <- leafletProxy('map')
    if (input$sublocs_metstn & nrow(Subloc_aqu_plots_metstn()) != 0) {
      for (i in 1:nrow(Subloc_aqu_plots_metstn())) {
        proxy %>%
          addMarkers(data = Subloc_aqu_plots_metstn()[i,],
                     icon = ~NEON_locations_aqu[Subloc_aqu_plots_metstn()$`General Type`[i]],
                     popup = paste0("<b>Station/Location: </b>",
                                    Subloc_aqu_plots_metstn()$Description[i],
                                    "<br><b>Type: </b>",
                                    Subloc_aqu_plots_metstn()$`General Type`[i]),
                     group = "Sub Locations",
                     layerId = Subloc_aqu_plots_metstn()$Name[i])
      }
    } else if (!input$sublocs_well | (nrow(Subloc_aqu_plots_well()) == 0)) {
      proxy %>% removeMarker(layerId = (FieldSite_locations_aqu %>% filter(`General Type` %in% "Met. Station"))$Name)
    }
  })
  # Sensor Station
  observe({
    proxy <- leafletProxy('map')
    if (input$sublocs_sensor & nrow(Subloc_aqu_plots_sensor()) != 0) {
      for (i in 1:nrow(Subloc_aqu_plots_sensor())) {
        proxy %>%
          addMarkers(data = Subloc_aqu_plots_sensor()[i,],
                     icon = ~NEON_locations_aqu[Subloc_aqu_plots_sensor()$`General Type`[i]],
                     popup = paste0("<b>Station/Location: </b>",
                                    Subloc_aqu_plots_sensor()$Description[i],
                                    "<br><b>Type: </b>",
                                    Subloc_aqu_plots_sensor()$`General Type`[i]),
                     group = "Sub Locations",
                     layerId = Subloc_aqu_plots_sensor()$Name[i])
      }
    } else if (!input$sublocs_sensor | (nrow(Subloc_aqu_plots_sensor()) == 0)) {
      proxy %>% removeMarker(layerId = (FieldSite_locations_aqu %>% filter(`General Type` %in% "Sensor Station"))$Name)
    }
  })
  # Staff gauge/camera
  observe({
    proxy <- leafletProxy('map')
    if (input$sublocs_gauge & nrow(Subloc_aqu_plots_gauge()) != 0) {
      for (i in 1:nrow(Subloc_aqu_plots_gauge())) {
        proxy %>%
          addMarkers(data = Subloc_aqu_plots_gauge()[i,],
                     icon = ~NEON_locations_aqu[Subloc_aqu_plots_gauge()$`General Type`[i]],
                     popup = paste0("<b>Station/Location: </b>",
                                    Subloc_aqu_plots_gauge()$Description[i],
                                    "<br><b>Type: </b>",
                                    Subloc_aqu_plots_gauge()$`General Type`[i]),
                     group = "Sub Locations",
                     layerId = Subloc_aqu_plots_gauge()$Name[i])
      }
    } else if (!input$sublocs_sensor | (nrow(Subloc_aqu_plots_gauge()) == 0)) {
      proxy %>% removeMarker(layerId = (FieldSite_locations_aqu %>% filter(`General Type` %in% "Staff gauge/camera"))$Name)
    }
  })
  # Sampling Reach Boundary
  observe({
    proxy <- leafletProxy('map')
    if (input$sublocs_reach & nrow(Subloc_aqu_plots_reach()) != 0) {
      for (i in 1:nrow(Subloc_aqu_plots_reach())) {
        proxy %>%
          addMarkers(data = Subloc_aqu_plots_reach()[i,],
                     icon = ~NEON_locations_aqu[Subloc_aqu_plots_reach()$`General Type`[i]],
                     popup = paste0("<b>Station/Location: </b>",
                                    Subloc_aqu_plots_reach()$Description[i],
                                    "<br><b>Type: </b>",
                                    Subloc_aqu_plots_reach()$`General Type`[i]),
                     group = "Sub Locations",
                     layerId = Subloc_aqu_plots_reach()$Name[i])
      }
    } else if (!input$sublocs_sensor | (nrow(Subloc_aqu_plots_sensor()) == 0)) {
      proxy %>% removeMarker(layerId = (FieldSite_locations_aqu %>% filter(`General Type` %in% "Sampling Reach Boundary"))$Name)
    }
  })
  # Riparian Assessment
  observe({
    proxy <- leafletProxy('map')
    if (input$sublocs_riparian & nrow(Subloc_aqu_plots_riparian()) != 0) {
      for (i in 1:nrow(Subloc_aqu_plots_riparian())) {
        proxy %>%
          addMarkers(data = Subloc_aqu_plots_riparian()[i,],
                     icon = ~NEON_locations_aqu[Subloc_aqu_plots_riparian()$`General Type`[i]],
                     popup = paste0("<b>Station/Location: </b>",
                                    Subloc_aqu_plots_riparian()$Description[i],
                                    "<br><b>Type: </b>",
                                    Subloc_aqu_plots_riparian()$`General Type`[i]),
                     group = "Sub Locations",
                     layerId = Subloc_aqu_plots_riparian()$Name[i])
      }
    } else if (!input$sublocs_sensor | (nrow(Subloc_aqu_plots_sensor()) == 0)) {
      proxy %>% removeMarker(layerId = (FieldSite_locations_aqu %>% filter(`General Type` %in% "Riparian Assessment"))$Name)
    }
  })
  
  # Key
  observe(if (input$sublocs_tes_selectall) {
    updateCheckboxInput(session, inputId = "sublocs_baseplot", value = TRUE)
    updateCheckboxInput(session, inputId = "sublocs_birdgrid", value = TRUE)
    updateCheckboxInput(session, inputId = "sublocs_mammalgrid", value = TRUE)
    updateCheckboxInput(session, inputId = "sublocs_mosquitoplot", value = TRUE)
    updateCheckboxInput(session, inputId = "sublocs_tickplot", value = TRUE)
    updateCheckboxInput(session, inputId = "sublocs_phenologyplot", value = TRUE)
  })
  observe(if (!input$sublocs_tes_selectall) {
    updateCheckboxInput(session, inputId = "sublocs_baseplot", value = FALSE)
    updateCheckboxInput(session, inputId = "sublocs_birdgrid", value = FALSE)
    updateCheckboxInput(session, inputId = "sublocs_mammalgrid", value = FALSE)
    updateCheckboxInput(session, inputId = "sublocs_mosquitoplot", value = FALSE)
    updateCheckboxInput(session, inputId = "sublocs_tickplot", value = FALSE)
    updateCheckboxInput(session, inputId = "sublocs_phenologyplot", value = FALSE)
  })
  
  observe(if (input$sublocs_aqu_selectall) {
    updateCheckboxInput(session, inputId = "sublocs_well", value = TRUE)
    updateCheckboxInput(session, inputId = "sublocs_metstn", value = TRUE)
    updateCheckboxInput(session, inputId = "sublocs_sensor", value = TRUE)
    updateCheckboxInput(session, inputId = "sublocs_gauge", value = TRUE)
    updateCheckboxInput(session, inputId = "sublocs_reach", value = TRUE)
    updateCheckboxInput(session, inputId = "sublocs_riparian", value = TRUE)
  })
  observe(if (!input$sublocs_aqu_selectall) {
    updateCheckboxInput(session, inputId = "sublocs_well", value = FALSE)
    updateCheckboxInput(session, inputId = "sublocs_metstn", value = FALSE)
    updateCheckboxInput(session, inputId = "sublocs_sensor", value = FALSE)
    updateCheckboxInput(session, inputId = "sublocs_gauge", value = FALSE)
    updateCheckboxInput(session, inputId = "sublocs_reach", value = FALSE)
    updateCheckboxInput(session, inputId = "sublocs_riparian", value = FALSE)
  })
  # Hide TOS and Flightpaths when launching app (TOS can make computer slow)
  leafletProxy("map") %>% hideGroup("TOS") %>% hideGroup("Flightpaths")
  
  #### NEON ####
  observeEvent(eventExpr = input$zoomtosite,
               handlerExpr = {
                 if (input$NEONsite_zoom %in% FieldSite_Tes) {
                   leafletProxy("map") %>% flyTo(lng = FieldSite_point$siteLongitude[FieldSite_point$siteCode %in% input$NEONsite_zoom],
                                                 lat = FieldSite_point$siteLatitude[FieldSite_point$siteCode %in% input$NEONsite_zoom],
                                                 zoom = 12)
                 } else if (input$NEONsite_zoom %in% FieldSite_Aqu) {
                   leafletProxy("map") %>% flyTo(lng = FieldSite_point$siteLongitude[FieldSite_point$siteCode %in% input$NEONsite_zoom],
                                                 lat = FieldSite_point$siteLatitude[FieldSite_point$siteCode %in% input$NEONsite_zoom],
                                                 zoom = 15.5)
                 }
               }
  )
  observeEvent(eventExpr = input$addsublocs,
               handlerExpr = {
                 leafletProxy('map') %>% showGroup(group = "Sub Locations")
                 updateTabsetPanel(session, inputId = "main", selected = "filter")
                 choices <- input$fieldsite_sublocs
                 updateSelectInput(session, inputId = "fieldsite_sublocs", selected = c(choices, input$NEONsite_zoom))
               })
  ####— NEON: Step 1- Find data ####
  ####—— 1a: By Site####
  # Variables
  NEONproducts_product <<- nneo_products() # Added this variable up here because one item in finding by "site" needed it
  NEONproducts_site <- reactive(NEONproducts_product[filter_site(site = input$NEONsite_site),])
  # list: getting data frame of availability based on site code
  # Filter by keywords, type
  keyword_lists(list = FieldSite_abbs)
  output$ui_selectkeywords_site <- renderUI({
    selectInput(inputId = "NEONproductkeywords_site", label = "Keywords", choices = get(x = input$NEONsite_site, envir = .NEON_keywords) ,multiple = TRUE)
  })
  NEONproducts_site_filter <- reactive(as.data.frame(cbind('Product Name' = NEONproducts_site()$productName, 'Product ID' = NEONproducts_site()$productCode, "keywords" = NEONproducts_site()$keywords, "producttype" = NEONproducts_site()$productScienceTeam))[order(NEONproducts_site()$productName),])
  keyword_filters_site <- reactive(filter_keyword(column = NEONproducts_site_filter()$keywords, keywords = input$NEONproductkeywords_site))
  NEONproductlist_site_filtered_keyword <- reactive(NEONproducts_site_filter()[keyword_filters_site(),])
  datatype_filters_site <- reactive({
    if (is.null(input$selectproducttype_site)) {
      NEON_datatypes
    } else {
      input$selectproducttype_site
    }
  })
  NEONproductlist_site <- reactive(NEONproductlist_site_filtered_keyword()[(NEONproductlist_site_filtered_keyword()$producttype %in% datatype_filters_site()),])
  # for dropdown
  output$dropdown_site <- renderPrint(paste0(FieldSite_point$siteName[FieldSite_point$siteCode %in% input$NEONsite_zoom], " ", FieldSite_point$`Habitat Specific`[FieldSite_point$siteCode %in% input$NEONsite_zoom]))
  output$dataproduct_number <- renderPrint(nrow(NEONproducts_product[filter_site(site = input$NEONsite_zoom),]))
  # single: filtering column of products for one site through ID
  NEONproductID_site <- reactive(req(
    if (gsub(pattern = " ", replacement = "", x = input$NEONproductID_site) == "") {
      "random string that will not match to anything"
    } else {
      gsub(pattern = " ", replacement = "", x = input$NEONproductID_site)
    }
  ))
  NEONproductinfo_site <- reactive(req(filter(.data = NEONproducts_site(), productCode == NEONproductID_site())))
  # Display products: list
  output$NEONproductoptions_site <- renderDataTable(NEONproductlist_site()[1:2], options = list(lengthMenu = c(10,25),
                                                                                                pageLength = 10))
  # Display products: single
  output$NEONproductsite_site <- renderPrint(req(input$NEONsite_site))
  output$NEONproductname_site <- renderPrint(req(NEONproductinfo_site()$productName))
  output$NEONproductdesc_site <- renderPrint(req(ifelse(is.null(req(NEONproductinfo_site()$productDescription)),
                                                        yes = NULL,
                                                        no = NEONproductinfo_site()$productDescription)))
  output$NEONproductdesign_site <- renderPrint(req(ifelse(is.null(req(NEONproductinfo_site()$productDesignDescription)),
                                                          yes = NULL,
                                                          no = NEONproductinfo_site()$productDesignDescription)))
  output$NEONproductnotes_site <- renderPrint(req(ifelse(is.null(req(NEONproductinfo_site()$productRemarks)),
                                                         yes = NULL,
                                                         no = NEONproductinfo_site()$productRemarks)))
  output$NEONproductdates_site <- renderPrint({
    dates <- if (length(NEONproductinfo_site()$siteCodes) == 0) {
      NA
    } else {
      NEONproductinfo_site()$siteCodes[[1]]$availableMonths[NEONproductinfo_site()$siteCodes[[1]]$siteCode %in% input$NEONsite_site][[1]]}
    req(dates)
  })
  output$NEONproductURL_site <- renderPrint({
    Urls <- if (length(NEONproductinfo_site()$siteCodes) == 0) {
      NA
    } else {
      NEONproductinfo_site()$siteCodes[[1]]$availableDataUrls[NEONproductinfo_site()$siteCodes[[1]]$siteCode %in% input$NEONsite_site][[1]]}
    req(Urls)
  })
  
  ####—— 1b: By Product####
  # Variables
  # list: getting data table with products and IDs
  # Filter by keywords, type, theme
  keywords <- NULL
  for (i in 1:length(NEONproducts_product$keywords)) {
    keywords <- c(keywords, NEONproducts_product$keywords[[i]])
  }
  keywords <- unique(keywords)
  keywords <- sort(keywords)
  output$ui_selectkeywords_product <- renderUI(selectInput(inputId = "NEONproductkeywords_product", label = "Keywords", choices = keywords, multiple = TRUE))
  NEONproduct_products_filter <- NEONproducts_product[c("productName", "productCode", "keywords", "productScienceTeam")]
  names(NEONproduct_products_filter) <- c('Product Name', 'Product ID', 'keywords', "producttype")
  NEONproduct_products_filter <- NEONproduct_products_filter[order(NEONproduct_products_filter$`Product Name`),]
  keyword_filters_product <- reactive(filter_keyword(column = NEONproduct_products_filter$keywords, keywords = input$NEONproductkeywords_product))
  datatype_filters_product <- reactive({
    if (is.null(input$selectproducttype_product)) {
      NEON_datatypes
    } else {
      input$selectproducttype_product
    }
  })
  NEONproductlist_product <- reactive(NEONproduct_products_filter[keyword_filters_product(),] %>% filter(`producttype` %in% datatype_filters_product()))
  # single: filtering one row of parent NEON products table through ID
  NEONproductID_product <- reactive(req(
    ifelse(gsub(pattern = " ", replacement = "", x = input$NEONproductID_product) == "",
           yes = "random string that will not match to anything",
           no = gsub(pattern = " ", replacement = "", x = input$NEONproductID_product))
  ))
  NEONproductinfo_product <- reactive(req(filter(.data = NEONproducts_product, productCode == NEONproductID_product())))
  # Display products: list
  output$NEON_product_options <- renderDataTable(NEONproductlist_product()[1:2], options = list(lengthMenu = c(10,25), pageLength = 10))
  # Display products: single
  output$NEONproductname_product <- renderPrint(req(NEONproductinfo_product()$productName))
  output$NEONproductdesc_product <- renderPrint(req(NEONproductinfo_product()$productDescription))
  output$NEONproductdesign_product <- renderPrint(req(NEONproductinfo_product()$productDesignDescription))
  output$NEONproductnotes_product <- renderPrint(req(NEONproductinfo_product()$productRemarks))
  output$ui_selectsite<- renderUI({
    sites <- if (length(NEONproductinfo_product()$siteCodes) == 0) {
      NA} else {
        sort(NEONproductinfo_product()$siteCodes[[1]]$siteCode)}
    selectInput(inputId = "NEONsite_product", label = "Available sites:", choices = req(sites))
  })
  output$NEONproductdates_product <- renderPrint({
    dates <- if (length(NEONproductinfo_product()$siteCodes) == 0) {
      NA
    } else { 
      NEONproductinfo_product()$siteCodes[[1]]$availableMonths[NEONproductinfo_product()$siteCodes[[1]]$siteCode %in% input$NEONsite_product][[1]]}
    req(dates)
  })
  output$NEONproductURL_product <- renderPrint({
    Urls <- if (length(NEONproductinfo_product()$siteCodes) == 0) {
      NA
    } else {
      NEONproductinfo_product()$siteCodes[[1]]$availableDataUrls[NEONproductinfo_product()$siteCodes[[1]]$siteCode %in% input$NEONsite_product][[1]]}
    req(Urls)
  })
  
  ####— NEON: Step 2- Download Data####
  ####—— Variables####
  Product_ID_general <- reactive(req(gsub(pattern = " ", replacement = "", x = input$dpID_general)))
  Product_ID_middle <- reactive(req(strsplit(Product_ID_general(), "[.]")[[1]][2]))
  Folder_general <- reactive(req(paste0("filesToStack", Product_ID_middle())))
  Product_ID_specific <- reactive(req(gsub(pattern = " ", replacement = "", x = input$dpID_specific)))
  Product_ID_AOP <- reactive(req(gsub(pattern = " ", replacement = "", x = input$dpID_AOP)))
  Field_Site_general <- reactive(req(
    if (input$location_NEON_general == "All (default)") {
      "all"
    } else {
      input$location_NEON_general
    })
  )
  Field_Site_specific <- reactive(req(input$location_NEON_specific))
  Field_Site_AOP <- reactive(req(input$location_NEON_AOP))
  Package_type_general <- reactive(req(input$package_type_general))
  Package_type_specific <- reactive(req(input$package_type_specific))
  Date_specific_long <- reactive(req(as.character(input$date_NEON)))
  Date_specific_parts <- reactive(req(strsplit(Date_specific_long(), "-")[[1]]))
  Date_specific <- reactive(req(paste0(Date_specific_parts()[1], "-", Date_specific_parts()[2])))
  Year_AOP <- reactive(req(strsplit(as.character(input$year_AOP), "-")[[1]][1]))
  Folder_path_general <- reactive(req(paste0("../NEON Downloads/NEON_", Field_Site_general(), "_", Product_ID_middle())))
  Folder_path_specific <- reactive(req(paste0("../NEON Downloads/NEON_", Field_Site_specific(), "_", Date_specific())))
  ####—— Download NEON data: general####
  observeEvent(eventExpr = input$download_NEON_general,
               handlerExpr = {
                 showNotification(ui = "Download in progess…", id = "download_general", type = "message")
                 download <- try(zipsByProduct(dpID = Product_ID_general(), site = Field_Site_general(), package = Package_type_general(), check.size = FALSE, savepath = '../NEON Downloads/'), silent = TRUE)
                 if (class(download) == "try-error") {
                   removeNotification(id = "download_general")
                   sendSweetAlert(session, title = "Download failed", text = paste0("This could be due to the data package you tried to obtain or the neonUtlities package used to pull data. Read the error code message: ", strsplit(download, ":")[[1]][-1]), type = 'error')
                 } else {
                   file.rename(from = paste0("../NEON Downloads/", Folder_general()), to = Folder_path_general())
                   removeNotification(id = "download_general")
                   sendSweetAlert(session, title = "File downloaded", text = "Check the 'NEON Downloads' directory. Go to step 2 to unzip files and make them more accesible.", type = 'success')
                 }
               })
  ####—— Download NEON data: specific ####
  observeEvent(eventExpr = input$download_NEON_specific,
               handlerExpr = {
                 showNotification(ui = "Download in progess…", id = "download_specific", type = "message")
                 dir.create(path = Folder_path_specific())
                 download <- try(getPackage(dpID = Product_ID_specific(), site_code = Field_Site_specific(), year_month = Date_specific(), package = Package_type_specific(), savepath = Folder_path_specific()), silent = TRUE)
                 if (class(download) == "try-error") {
                   if (length(dir(path = Folder_path_specific())) == 0) {
                     unlink(x = Folder_path_specific(), recursive = TRUE)
                   }
                   removeNotification(id = "download_specific")
                   sendSweetAlert(session, title = "Download failed", text = paste0("This could be due to the data package you tried to obtain or the neonUtlities package used to pull data. Read the error message: ", download), type = 'error')
                 } else {
                   removeNotification(id = "download_specific")
                   sendSweetAlert(session, title = "File downloaded", text = "Check the 'NEON Downloads' directory. Go to step 2 to unzip files and make them more accesible.", type = 'success')
                 }
               })
  ####—— Download NEON data: AOP####
  product_table <- reactive(NEONproducts_product[NEONproducts_product$productCode == Product_ID_AOP(),])
  # Checking is data product is AOP
  is_AOP <- reactive(if (nrow(product_table()) != 0) {
    if (product_table()$productScienceTeamAbbr == "AOP") {
      "YES"
    } else {
      "NO"
    }
  } else {
    "None"
  })
  output$check_AOP <- renderPrint({
    if (is_AOP() != "None") {
      is_AOP()
    }
  })
  # Calculating Size
  observeEvent(eventExpr = input$get_AOP_size,
               handlerExpr = {
                 if (is_AOP() != "YES") {
                   sendSweetAlert(session, title = "Download failed", text = "Please choose an AOP product", type = 'error')
                 } else {
                   showNotification(ui = "Calculation in progress...", id = "calculation_AOP", type = "message")
                   data_test <- try(nneo_data(product_code = Product_ID_AOP(), site_code = Field_Site_AOP(), year_month = paste0(Year_AOP(), "-01")))
                   if (class(data_test) == "try-error") {
                     removeNotification(id = "calculation_AOP")
                     sendSweetAlert(session, title = "Calculation failed", text = paste0("The product/site/year-month combination that you tried to calculate size for was invalid. Read the error message: ", strsplit(data_test, ":")[[1]][2]), type = 'error')
                   } else {
                     total_size <- 0
                     for (i in c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")) {
                       data <- nneo_data(product_code = Product_ID_AOP(), site_code = Field_Site_AOP(), year_month = paste0(Year_AOP(), "-", i))$data$files
                       size <- as.numeric(data$size)
                       total_size <- total_size + sum(size)
                     }
                     if (total_size < 10^9 & total_size != 0) {
                       size_mb <- total_size * 10^-6
                       total_size <- paste0(as.character(size_mb), " MB")
                     } else if (total_size > 10^9) {
                       size_gb <- total_size * 10^-9
                       total_size <- paste0(as.character(size_gb), " GB")
                     } else if (total_size == 0) {
                       total_size <- "No data available"
                     }
                     removeNotification(id = "calculation_AOP")
                     output$AOP_size <- renderPrint(total_size)
                   }
                 }
               })
  observeEvent(eventExpr = input$download_NEON_AOP,
               handlerExpr = {
                 showNotification(ui = "Download in progess…", id = "download_AOP", type = "message")
                 download <- try(byFileAOP(dpID = Product_ID_AOP(), site = Field_Site_AOP(), year = Year_AOP(), check.size = FALSE, savepath = '..'), silent = TRUE)
                 if (class(download) == "try-error") {
                   removeNotification("download_AOP")
                   sendSweetAlert(session, title = "Download failed", text = paste0("This could be due to the data package you tried to obtain or the neonUtlities package used to pull data. Read the error message: ", strsplit(download, ":")[[1]][2]), type = 'error')
                 } else {
                   removeNotification("download_AOP")
                   sendSweetAlert(session, title = "File downloaded", text = "Check the 'NEON Download' directory. Go to step 2 to unzip files and make them more accesible.", type = 'success')
                 }
               })
  
  ####— NEON: Step 3- Unzip/Join Downloads####
  # Variables
  NEON_folder_path <- reactive(req(readDirectoryInput(session, 'NEON_unzip_folder')))
  NEON_file_name <- reactive(req(input$NEON_unzip_file))
  NEON_file_path <- reactive(req(paste0("../NEON Downloads/", NEON_file_name())))
  # Server function needed by directoryInput (https://github.com/wleepang/shiny-directory-input)
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {input$NEON_unzip_folder},
               handlerExpr = {
                 if (input$NEON_unzip_folder > 0) {
                   # condition prevents handler execution on initial app launch, launch the directory selection dialog with initial path read from the widget
                   path = choose.dir(default = readDirectoryInput(session, 'NEON_unzip_folder'))
                   # update the widget value
                   updateDirectoryInput(session, 'NEON_unzip_folder', value = path)}
               })
  # Functions needed to make list of files reactive
  has.new.files <- function() {
    unique(list.files(path = '../NEON Downloads', pattern = ".zip"))
  }
  get.files <- function() {
    list.files(path = '../NEON Downloads', pattern = ".zip")
  }
  NEON_unzip_files <- reactivePoll(intervalMillis = 10, session, checkFunc = has.new.files, valueFunc = get.files)
  observeEvent(NEON_unzip_files(), ignoreInit = TRUE, ignoreNULL = TRUE, {
    updateSelectInput(session, inputId = 'NEON_unzip_file', choices = NEON_unzip_files())
  })
  # Unzip data: general/specific
  observeEvent(eventExpr = input$unzip_NEON_folder,
               handlerExpr = {
                 showNotification(ui = "Unzip in progess…", id = "unzip_normal", type = "message")
                 unzip <- try(stackByTable(filepath = NEON_folder_path(), folder = TRUE), silent = TRUE) 
                 if (class(unzip) == "try-error") {
                   removeNotification("unzip_normal")
                   sendSweetAlert(session, title = "Unzip failed", text = paste0("Check that you are unzipping the folder from part 2. Read the error code message: ", strsplit(unzip, ":")[[1]][-1]), type = "error")
                 } else {
                   removeNotification("unzip_normal")
                   sendSweetAlert(session, title = "File unzipped", text = "The outer appearance of the folder should be the same. On the inside, there should be a new folder called 'stackedFiles' which contains the datasets.", type = "success")
                 }
               })
  # Unzip data: manual
  observeEvent(eventExpr = input$unzip_NEON_file,
               handlerExpr = {
                 showNotification(ui = "Unzip in progess…", id = "unzip_manual", type = "message")
                 unzip <- try(stackByTable(filepath = NEON_file_path(), folder = FALSE))
                 if (class(unzip) == "try-error") {
                   removeNotification("unzip_manual")
                   sendSweetAlert(session, title = "Unzip failed", text = paste0("Check that you are unzipping the .zip file that was manually downloaded. Read the error code message: ", strsplit(unzip, ":")[[1]][-1]), type = "error")
                 } else {
                   removeNotification("unzip_manual")
                   sendSweetAlert(session, title = "File unzipped", text = paste0("There should now be a new folder titled '", strsplit(NEON_file_name(), ".zip")[[1]][1], "' with all of the datasets."), type = "success")
                 }
               })
  
  ####FOR ME TAB####
  
  #Text for troublshooting
  output$text_me <- renderText(input$sublocs_birdgrid & nrow(Subloc_tes_plots_bird()) != 0)
  #Text for troublshooting 2
  output$text_me_two <- renderText(!input$sublocs_birdgrid | (nrow(Subloc_tes_plots_bird()) == 0))
  #Table for troubleshooting
  #output$table_me <- renderDataTable()
}