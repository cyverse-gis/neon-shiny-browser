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
  legend <- reactiveValues(group = c("Field Sites", "Domains", "Flight Boxes", "Sub Locations"))
  
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
        showGroup("Satellite") %>%
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
                                           filter(Habitat %in% input$fieldsite_habitat) %>%
                                           filter(stateCode %in% input$fieldsite_state))
  Field_sites_poly_filtered <- reactive(FieldSite_poly %>% filter(code %in% Field_sites_point_filtered()$siteCode))
  Domain_included <- reactive(domain_data %>% filter(DomainName %in% input$fieldsite_domain))
  Domain_unincluded <- reactive(domain_data %>% filter(!(DomainName %in% input$fieldsite_domain)))
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
  
  #### —— Plot Flight Boxes ####
  observe({
    proxy <- leafletProxy("map")
    # pal <- colorFactor(palette = c("#FFFFFF", "#0000FF"), domain = Flight_data_filtered()$Year)
    if (nrow(Flight_data_filtered()) == 0) {
      proxy %>% clearGroup(group = "Flight Boxes")
    } else {
      proxy %>% clearGroup(group = "Flight Boxes") %>%
        # Areas for NEON flight paths (red)
        addPolygons(data = Flight_data_filtered()$geometry,
                    color = "Red",
                    group = "Flight Boxes",
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
                                  "<a href='https://www.neonscience.org/field-sites/field-sites-map/", Field_sites_point_filtered()$siteCode, "' target='_blank'>",
                                  Field_sites_point_filtered()$siteDescription, " (",
                                  Field_sites_point_filtered()$siteCode, ")", "</a>",
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
                                       "<a href='https://www.neonscience.org/field-sites/field-sites-map/", Field_sites_poly_filtered()$code[i], "' target='_blank'>",
                                       Field_sites_poly_filtered()$name[i], "</a>"),
                        opacity = 1,
                        fillOpacity = 0,
                        highlightOptions = highlightOptions(stroke = TRUE, color = "#39ff14", weight = 7)
            )
        } else if (is.list(Field_sites_poly_filtered()$coordinates[[i]])) {
          for (num in 1:length(Field_sites_poly_filtered()$coordinates[[i]])) {
          proxy %>%
            addPolylines(lng = Field_sites_poly_filtered()$coordinates[[i]][[num]][,1],
                         lat = Field_sites_poly_filtered()$coordinates[[i]][[num]][,2],
                         group = "Field Sites",
                         color = "#49E2BD",
                        # layerId = Field_sites_poly_filtered()$code[i],
                         popup = paste0("Boundaries for ",
                                        "<a href='https://www.neonscience.org/field-sites/field-sites-map/", Field_sites_poly_filtered()$code[i], " 'target='_blank'>",
                                        Field_sites_poly_filtered()$name[i], "</a>"),
                         opacity = 1,
                         fillOpacity = 0.4,
                         highlightOptions = highlightOptions(stroke = TRUE, color = "#39ff14", weight = 7)
            )
          }
        }
      }
    }
  })
  #### —— Plot Sublocations ####
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
    else if (!input$sublocs_phenologyplot | (nrow(Subloc_tes_plots_phe()) == 0)) {
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
    } else if (!input$sublocs_metstn | (nrow(Subloc_aqu_plots_well()) == 0)) {
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
    } else if (!input$sublocs_gauge | (nrow(Subloc_aqu_plots_gauge()) == 0)) {
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
    } else if (!input$sublocs_reach | (nrow(Subloc_aqu_plots_sensor()) == 0)) {
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
    } else if (!input$sublocs_riparian | (nrow(Subloc_aqu_plots_sensor()) == 0)) {
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
  # Hide Flight Boxes when launching app
  leafletProxy("map") %>% hideGroup("Flight Boxes")
  
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
                 updateRadioButtons(session, inputId = "map_features", selected = "fieldsites")
                 choices <- input$fieldsite_sublocs
                 updateSelectInput(session, inputId = "fieldsite_sublocs", selected = c(choices, input$NEONsite_zoom))
               })
  observeEvent(eventExpr = input$togglesite,
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
                 leafletProxy('map') %>% showGroup(group = "Sub Locations")
                 choices <- input$fieldsite_sublocs
                 updateSelectInput(session, inputId = "fieldsite_sublocs", selected = c(choices, input$NEONsite_zoom))
                 updateTabsetPanel(session, inputId = "main", selected = "data")
                 updateTabsetPanel(session, inputId = "data", selected = "find")
                 updateRadioButtons(session, inputId = "NEON_browsing_type", selected = "site")
                 updateRadioButtons(session, inputId = "NEONbrowsingstep_site", selected = "list")
                 updateSelectInput(session, inputId = "NEONsite_site", selected = input$NEONsite_zoom)
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
  
  ## for dropdown
  output$dropdown_site <- renderPrint(paste0(FieldSite_point$siteName[FieldSite_point$siteCode %in% input$NEONsite_zoom], " ", FieldSite_point$`Habitat Specific`[FieldSite_point$siteCode %in% input$NEONsite_zoom]))
  output$dropdown_state <- renderPrint(FieldSite_point$stateName[FieldSite_point$siteCode %in% input$NEONsite_zoom])
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
  output$NEONproductoptions_site <- renderDT(datatable(data.frame(unlist(NEONproductlist_site()[1]), unlist(NEONproductlist_site()[2])),
                                                       colnames = c("Product Name", "Product ID"), rownames = FALSE, extensions = 'Scroller', class = 'cell-border stripe hover order-column',
                                                       options = list(dom = 'tlfipr',
                                                                      lengthMenu = c(10,25,50),
                                                                      pageLength = 10,
                                                                      deferRender = TRUE,
                                                                      scrollY = '40vh'
                                                       ),
                                                       selection = list(mode = 'single', target = 'cell')))
  observeEvent(eventExpr = input$NEONproductoptions_site_cells_selected, ignoreInit = TRUE,
               handlerExpr = {
                 if (length(input$NEONproductoptions_site_cells_selected) > 0) {
                   updateRadioButtons(session, inputId = "NEONbrowsingstep_site", selected = "single")
                 }
                 updateTextInput(session = session, inputId = "NEONproductID_site", value = ifelse(length(input$NEONproductoptions_site_cells_selected)==0,NA,NEONproductlist_site()[[2]][[input$NEONproductoptions_site_cells_selected[1]]]))
               })
  # Display products: single
  output$NEONproductsite_site <- renderUI({
    site <- input$NEONsite_site
    HTML(paste0("<p style='border:1px; border-radius:5px; border-style:solid; border-color:#CCCCCC; padding: 0.5em;'>",site, "</p>"))
    })
  output$NEONproductname_site <- renderUI({
    if (length(NEONproductinfo_site()$productName) == 0) {
      HTML(paste0("<p style='border:1px; border-radius:5px; border-style:solid; border-color:#CCCCCC; padding: 0.5em;'>", "<br>", "</p>"))
    } else {
      HTML(paste0("<p style='border:1px; border-radius:5px; border-style:solid; border-color:#CCCCCC; padding: 0.5em;'><a href='http://data.neonscience.org/data-product-view?dpCode=", NEONproductID_site(),"' target='_blank'>", NEONproductinfo_site()$productName, "</a></p>"))
    }
  })
  
  # Buttons to toggle downloads
  observe({
    if (nrow(NEONproductinfo_product()) == 0) {
      hideElement(id = "togglegeneral_site", anim = TRUE, animType = "slide")
      hideElement(id = "togglespecific_site", anim = TRUE, animType = "slide") 
      hideElement(id = "toggleAOP_site", anim = TRUE, animType = "slide")
    }
    is_AOP <- if (nrow(NEONproductinfo_site()) > 0) {
      if (NEONproductinfo_site()$productScienceTeamAbbr == "AOP") {
        TRUE
      } else {
        FALSE
      }
    }
    if (nrow(NEONproductinfo_site()) > 0) {
      if (is_AOP == FALSE) {
        hideElement(id = "toggleAOP_site", anim = TRUE, animType = "slide")
        showElement(id = "togglegeneral_site", anim = TRUE, animType = "slide")
        showElement(id = "togglespecific_site", anim = TRUE, animType = "slide")  
      } else if (is_AOP == TRUE) {
        hideElement(id = "togglegeneral_site", anim = TRUE, animType = "slide")
        hideElement(id = "togglespecific_site", anim = TRUE, animType = "slide") 
        showElement(id = "toggleAOP_site", anim = TRUE, animType = "slide")
      }
    }
  })
  observeEvent(eventExpr = input$togglegeneral_site,
               handlerExpr = {
                 updateTextInput(session, inputId = "dpID_general", value = input$NEONproductID_site)
                 updateSelectInput(session, inputId = "location_NEON_general", selected = input$NEONsite_site)
                 updateTabsetPanel(session, inputId = "data", selected = "download")
                 updateRadioButtons(session, inputId = "NEON_download_type", selected = "general")
                 updateCheckboxInput(session, inputId = "toggledownload_site", value = FALSE)
                 updateRadioButtons(session, inputId = "NEONbrowsingstep_site", selected = "list")
               })
  observeEvent(eventExpr = input$togglespecific_site,
               handlerExpr = {
                 updateTextInput(session, inputId = "dpID_specific", value = input$NEONproductID_site)
                 updateSelectInput(session, inputId = "location_NEON_specific", selected = input$NEONsite_site)
                 if (length(input$NEONproducttable_site_cells_selected) > 0 & input$NEONproducttable_site_cells_selected[2] > 0) {
                   month_date <- datesTable(dates = NEONproductinfo_site()$siteCodes[[1]]$availableMonths[NEONproductinfo_site()$siteCodes[[1]]$siteCode %in% input$NEONsite_site][[1]], process = "cell", cells = input$NEONproducttable_site_cells_selected)
                   updateAirDateInput(session, inputId = "date_NEON", value = month_date)
                 }
                 updateTabsetPanel(session, inputId = "data", selected = "download")
                 updateRadioButtons(session, inputId = "NEON_download_type", selected = "specific")
                 updateCheckboxInput(session, inputId = "toggledownload_site", value = FALSE)
                 updateRadioButtons(session, inputId = "NEONbrowsingstep_site", selected = "list")
               })
  observeEvent(eventExpr = input$toggleAOP_site,
               handlerExpr = {
                 updateTextInput(session, inputId = "dpID_AOP", value = input$NEONproductID_site)
                 updateSelectInput(session, inputId = "location_NEON_AOP", selected = input$NEONsite_site)
                 if (length(input$NEONproducttable_site_cells_selected) > 0 & input$NEONproducttable_site_cells_selected[2] > 0) {
                   month_date <- datesTable(dates = NEONproductinfo_site()$siteCodes[[1]]$availableMonths[NEONproductinfo_site()$siteCodes[[1]]$siteCode %in% input$NEONsite_site][[1]], process = "cell", cells = input$NEONproducttable_site_cells_selected)
                   updateAirDateInput(session, inputId = "year_AOP", value = month_date)
                 }
                 updateTabsetPanel(session, inputId = "data", selected = "download")
                 updateRadioButtons(session, inputId = "NEON_download_type", selected = "AOP")
                 updateCheckboxInput(session, inputId = "toggledownload_site", value = FALSE)
                 updateRadioButtons(session, inputId = "NEONbrowsingstep_site", selected = "list")
               })
  
  output$NEONproductdesc_site <- renderUI({
    desc <- ifelse(length(NEONproductinfo_site()$productDescription) == 0,
                                                        yes = "<br>",
                                                        no = NEONproductinfo_site()$productDescription)
    HTML(paste0("<p style='border:1px; border-radius:5px; border-style:solid; border-color:#CCCCCC; padding: 0.5em;'>", desc, "</p>"))
    })
  output$NEONproductdesign_site <- renderUI({
    design <- ifelse(length(NEONproductinfo_site()$productDesignDescription) == 0,
                                                          yes = "<br>",
                                                          no = NEONproductinfo_site()$productDesignDescription)
    HTML(paste0("<p style='border:1px; border-radius:5px; border-style:solid; border-color:#CCCCCC; padding: 0.5em;'>", design, "</p>"))
    })
  output$NEONproductnotes_site <- renderUI({
    notes <- ifelse(length(NEONproductinfo_site()$productRemarks) == 0,
                                                         yes = "<br>",
                                                         no = NEONproductinfo_site()$productRemarks)
    HTML(paste0("<p style='border:1px; border-radius:5px; border-style:solid; border-color:#CCCCCC; padding: 0.5em;'>", notes, "</p>"))
    })
  output$NEONproducttable_site <- renderDT({
    dates <- if (length(NEONproductinfo_site()$siteCodes) == 0) {
      NA
    } else {
      NEONproductinfo_site()$siteCodes[[1]]$availableMonths[NEONproductinfo_site()$siteCodes[[1]]$siteCode %in% input$NEONsite_site][[1]]}
    if (sum(is.na(dates))) {
      datatable(data = data.frame())
    } else {
      date_table_names <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
      date_tables <- character(12)
      years <- NULL
      for (date in dates) {
        years <- c(years, strsplit(date, "-")[[1]][1])
      }
      years <- unique(years)
      years <- years[order(years)]
      datatable(datesTable(dates, "table"), colnames = years, rownames = date_table_names, class = 'cell-border stripe hover order-column compact',
                options = list(dom = "t",
                               pageLength = 12,
                               ordering = FALSE),
                selection = list(mode = 'single', target = 'cell'))
    }
  })
  output$NEONproductdates_site <- renderUI({
    dates <- if (length(NEONproductinfo_site()$siteCodes) == 0) {
      NULL
    } else {
      NEONproductinfo_site()$siteCodes[[1]]$availableMonths[NEONproductinfo_site()$siteCodes[[1]]$siteCode %in% input$NEONsite_site][[1]]}
    if (sum(is.na(dates)) | length(dates) == 0) {
      NULL
    } else {
      years <- NULL
      for (date in dates) {
        years <- c(years, strsplit(date, "-")[[1]][1])
      }
      years <- unique(years)
      years <- years[order(years)]
      date_list <- c("<b>", years[1], ": ", "</b>", paste(dates[grepl(years[1], dates)], collapse = ", "))
      for (year in years[-1]) {
        date_list <- c(date_list, "<br>","<b>", year, ": ", "</b>", paste(dates[grepl(year, dates)], collapse = ", "))
      }
      date_list <- paste(date_list, collapse = "")
      HTML(paste0("<p style='border:1px; border-radius:5px; border-style:solid; border-color:#CCCCCC; padding: 0.5em;'>", date_list, "</p>"))
    }
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
  output$NEONproductoptions_product <- renderDT(datatable(NEONproductlist_product()[1:2], class = 'cell-border stripe hover order-column', rownames = FALSE,
                                                          options = list(dom = 'tlfipr',
                                                                         lengthMenu = c(10,25,50),
                                                                         pageLength = 25,
                                                                         deferRender = TRUE,
                                                                         scrollY = '40vh'
                                                          ),
                                                          selection = list(mode = 'single', target = 'cell')))
  observeEvent(eventExpr = input$NEONproductoptions_product_cells_selected,
               handlerExpr = {
                 if (length(input$NEONproductoptions_product_cells_selected) > 0) {
                   updateRadioButtons(session, inputId = "NEONbrowsingstep_product", selected = "single")
                 }
                 updateTextInput(session = session, inputId = "NEONproductID_product", value = ifelse(length(input$NEONproductoptions_product_cells_selected)==0,NA,NEONproductlist_product()[[2]][[input$NEONproductoptions_product_cells_selected[1]]]))
               })
  # Display products: single
  output$NEONproductname_product <- renderUI({
    if (length(NEONproductinfo_product()$productName) == 0) {
      HTML(paste0("<p style='border:1px; border-radius:5px; border-style:solid; border-color:#CCCCCC; padding: 0.5em;'>", "<br>", "</p>"))
    } else {
      HTML(paste0("<p style='border:1px; border-radius:5px; border-style:solid; border-color:#CCCCCC; padding: 0.5em;'><a href='http://data.neonscience.org/data-product-view?dpCode=", NEONproductID_product(),"' target='_blank'>", NEONproductinfo_product()$productName, "</a></p>"))
    }
  })
  # Buttons to toggle downloads
  output$nodates_download <- renderUI({
    if (nrow(NEONproductinfo_product()) > 0) {
      if (NEONproductinfo_product()$productStatus == "ACTIVE") {
      } else {
        HTML(paste0("<p style='border-left:5px solid #44ADE9;'>", "This product cannot be downloaded.", "</p>"))
      }
    } 
    else {
      NULL
    }
  })
  observe({
    if (nrow(NEONproductinfo_product()) == 0) {
      hideElement(id = "togglegeneral_product", anim = TRUE, animType = "slide")
      hideElement(id = "togglespecific_product", anim = TRUE, animType = "slide") 
      hideElement(id = "toggleAOP_product", anim = TRUE, animType = "slide")
    } else {
      available <- if (NEONproductinfo_product()$productStatus == "ACTIVE") {
        TRUE
      } else {
        FALSE
      }
      is_AOP <- if (NEONproductinfo_product()$productScienceTeamAbbr == "AOP") {
        TRUE
      } else {
        FALSE
      }
      if (available == TRUE) {
        if (is_AOP == FALSE) {
          hideElement(id = "toggleAOP_product", anim = TRUE, animType = "slide")
          showElement(id = "togglegeneral_product", anim = TRUE, animType = "slide")
          showElement(id = "togglespecific_product", anim = TRUE, animType = "slide")  
        } else if (is_AOP == TRUE) {
          hideElement(id = "togglegeneral_product", anim = TRUE, animType = "slide")
          hideElement(id = "togglespecific_product", anim = TRUE, animType = "slide") 
          showElement(id = "toggleAOP_product", anim = TRUE, animType = "slide")
        }
      } else {
        hideElement(id = "togglegeneral_product", anim = TRUE, animType = "slide")
        hideElement(id = "togglespecific_product", anim = TRUE, animType = "slide") 
        hideElement(id = "toggleAOP_product", anim = TRUE, animType = "slide")
      }
    }
  })
  observeEvent(eventExpr = input$togglegeneral_product,
               handlerExpr = {
                 updateTextInput(session, inputId = "dpID_general", value = input$NEONproductID_product)
                 updateSelectInput(session, inputId = "location_NEON_general", selected = input$NEONsite_product)
                 updateTabsetPanel(session, inputId = "data", selected = "download")
                 updateRadioButtons(session, inputId = "NEON_download_type", selected = "general")
                 updateCheckboxInput(session, inputId = "toggledownload_product", value = FALSE)
                 updateRadioButtons(session, inputId = "NEONbrowsingstep_product", selected = "list")
               })
  observeEvent(eventExpr = input$togglespecific_product,
               handlerExpr = {
                 updateTextInput(session, inputId = "dpID_specific", value = input$NEONproductID_product)
                 updateSelectInput(session, inputId = "location_NEON_specific", selected = input$NEONsite_product)
                 if (length(input$NEONproducttable_product_cells_selected) > 0 & input$NEONproducttable_product_cells_selected[2] > 0) {
                   month_date <- datesTable(dates = NEONproductinfo_product()$siteCodes[[1]]$availableMonths[NEONproductinfo_product()$siteCodes[[1]]$siteCode %in% input$NEONsite_product][[1]], process = "cell", cells = input$NEONproducttable_product_cells_selected)
                   updateAirDateInput(session, inputId = "date_NEON", value = month_date)
                 }
                 updateTabsetPanel(session, inputId = "data", selected = "download")
                 updateRadioButtons(session, inputId = "NEON_download_type", selected = "specific")
                 updateCheckboxInput(session, inputId = "toggledownload_product", value = FALSE)
                 updateRadioButtons(session, inputId = "NEONbrowsingstep_product", selected = "list")
               })
  observeEvent(eventExpr = input$toggleAOP_product,
               handlerExpr = {
                 updateTextInput(session, inputId = "dpID_AOP", value = input$NEONproductID_product)
                 updateSelectInput(session, inputId = "location_NEON_AOP", selected = input$NEONsite_product)
                 if (length(input$NEONproducttable_product_cells_selected) > 0 & input$NEONproducttable_product_cells_selected[2] > 0) {
                   month_date <- datesTable(dates = NEONproductinfo_product()$siteCodes[[1]]$availableMonths[NEONproductinfo_product()$siteCodes[[1]]$siteCode %in% input$NEONsite_product][[1]], process = "cell", cells = input$NEONproducttable_product_cells_selected)
                   updateAirDateInput(session, inputId = "year_AOP", value = month_date)
                 }
                 updateTabsetPanel(session, inputId = "data", selected = "download")
                 updateRadioButtons(session, inputId = "NEON_download_type", selected = "AOP")
                 updateCheckboxInput(session, inputId = "toggledownload_product", value = FALSE)
                 updateRadioButtons(session, inputId = "NEONbrowsingstep_product", selected = "list")
               })
  
  output$NEONproductdesc_product <- renderUI({
    desc <- ifelse(length(NEONproductinfo_product()$productDescription) == 0,
                   yes = "<br>",
                   no = NEONproductinfo_product()$productDescription)
    HTML(paste0("<p style='border:1px; border-radius:5px; border-style:solid; border-color:#CCCCCC; padding: 0.5em;'>", desc, "</p>"))
    })
  output$NEONproductdesign_product <- renderUI({
    design <- ifelse(length(NEONproductinfo_product()$productDesignDescription) == 0,
                     yes = "<br>",
                     no = NEONproductinfo_product()$productDesignDescription)
    HTML(paste0("<p style='border:1px; border-radius:5px; border-style:solid; border-color:#CCCCCC; padding: 0.5em;'>", design, "</p>"))
  })
  output$NEONproductnotes_product <- renderPrint({
    notes <- ifelse(length(NEONproductinfo_product()$productRemarks) == 0,
                    yes = "<br>",
                    no = NEONproductinfo_product()$productRemarks)
    HTML(paste0("<p style='border:1px; border-radius:5px; border-style:solid; border-color:#CCCCCC; padding: 0.5em;'>", notes, "</p>"))
  })
  output$ui_selectsite<- renderUI({
    sites <- if (length(NEONproductinfo_product()$siteCodes) == 0) {
      NA
    } else {
      sort(NEONproductinfo_product()$siteCodes[[1]]$siteCode)}
    selectInput(inputId = "NEONsite_product", label = "Available sites:", choices = req(sites))
  })
  output$nodates_message <- renderUI({
    if (nrow(NEONproductinfo_product()) > 0) {
      if (NEONproductinfo_product()$productStatus == "ACTIVE") {
      } else if (NEONproductinfo_product()$productStatus == "ONREQUEST") {
        HTML("<p style='border:1px; border-radius:5px; border-style:solid; border-color:#CCCCCC; padding: 0.5em;'>This product is only available by request.</p>")
      } else if (NEONproductinfo_product()$productStatus == "FUTURE") {
        if (NEONproductinfo_product()$productCode %in% c('DP1.00010.001', 'DP1.00007.001', 'DP1.00036.001', 'DP1.00037.001',
                                                         'DP4.00067.001', 'DP1.00099.001', 'DP1.00034.001', 'DP2.00008.001',
                                                         'DP3.00009.001', 'DP4.00201.001', 'DP1.00100.001', 'DP1.00035.001', 
                                                         'DP2.00009.001', 'DP3.00010.001', 'DP4.00137.001', 'DP4.00007.001',
                                                         'DP4.00002.001', 'DP2.00024.001', 'DP3.00008.001')) {
          HTML(paste0("<p style='border:1px; border-radius:5px; border-style:solid; border-color:#CCCCCC; padding: 0.5em;'>", "This data product is bundled into DP4.00200.001, ", actionLink(inputId = "eddy_covariance", label = "Bundled data products - eddy covariance"), " and is not available as a stand-alone download." ,"</p>"))
        }
      }
    } else {}
  })
  observeEvent(input$eddy_covariance, updateTextInput(session, inputId = "NEONproductID_product", value = "DP4.00200.001"))
  output$NEONproducttable_product <- renderDT({
    dates <- if (length(NEONproductinfo_product()$siteCodes) == 0) {
      NA
    } else { 
      NEONproductinfo_product()$siteCodes[[1]]$availableMonths[NEONproductinfo_product()$siteCodes[[1]]$siteCode %in% input$NEONsite_product][[1]]}
    if (sum(is.na(dates)) | length(dates) == 0) {
      datatable(data = data.frame())
    } else {
      date_table_names <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
      date_tables <- character(12)
      years <- NULL
      for (date in dates) {
        years <- c(years, strsplit(date, "-")[[1]][1])
      }
      years <- unique(years)
      years <- years[order(years)]
      datatable(datesTable(dates, "table"), colnames = years, rownames = date_table_names, class = 'cell-border stripe hover order-column compact',
                options = list(dom = "t",
                               pageLength = 12,
                               ordering = FALSE),
                selection = list(mode = 'single', target = 'cell'))
    }
  })
  output$NEONproductdates_product <- renderUI({
    dates <- if (length(NEONproductinfo_product()$siteCodes) == 0) {
      NA
    } else { 
      NEONproductinfo_product()$siteCodes[[1]]$availableMonths[NEONproductinfo_product()$siteCodes[[1]]$siteCode %in% input$NEONsite_product][[1]]}
    if (sum(is.na(dates)) | length(dates) == 0) {
      NULL
    } else {
      years <- NULL
      for (date in dates) {
        years <- c(years, strsplit(date, "-")[[1]][1])
      }
      years <- unique(years)
      years <- years[order(years)]
      date_list <- c("<b>", years[1], ": ", "</b>", paste(dates[grepl(years[1], dates)], collapse = ", "))
      for (year in years[-1]) {
        date_list <- c(date_list, "<br>","<b>", year, ": ", "</b>", paste(dates[grepl(year, dates)], collapse = ", "))
      }
      date_list <- paste(date_list, collapse = "")
      HTML(paste0("<p style='border:1px; border-radius:5px; border-style:solid; border-color:#CCCCCC; padding: 0.5em;'>", date_list, "</p>"))
    }
  })
  output$NEONproductURL_site <- renderPrint({
    Urls <- if (length(NEONproductinfo_site()$siteCodes) == 0) {
      NA
    } else {
      NEONproductinfo_site()$siteCodes[[1]]$availableDataUrls[NEONproductinfo_site()$siteCodes[[1]]$siteCode %in% input$NEONsite_site][[1]]}
    req(Urls)
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
  Field_Site_general <- reactive(req(input$location_NEON_general))
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
  # Calculate Size
  observeEvent(eventExpr = input$get_general_size, ignoreInit = TRUE,
               handlerExpr = {
                 if (input$dpID_general == "") {}
                 else {
                   output$general_size <- renderPrint("")
                   showNotification(ui = "Calculation in progress...", id = "calculation_general", type = "message")
                   size <- try(getProductSize(dpID = Product_ID_general(), site = Field_Site_general(), package = Package_type_general()), silent = TRUE)
                   if (class(size) == "try-error") {
                     removeNotification(id = "calculation_general")
                     sendSweetAlert(session, title = "Calculation failed", text = paste0("The product that you tried to calculate size for was invalid. Read the error message: ", strsplit(size, ":")[[1]][2]), type = 'error')
                   } else {
                     if (size < 1 & size != 0) {
                       size_kb <- size * 10^3
                       total_size <- paste0(as.character(size_kb), " KB")
                     } else if (size > 1) {
                       size_mb <- size
                       total_size <- paste0(as.character(size_mb), " MB")
                     } else if (size == 0) {
                       total_size <- "No data available"
                     }
                     output$general_size <- renderPrint(total_size)
                   }
                   removeNotification(id = "calculation_general")
                 }
               })
  # Download
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
                   sendSweetAlert(session, title = "Calculation failed", text = "Please choose an AOP product", type = 'error')
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
                       if (class(data) != "try-error") {
                         size <- as.numeric(data$size)
                         total_size <- total_size + sum(size)
                       }
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
  output$text_me <- renderText(length(NEONproductinfo_site()$productName) == 0)
  #Text for troublshooting 2
  output$text_me_two <- renderText(input$NEONproducttable_product_cells_selected)
  #Table for troubleshooting
  #output$table_me <- shiny::renderDataTable()
}