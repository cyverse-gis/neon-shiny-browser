# Shiny server
function(input, output, session) {
  # Initialization
  if (dir_created == TRUE) {
    delay(ms = 5000, showNotification(ui = "'NEON Downloads' folder created outside the directory containing this app. All downloads will go to this folder.", duration = NULL, type = "message"))
  } else {
    delay(ms = 5000, showNotification(ui = "Welcome back!", duration = 15, type = "message"))
  }
  delay(ms = 5000, expr = showNotification(ui = "First time here?", action = actionLink(inputId = "firsttime", label = "Yes"), duration = 15, type = "message", id = "first"))
  observeEvent(input$firsttime, confirmSweetAlert(session, inputId = "firsttime_confirm", title = "Welcome to the CyVerse NEON Browser!", text = "This will bring you to the tutorial section and get you started with NEON and this app.", btn_labels = c("Cancel", "Confirm")))
  observeEvent(input$firsttime_confirm, handlerExpr = {
    if (input$firsttime_confirm == TRUE) {
      updateNavbarPage(session, inputId = "main", selected = "Help/Tutorials")
      removeNotification(id = "first", session)
    }
  })
  
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
    map %>% setView(lng = -98.5795, lat = 39.8283, zoom = 2.5) %>% hideGroup("Flight Boxes")
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
      addPolygons(data = Domain_unincluded()$geometry,
                  weight = 2,
                  fillOpacity = '0.18',
                  group = "Domains",
                  popup = paste0(Domain_unincluded()$DomainName),
                  color = "gray") %>%
      addPolygons(data = Domain_included()$geometry,
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
    proxy <- leafletProxy("map")
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
                        group = "Sub Locations",
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
    proxy <- leafletProxy("map")
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
    proxy <- leafletProxy("map")
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
                        group = "Sub Locations",
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
    proxy <- leafletProxy("map")
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
    proxy <- leafletProxy("map")
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
                        group = "Sub Locations",
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
    proxy <- leafletProxy("map")
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
                        group = "Sub Locations",
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
    proxy <- leafletProxy("map")
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
    proxy <- leafletProxy("map")
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
    proxy <- leafletProxy("map")
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
    proxy <- leafletProxy("map")
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
    proxy <- leafletProxy("map")
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
    proxy <- leafletProxy("map")
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
                 if (input$NEONsite_dropdown %in% FieldSite_Tes) {
                   leafletProxy("map") %>% flyTo(lng = FieldSite_point$siteLongitude[FieldSite_point$siteCode %in% input$NEONsite_dropdown],
                                                 lat = FieldSite_point$siteLatitude[FieldSite_point$siteCode %in% input$NEONsite_dropdown],
                                                 zoom = 12)
                 } else if (input$NEONsite_dropdown %in% FieldSite_Aqu) {
                   leafletProxy("map") %>% flyTo(lng = FieldSite_point$siteLongitude[FieldSite_point$siteCode %in% input$NEONsite_dropdown],
                                                 lat = FieldSite_point$siteLatitude[FieldSite_point$siteCode %in% input$NEONsite_dropdown],
                                                 zoom = 15.5)
                 }
               }
  )
  observeEvent(eventExpr = input$addsublocs,
               handlerExpr = {
                 leafletProxy("map") %>% showGroup(group = "Sub Locations")
                 updateTabsetPanel(session, inputId = "main_data", selected = "filter")
                 updateRadioButtons(session, inputId = "map_features", selected = "fieldsites")
                 choices <- input$fieldsite_sublocs
                 updateSelectInput(session, inputId = "fieldsite_sublocs", selected = c(choices, input$NEONsite_dropdown))
               })
  observeEvent(eventExpr = input$togglesite,
               handlerExpr = {
                 if (input$NEONsite_dropdown %in% FieldSite_Tes) {
                   leafletProxy("map") %>% flyTo(lng = FieldSite_point$siteLongitude[FieldSite_point$siteCode %in% input$NEONsite_dropdown],
                                                 lat = FieldSite_point$siteLatitude[FieldSite_point$siteCode %in% input$NEONsite_dropdown],
                                                 zoom = 12)
                 } else if (input$NEONsite_dropdown %in% FieldSite_Aqu) {
                   leafletProxy("map") %>% flyTo(lng = FieldSite_point$siteLongitude[FieldSite_point$siteCode %in% input$NEONsite_dropdown],
                                                 lat = FieldSite_point$siteLatitude[FieldSite_point$siteCode %in% input$NEONsite_dropdown],
                                                 zoom = 15.5)
                 }
                 leafletProxy("map") %>% showGroup(group = "Sub Locations")
                 choices <- input$fieldsite_sublocs
                 updateSelectInput(session, inputId = "fieldsite_sublocs", selected = c(choices, input$NEONsite_dropdown))
                 updateTabsetPanel(session, inputId = "main_data", selected = "data")
                 updateTabsetPanel(session, inputId = "data", selected = "find")
                 updateRadioButtons(session, inputId = "NEON_browsing_type", selected = "site")
                 updateRadioButtons(session, inputId = "NEONbrowsingstep_site", selected = "list")
                 updateSelectInput(session, inputId = "NEONsite_site", selected = input$NEONsite_dropdown)
               })
  ####— NEON: Step 1- Find data ####
  ## for dropdown
  output$dropdown_site <- renderPrint(paste0(FieldSite_point$siteName[FieldSite_point$siteCode %in% input$NEONsite_dropdown], " ", FieldSite_point$`Habitat Specific`[FieldSite_point$siteCode %in% input$NEONsite_dropdown]))
  output$dropdown_state <- renderPrint(FieldSite_point$stateName[FieldSite_point$siteCode %in% input$NEONsite_dropdown])
  output$dataproduct_number <- renderPrint(nrow(NEONproducts_product[filter_site(site = input$NEONsite_dropdown),]))
  ####—— 1a: By Site####
  # Variables
  NEONproducts_product <<- nneo_products() # Added this variable up here because one item in finding by "site" needed it
  NEONproducts_site <- reactive(NEONproducts_product[filter_site(site = input$NEONsite_site),])
  # list: getting data frame of availability based on site code
  # Filter by keywords, type, theme
  keyword_lists(list = FieldSite_abbs)
  output$ui_selectkeywords_site <- renderUI({
    selectInput(inputId = "NEONproductkeywords_site", label = "Keywords", choices = get(x = input$NEONsite_site, envir = .NEON_keywords), multiple = TRUE)
  })
  NEONproducts_site_filter <- reactive(as.data.frame(cbind("Product Name" = NEONproducts_site()$productName, "Product ID" = NEONproducts_site()$productCode, "keywords" = NEONproducts_site()$keywords, "producttype" = NEONproducts_site()$productScienceTeam, "themes" = NEONproducts_site()$themes))[order(NEONproducts_site()$productName),])
  keyword_filters_site <- reactive(filter_keyword(column = NEONproducts_site_filter()$keywords, keywords = input$NEONproductkeywords_site) & filter_keyword(column = NEONproducts_site_filter()$themes, keywords = input$selectproducttheme_site))
  NEONproductlist_site_filtered <- reactive(NEONproducts_site_filter()[keyword_filters_site(),])
  datatype_filters_site <- reactive({
    if (is.null(input$selectproducttype_site)) {
      NEON_datatypes
    } else {
      input$selectproducttype_site
    }
  })
  NEONproductlist_site <- reactive(NEONproductlist_site_filtered()[(NEONproductlist_site_filtered()$producttype %in% datatype_filters_site()),])
  # Filters
  observe({
    if (input$showfilterinfo_site == TRUE) {
      addTooltip(session, id = "NEONproductkeywords_site", title = HTML("Filter data products by keywords describing their contents. Each product can have more than one, so only products that have <u>all</u> of the keywords chosen will appear."), trigger = "focus", placement = "top")
      addTooltip(session, id = "selectproducttype_site", title = HTML("Filter data products by their data collection method. Each product has one type, so the filter includes all products with the chosen types. Learn more about what each method means <a href='https://www.neonscience.org/data-collection' target='_blank'>here</a>."), trigger = "focus", placement = "top")
      addTooltip(session, id = "selectproducttheme_site", title = HTML("Filter data products by their theme. Each product can have more than one, so only products that have <u>all</u> of the themes chosen will appear. Learn more about each theme <a href='https://www.neonscience.org/data/data-themes' target='_blank'>here</a>."), trigger = "focus", placement = "top")
    } else {
      removeTooltip(session, id = "NEONproductkeywords_site")
      removeTooltip(session, id = "selectproducttype_site")
      removeTooltip(session, id = "selectproducttheme_site")
    }
  })
  # Display products: list
  output$NEONproductoptions_site <- renderDT(datatable(data.frame(unlist(NEONproductlist_site()[1]), unlist(NEONproductlist_site()[2])),
                                                       colnames = c("Product Name", "Product ID"), rownames = FALSE, extensions = 'Scroller', class = 'cell-border stripe hover order-column',
                                                       options = list(dom = 'tlfipr',
                                                                      lengthMenu = c(10,25,50),
                                                                      pageLength = 25,
                                                                      deferRender = TRUE,
                                                                      scrollY = '40vh'
                                                       ),
                                                       selection = list(mode = 'single', target = 'cell')))
  observeEvent(eventExpr = input$NEONproductoptions_site_cells_selected,
               handlerExpr = {
                 if (length(input$NEONproductoptions_site_cells_selected) > 0) {
                   updateRadioButtons(session, inputId = "NEONbrowsingstep_site", selected = "single")
                 }
                 updateTextInput(session = session, inputId = "NEONproductID_site", value = ifelse(length(input$NEONproductoptions_site_cells_selected)==0,NA,NEONproductlist_site()[[2]][[input$NEONproductoptions_site_cells_selected[1]]]))
               })
  # Modal
  output$NEONproductoptions_site2 <- renderDT(datatable(data.frame(unlist(NEONproductlist_site()[1]), unlist(NEONproductlist_site()[2])),
                                                        colnames = c("Product Name", "Product ID"), rownames = FALSE, extensions = 'Scroller', class = 'cell-border stripe hover order-column',
                                                        options = list(dom = 'tlfipr',
                                                                       lengthMenu = c(10,25,50),
                                                                       pageLength = 25,
                                                                       deferRender = TRUE,
                                                                       scrollY = '50vh'
                                                        ), caption = HTML("<center>Click on a product to view it</center>"),
                                                        selection = list(mode = 'single', target = 'cell')))
  output$ui_selectkeywords_site2 <- renderUI({
    selectInput(inputId = "NEONproductkeywords_site2", label = "Keywords", choices = get(x = input$NEONsite_site, envir = .NEON_keywords), multiple = TRUE)
  })
  observeEvent(input$NEONproductkeywords_site2, updateSelectInput(session, inputId = "NEONproductkeywords_site", selected = input$NEONproductkeywords_site2))
  observe({
    if (length(input$NEONproductkeywords_site2) == 0) {
      updateSelectInput(session, inputId = "NEONproductkeywords_site", selected = NA)
    }
  })
  observeEvent(input$NEONproductkeywords_site, updateSelectInput(session, inputId = "NEONproductkeywords_site2", selected = input$NEONproductkeywords_site))
  observe({
    if (length(input$NEONproductkeywords_site) == 0) {
      updateSelectInput(session, inputId = "NEONproductkeywords_site2", selected = NA)
    }
  })
  observeEvent(input$selectproducttype_site2, updateSelectInput(session, inputId = "selectproducttype_site", selected = input$selectproducttype_site2))
  observe({
    if (length(input$selectproducttype_site2) == 0) {
      updateSelectInput(session, inputId = "selectproducttype_site", selected = NA)
    }
  })
  observeEvent(input$selectproducttype_site, updateSelectInput(session, inputId = "selectproducttype_site2", selected = input$selectproducttype_site))
  observe({
    if (length(input$selectproducttype_site) == 0) {
      updateSelectInput(session, inputId = "selectproducttype_site2", selected = NA)
    }
  })
  observeEvent(input$selectproducttheme_site2, updateSelectInput(session, inputId = "selectproducttheme_site", selected = input$selectproducttheme_site2))
  observe({
    if (length(input$selectproducttheme_site2) == 0) {
      updateSelectInput(session, inputId = "selectproducttheme_site", selected = NA)
    }
  })
  observeEvent(input$selectproducttheme_site, updateSelectInput(session, inputId = "selectproducttheme_site2", selected = input$selectproducttheme_site))
  observe({
    if (length(input$selectproducttheme_site) == 0) {
      updateSelectInput(session, inputId = "selectproducttheme_site2", selected = NA)
    }
  })
  observe({
    if (input$showfilterinfo_site2 == TRUE) {
      addTooltip(session, id = "NEONproductkeywords_site2", title = HTML("Filter data products by keywords describing their contents. Each product can have more than one, so only products that have <u>all</u> of the keywords chosen will appear."), trigger = "focus", placement = "top")
      addTooltip(session, id = "selectproducttype_site2", title = HTML("Filter data products by their data collection method. Each product has one type, so the filter includes all products with the chosen types. Learn more about what each method means <a href='https://www.neonscience.org/data-collection' target='_blank'>here</a>."), trigger = "focus", placement = "top")
      addTooltip(session, id = "selectproducttheme_site2", title = HTML("Filter data products by their theme. Each product can have more than one, so only products that have <u>all</u> of the themes chosen will appear. Learn more about each theme <a href='https://www.neonscience.org/data/data-themes' target='_blank'>here</a>"), trigger = "focus", placement = "top")
    } else {
      removeTooltip(session, id = "NEONproductkeywords_site2")
      removeTooltip(session, id = "selectproducttype_site2")
      removeTooltip(session, id = "selectproducttheme_site2")
    }
  })
  observeEvent(eventExpr = input$NEONproductoptions_site2_cells_selected,
               handlerExpr = {
                 if (length(input$NEONproductoptions_site2_cells_selected) > 0) {
                   updateRadioButtons(session, inputId = "NEONbrowsingstep_site", selected = "single")
                   toggleModal(session, modalId = "tableexpand_site", toggle = "close")
                 }
                 updateTextInput(session = session, inputId = "NEONproductID_site", value = ifelse(length(input$NEONproductoptions_site2_cells_selected)==0, NA, NEONproductlist_site()[[2]][[input$NEONproductoptions_site2_cells_selected[1]]]))
               })
  modal_clicked_site <- 0
  observeEvent(input$expandtable_site, handlerExpr = {
    modal_clicked_site <<- modal_clicked_site + 1
    if (modal_clicked_site < 2) {
      toggleDropdownButton(inputId = "filter_site")
      delay(ms = 2000, expr = toggleDropdownButton(inputId = "filter_site"))
    }
  })
  # Single: filtering column of products for one site through ID
  NEONproductID_site <- reactive(req(
    if (gsub(pattern = " ", replacement = "", x = input$NEONproductID_site) == "") {
      "random string that will not match to anything"
    } else {
      gsub(pattern = " ", replacement = "", x = input$NEONproductID_site)
    }
  ))
  NEONproductinfo_site <- reactive(req(filter(.data = NEONproducts_site(), productCode == NEONproductID_site())))
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
    if (nrow(NEONproductinfo_site()) == 0) {
      hideElement(id = "toggledownload_site", anim = TRUE, animType = "slide")
    } else {
      if (NEONproductinfo_site()$productStatus == "ACTIVE") {
        showElement(id = "toggledownload_site", anim = TRUE, animType = "slide")
      } else {
        hideElement(id = "toggledownload_site", anim = TRUE, animType = "slide")
      }
    }
  })
  observeEvent(input$toggledownload_site, handlerExpr = {
    is_AOP <- if (NEONproductinfo_site()$productScienceTeamAbbr == "AOP") {
      TRUE
    } else {
      FALSE
    }
    if (!is_AOP) {
      updateTextInput(session, inputId = "dpID_regular", value = input$NEONproductID_site)
      delay(ms = 1000, updateSelectInput(session, inputId = "fieldsite_NEON_regular", selected = input$NEONsite_site))
      updateTabsetPanel(session, inputId = "data", selected = "download")
      updateRadioButtons(session, inputId = "NEON_download_type", selected = "regular")
      updateCheckboxInput(session, inputId = "toggledownload_site", value = FALSE)
    } else if (is_AOP) {
      updateTextInput(session, inputId = "dpID_AOP", value = input$NEONproductID_site)
      delay(ms = 1000, updateSelectInput(session, inputId = "fieldsite_NEON_AOP", selected = input$NEONsite_site))
      updateTabsetPanel(session, inputId = "data", selected = "download")
      updateRadioButtons(session, inputId = "NEON_download_type", selected = "AOP")
      updateCheckboxInput(session, inputId = "toggledownload_site", value = FALSE)
    }
  })
  
  output$NEONproductdesc_site <- renderUI({
    desc <- ifelse(length(NEONproductinfo_site()$productDescription) == 0,
                   yes = "<br>",
                   no = NEONproductinfo_site()$productDescription)
    HTML(paste0("<p style='border:1px; border-radius:5px; border-style:solid; border-color:#CCCCCC; padding: 0.5em;'>", desc, "</p>"))
  })
  output$NEONproductabstract_site <- renderUI({
    abstract <- ifelse(length(NEONproductinfo_site()$productAbstract) == 0,
                       yes = "<br>",
                       no = NEONproductinfo_site()$productAbstract)
    HTML(paste0("<p style='border:1px; border-radius:5px; border-style:solid; border-color:#CCCCCC; padding: 0.5em;'>", abstract, "</p>"))
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
  # Download full table
  output$fullinfo_site <- downloadHandler(
    filename = function() {
      paste0(input$NEONproductID_site, "_fullinfo.csv")
    },
    content = function(file) {
      table <- NEONproductinfo_site()
      for (i in 1:ncol(table)) {
        table[i] <- as.character(table[i])
      }
      write.csv(x = table, file = file)
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
  output$ui_selectkeywords_product <- renderUI({
    selectInput(inputId = "NEONproductkeywords_product", label = "Keywords", choices = keywords, multiple = TRUE)
  })
  NEONproduct_products_filter <- NEONproducts_product[c("productName", "productCode", "keywords", "productScienceTeam", "themes")]
  names(NEONproduct_products_filter) <- c('Product Name', 'Product ID', 'keywords', "producttype", "themes")
  NEONproduct_products_filter <- NEONproduct_products_filter[order(NEONproduct_products_filter$`Product Name`),]
  keyword_filters_product <- reactive(filter_keyword(column = NEONproduct_products_filter$keywords, keywords = input$NEONproductkeywords_product) & filter_keyword(column = NEONproduct_products_filter$themes, keywords = input$selectproducttheme_product))
  datatype_filters_product <- reactive({
    if (is.null(input$selectproducttype_product)) {
      NEON_datatypes
    } else {
      input$selectproducttype_product
    }
  })
  NEONproductlist_product <- reactive(NEONproduct_products_filter[keyword_filters_product(),] %>% filter(`producttype` %in% datatype_filters_product()))
  # Filters
  observe({
    if (input$showfilterinfo_product == TRUE) {
      addTooltip(session, id = "NEONproductkeywords_product", title = HTML("Filter data products by keywords describing their contents. Each product can have more than one, so only products that have <u>all</u> of the keywords chosen will appear."), trigger = "focus", placement = "top")
      addTooltip(session, id = "selectproducttype_product", title = HTML("Filter data products by their data collection method. Each product has one type, so the filter includes all products with the chosen types. Learn more about what each method means <a href='https://www.neonscience.org/data-collection' target='_blank'>here</a>."), trigger = "focus", placement = "top")
      addTooltip(session, id = "selectproducttheme_product", title = HTML("Filter data products by their theme. Each product can have more than one, so only products that have <u>all</u> of the themes chosen will appear. Learn more about each theme <a href='https://www.neonscience.org/data/data-themes' target='_blank'>here</a>"), trigger = "focus", placement = "top")
    } else {
      removeTooltip(session, id = "NEONproductkeywords_product")
      removeTooltip(session, id = "selectproducttype_product")
      removeTooltip(session, id = "selectproducttheme_product")
    }
  })
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
  # Modal
  output$NEONproductoptions_product2 <- renderDT(datatable(NEONproductlist_product()[1:2], class = 'cell-border stripe hover order-column', rownames = FALSE,
                                                           options = list(dom = 'tlfipr',
                                                                          lengthMenu = c(10,25,50),
                                                                          pageLength = 25,
                                                                          deferRender = TRUE,
                                                                          scrollY = '50vh'
                                                           ), caption = HTML("<center>Click on a product to view it</center>"),
                                                           selection = list(mode = 'single', target = 'cell')))
  output$ui_selectkeywords_product2 <- renderUI({
    selectInput(inputId = "NEONproductkeywords_product2", label = "Keywords", choices = get(x = input$NEONsite_site, envir = .NEON_keywords), multiple = TRUE)
  })
  observeEvent(input$NEONproductkeywords_product2, updateSelectInput(session, inputId = "NEONproductkeywords_product", selected = input$NEONproductkeywords_product2))
  observe({
    if (length(input$NEONproductkeywords_product2) == 0) {
      updateSelectInput(session, inputId = "NEONproductkeywords_product", selected = NA)
    }
  })
  observeEvent(input$NEONproductkeywords_product, updateSelectInput(session, inputId = "NEONproductkeywords_product2", selected = input$NEONproductkeywords_product))
  observe({
    if (length(input$NEONproductkeywords_product) == 0) {
      updateSelectInput(session, inputId = "NEONproductkeywords_product2", selected = NA)
    }
  })
  observeEvent(input$selectproducttype_product2, updateSelectInput(session, inputId = "selectproducttype_product", selected = input$selectproducttype_product2))
  observe({
    if (length(input$selectproducttype_product2) == 0) {
      updateSelectInput(session, inputId = "selectproducttype_product", selected = NA)
    }
  })
  observeEvent(input$selectproducttype_product, updateSelectInput(session, inputId = "selectproducttype_product2", selected = input$selectproducttype_product))
  observe({
    if (length(input$selectproducttype_product) == 0) {
      updateSelectInput(session, inputId = "selectproducttype_product2", selected = NA)
    }
  })
  observeEvent(input$selectproducttheme_product2, updateSelectInput(session, inputId = "selectproducttheme_product", selected = input$selectproducttheme_product2))
  observe({
    if (length(input$selectproducttheme_product2) == 0) {
      updateSelectInput(session, inputId = "selectproducttheme_product", selected = NA)
    }
  })
  observeEvent(input$selectproducttheme_product, updateSelectInput(session, inputId = "selectproducttheme_product2", selected = input$selectproducttheme_product))
  observe({
    if (length(input$selectproducttheme_product) == 0) {
      updateSelectInput(session, inputId = "selectproducttheme_product2", selected = NA)
    }
  })
  observe({
    if (input$showfilterinfo_product2 == TRUE) {
      addTooltip(session, id = "NEONproductkeywords_product2", title = HTML("Filter data products by keywords describing their contents. Each product can have more than one, so only products that have <u>all</u> of the keywords chosen will appear."), trigger = "focus", placement = "top")
      addTooltip(session, id = "selectproducttype_product2", title = HTML("Filter data products by their data collection method. Each product has one type, so the filter includes all products with the chosen types. Learn more about what each method means <a href='https://www.neonscience.org/data-collection' target='_blank'>here</a>."), trigger = "focus", placement = "top")
      addTooltip(session, id = "selectproducttheme_product2", title = HTML("Filter data products by their theme. Each product can have more than one, so only products that have <u>all</u> of the themes chosen will appear. Learn more about each theme <a href='https://www.neonscience.org/data/data-themes' target='_blank'>here</a>."), trigger = "focus", placement = "top")
    } else {
      removeTooltip(session, id = "NEONproductkeywords_product2")
      removeTooltip(session, id = "selectproducttype_product2")
      removeTooltip(session, id = "selectproducttheme_product2")
    }
  })
  observeEvent(eventExpr = input$NEONproductoptions_product2_cells_selected,
               handlerExpr = {
                 if (length(input$NEONproductoptions_product2_cells_selected) > 0) {
                   updateRadioButtons(session, inputId = "NEONbrowsingstep_product", selected = "single")
                   toggleModal(session, modalId = "tableexpand_product", toggle = "close")
                 }
                 updateTextInput(session = session, inputId = "NEONproductID_product", value = ifelse(length(input$NEONproductoptions_product2_cells_selected)==0, NA, NEONproductlist_product()[[2]][[input$NEONproductoptions_product2_cells_selected[1]]]))
               })
  modal_clicked_product <- 0
  observeEvent(input$expandtable_product, handlerExpr = {
    modal_clicked_product <<- modal_clicked_product + 1
    if (modal_clicked_product < 2) {
      toggleDropdownButton(inputId = "filter_product")
      delay(ms = 2000, expr = toggleDropdownButton(inputId = "filter_product"))
    }
  })
  # Single: filtering one row of parent NEON products table through ID
  NEONproductID_product <- reactive(req(
    ifelse(gsub(pattern = " ", replacement = "", x = input$NEONproductID_product) == "",
           yes = "random string that will not match to anything",
           no = gsub(pattern = " ", replacement = "", x = input$NEONproductID_product))
  ))
  NEONproductinfo_product <- reactive(req(filter(.data = NEONproducts_product, productCode == NEONproductID_product())))
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
  observeEvent(input$toggledownload_product, handlerExpr = {
    is_AOP <- if (NEONproductinfo_product()$productScienceTeamAbbr == "AOP") {
      TRUE
    } else {
      FALSE
    }
    if (!is_AOP) {
      updateTextInput(session, inputId = "dpID_regular", value = input$NEONproductID_product)
      delay(ms = 1000, updateSelectInput(session, inputId = "fieldsite_NEON_regular", selected = input$NEONsite_product))
      updateTabsetPanel(session, inputId = "data", selected = "download")
      updateRadioButtons(session, inputId = "NEON_download_type", selected = "regular")
      updateCheckboxInput(session, inputId = "toggledownload_product", value = FALSE)
    } else if (is_AOP) {
      updateTextInput(session, inputId = "dpID_AOP", value = input$NEONproductID_product)
      delay(ms = 1000, updateSelectInput(session, inputId = "fieldsite_NEON_AOP", selected = input$NEONsite_product))
      updateTabsetPanel(session, inputId = "data", selected = "download")
      updateRadioButtons(session, inputId = "NEON_download_type", selected = "AOP")
      updateCheckboxInput(session, inputId = "toggledownload_product", value = FALSE)
    }
  })
  observe({
    if (nrow(NEONproductinfo_product()) == 0) {
      hideElement(id = "toggledownload_product", anim = TRUE, animType = "slide")
    } else {
      if (NEONproductinfo_product()$productStatus == "ACTIVE") {
        showElement(id = "toggledownload_product", anim = TRUE, animType = "slide")
      } else {
        hideElement(id = "toggledownload_product", anim = TRUE, animType = "slide")
      }
    }
  })
  
  output$NEONproductdesc_product <- renderUI({
    desc <- ifelse(length(NEONproductinfo_product()$productDescription) == 0,
                   yes = "<br>",
                   no = NEONproductinfo_product()$productDescription)
    HTML(paste0("<p style='border:1px; border-radius:5px; border-style:solid; border-color:#CCCCCC; padding: 0.5em;'>", desc, "</p>"))
  })
  output$NEONproductabstract_product <- renderUI({
    abstract <- ifelse(length(NEONproductinfo_product()$productAbstract) == 0,
                       yes = "<br>",
                       no = NEONproductinfo_product()$productAbstract)
    HTML(paste0("<p style='border:1px; border-radius:5px; border-style:solid; border-color:#CCCCCC; padding: 0.5em;'>", abstract, "</p>"))
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
  # Download full table
  output$fullinfo_product <- downloadHandler(
    filename = function() {
      paste0(input$NEONproductID_product, "_fullinfo.csv")
    },
    content = function(file) {
      table <- NEONproductinfo_product()
      for (i in 1:ncol(table)) {
        table[i] <- as.character(table[i])
      }
      write.csv(x = table, file = file)
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
  Product_ID_regular <- reactive(req(
    ifelse(gsub(pattern = " ", replacement = "", x = input$dpID_regular) == "",
           yes = "random string that will not match to anything",
           no = gsub(pattern = " ", replacement = "", x = input$dpID_regular))
  ))
  Regular_ID_middle <- reactive(req(strsplit(Product_ID_regular(), "[.]")[[1]][2]))
  Folder_regular <- reactive(req(paste0("filesToStack", Regular_ID_middle())))
  Product_ID_AOP <- reactive(req(
    ifelse(gsub(pattern = " ", replacement = "", x = input$dpID_AOP) == "",
           yes = "random string that will not match to anything",
           no = gsub(pattern = " ", replacement = "", x = input$dpID_AOP))
  ))
  AOP_ID_middle <- reactive(req(strsplit(Product_ID_AOP(), "[.]")[[1]][2]))
  Field_Site_regular <- reactive(req(input$fieldsite_NEON_regular))
  Field_Site_AOP <- reactive(req(input$fieldsite_NEON_AOP))
  Package_type_regular <- reactive(req(input$package_type_regular))
  Folder_path_regular <- reactive(req(paste0("NEON_", Field_Site_regular(), "_", Product_ID_regular())))
  Folder_path_AOP <- reactive(req(paste0("NEON_", Field_Site_AOP(), "_", Product_ID_AOP(), "_", Year_AOP())))
  
  ####—— Download NEON data: Regular ####
  NEONproductinfo_regular <- reactive(req(filter(.data = NEONproducts_product, productCode == Product_ID_regular())))
  output$ui_fieldsite_regular <- renderUI({
    sites <- if (length(NEONproductinfo_regular()$siteCodes) == 0) {
      NA
    } else {
      sort(NEONproductinfo_regular()$siteCodes[[1]]$siteCode)}
    selectInput(inputId = "fieldsite_NEON_regular", label = "Field Site", choices = sites)}
  )
  output$download_dates_regular <- renderDT({
    dates <- if (length(NEONproductinfo_regular()$siteCodes) == 0) {
      NA
    } else { 
      NEONproductinfo_regular()$siteCodes[[1]]$availableMonths[NEONproductinfo_regular()$siteCodes[[1]]$siteCode %in% input$fieldsite_NEON_regular][[1]]}
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
                selection = list(mode = 'multiple', target = input$target_download_regular))
    }
  })
  product_dates <- reactive(NEONproductinfo_regular()$siteCodes[[1]]$availableMonths[NEONproductinfo_regular()$siteCodes[[1]]$siteCode %in% input$fieldsite_NEON_regular][[1]])
  download_dates <- reactive({
    if (nrow(NEONproductinfo_regular()) == 0) {
      NULL
    } else {
      if (length(product_dates()) == 0) {
      } else {
        if (input$target_download_regular == "cell") {
          if (input$cellselection_download_regular == "Start End") {
            selected <- input$download_dates_regular_cells_selected
            if (length(selected) == 0) {
              NULL
            } else {
              selected <- split(selected, ceiling(2*seq_along(selected)/length(selected)))
              dates <- NULL
              for (i in 1:length(selected[[1]])) {
                date <- datesTable(dates = product_dates(), process = "regular", selected = c(selected[[1]][i], selected[[2]][i]), target = input$target_download_regular)
                dates <- c(dates, date)
              }
              dates <- sort(dates)
              dates <- dates[grepl(pattern = "20", x = dates)]
              if (length(dates) >= 2) {
                start_date <- min(dates)
                stop_date <- max(dates)
                all_dates <- product_dates()[product_dates() >= start_date & product_dates() <= stop_date]
                all_dates 
              }
            }
          } else {
            selected <- input$download_dates_regular_cells_selected
            if (length(selected) == 0) {
              NULL
            } else {
              selected <- split(selected, ceiling(2*seq_along(selected)/length(selected)))
              dates <- NULL
              for (i in 1:length(selected[[1]])) {
                date <- datesTable(dates = product_dates(), process = "regular", selected = c(selected[[1]][i], selected[[2]][i]), target = input$target_download_regular)
                dates <- c(dates, date)
              }
              dates <- sort(dates)
              dates <- dates[grepl(pattern = "20", x = dates)]
              dates
            }
          }
        } else if (input$target_download_regular == "row") {
          selected <- input$download_dates_regular_rows_selected
          if (length(selected) == 0) {
            NULL
          } else {
            dates <- NULL
            for (i in 1:length(selected)) {
              date <- datesTable(dates = product_dates(), process = "regular", selected = selected[i], target = input$target_download_regular)
              dates <- c(dates, date)
            }
            dates <- sort(dates)
            dates
          }
        } else if (input$target_download_regular == "column") {
          selected <- input$download_dates_regular_columns_selected
          if (length(selected) == 0) {
            NULL
          } else {
            selected <- selected[selected != 0]
            dates <- NULL
            for (i in 1:length(selected)) {
              date <- datesTable(dates = product_dates(), process = "regular", selected = selected[i], target = input$target_download_regular)
              dates <- c(date, dates)
            }
            dates <- sort(dates)
            dates
          }
        }
      }
    }
  })
  # Calculate Download Size
  observeEvent(eventExpr = input$get_regular_size, ignoreInit = TRUE,
               handlerExpr = {
                 if (input$dpID_regular == "" | length(download_dates()) == 0) {}
                 else {
                   output$regular_size <- renderPrint("")
                   withProgress(message = "Calculation in progress", value = 0, max = 1.1, expr = {
                     size <- try(getProductSize(dpID = Product_ID_regular(), site = Field_Site_regular(), package = Package_type_regular(), dates = download_dates()))
                   })
                   if (class(size) == "try-error") {
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
                     output$regular_size <- renderPrint(total_size)
                   }
                 }
               })
  # Download
  observeEvent(eventExpr = input$download_NEON_regular, ignoreInit = TRUE,
               handlerExpr = {
                 if (regexpr("DP[1-4]{1}.[0-9]{5}.001", Product_ID_regular()) != 1 | nrow(NEONproductinfo_regular()) == 0 | length(download_dates()) == 0) {}
                 else {
                   disable(id = "download_NEON_regular")
                   folder <- unique_folderpath(pathname = Folder_path_regular())
                   dir.create(paste0("../NEON Downloads/", folder))
                   withProgress(message = "Downloading files", value = 0, expr = {
                     dates_left <- length(download_dates())
                     for (date in download_dates()) {
                       incProgress(amount = 0, detail = paste0("Downloading ", date))
                       try(getPackage(dpID = Product_ID_regular(), site_code = Field_Site_regular(), year_month = date, package = Package_type_regular(), savepath = paste0("../NEON Downloads/", folder)))
                       incProgress(amount = 1/dates_left)
                     }
                   })
                   if (length(list.files(paste0("../NEON Downloads/", folder))) == 0) {
                     sendSweetAlert(session, title = "No Files", text = "There were no files downloaded. Please select available dates.", type = "error")
                     unlink(paste0("../NEON Downloads/", folder), recursive = T)
                     enable(id = "download_NEON_regular")
                   } else {
                     if (Product_ID_regular() == "DP4.00200.001") {
                       withProgress(message = "Transferring as zip", value = 0, expr = {
                         setwd(paste0("../NEON Downloads/", folder))
                         unzipEddy(site = Field_Site_regular(), path = folder)
                       })
                       size <- sum(file.info(list.files(paste0("../NEON Downloads/", folder), all.files = TRUE, recursive = TRUE, full.names = T))$size)
                       write_downloadSummary(method = "Regular", dpID = Product_ID_regular(), dpName = NEONproducts_product$productName[NEONproducts_product$productCode == Product_ID_regular()], site = Field_Site_regular(), dates = download_dates(), package = Package_type_regular(), size = utils:::format.object_size(x = size, units = "auto"), path = paste0("NEON Downloads/", folder))
                       sendSweetAlert(session, title = "Download Complete", text = paste0("Check the 'NEON Downloads' directory for a folder titled ", folder, "."), type = 'success')
                       enable(id = "download_NEON_regular")
                     } else {
                       withProgress(message = "Stacking files", value = 0, expr = {
                         stack <- try(stackByTable(filepath = paste0("../NEON Downloads/", folder), folder = T))
                       })
                       if (class(stack) == "try-error") {
                         sendSweetAlert(session, title = "Stack failed", text = "Something went wrong in the stacking process. Please submit an issue on Github.", type = "error")
                         enable(id = "download_NEON_regular")
                       } else {
                         for (file in list.files(paste0("../NEON Downloads/", folder, "/stackedFiles/"))) {
                           file.rename(from = paste0("../NEON Downloads/", folder, "/stackedFiles/", file), to = paste0("../NEON Downloads/", folder, "/", file))
                         }
                         unlink(x = paste0("../NEON Downloads/", folder, "/stackedFiles/"), recursive = T)
                         size <- sum(file.info(list.files(paste0("../NEON Downloads/", folder), all.files = TRUE, recursive = TRUE, full.names = T))$size)
                         write_downloadSummary(method = "Regular", dpID = Product_ID_regular(), dpName = NEONproducts_product$productName[NEONproducts_product$productCode == Product_ID_regular()], site = Field_Site_regular(), dates = download_dates(), package = Package_type_regular(), size = utils:::format.object_size(x = size, units = "auto"), path = paste0("NEON Downloads/", folder))
                         sendSweetAlert(session, title = "Download Complete", text = paste0("Check the 'NEON Downloads' directory for a folder titled ", folder, "."), type = 'success')
                         enable(id = "download_NEON_regular")
                       }
                     }
                   }
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
  # Display dates
  NEONproductinfo_AOP <- reactive(req(filter(.data = NEONproducts_product, productCode == Product_ID_AOP())))
  output$ui_fieldsite_AOP <- renderUI({
    sites <- if (length(NEONproductinfo_AOP()$siteCodes) == 0) {
      NA
    } else {
      sort(NEONproductinfo_AOP()$siteCodes[[1]]$siteCode)}
    selectInput(inputId = "fieldsite_NEON_AOP", label = "Field Site", choices = sites)
  })
  output$download_dates_AOP <- renderDT({
    dates <- if (length(NEONproductinfo_AOP()$siteCodes) == 0) {
      NA
    } else { 
      NEONproductinfo_AOP()$siteCodes[[1]]$availableMonths[NEONproductinfo_AOP()$siteCodes[[1]]$siteCode %in% input$fieldsite_NEON_AOP][[1]]}
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
                selection = list(mode = 'single', target = "column"))
    }
  })
  Year_AOP <- reactive({
    dates <- if (length(NEONproductinfo_AOP()$siteCodes) == 0) {
      NA
    } else { 
      NEONproductinfo_AOP()$siteCodes[[1]]$availableMonths[NEONproductinfo_AOP()$siteCodes[[1]]$siteCode %in% input$fieldsite_NEON_AOP][[1]]}
    selected <- input$download_dates_AOP_columns_selected
    year <- datesTable(dates = dates, process = "AOP", selected = selected)
    year
  })
  # Calculating Size
  observeEvent(eventExpr = input$get_AOP_size,
               handlerExpr = {
                 if (is_AOP() != "YES") {
                   sendSweetAlert(session, title = "Calculation failed", text = "Please choose an AOP product.", type = 'error')
                 } else if (length(Year_AOP()) == 0) {
                   sendSweetAlert(session, title = "Calculation failed", text = "Please choose a year.", type = 'error')
                 } else {
                   data_test <- try(nneo_data(product_code = Product_ID_AOP(), site_code = Field_Site_AOP(), year_month = paste0(Year_AOP(), "-01")))
                   if (class(data_test) == "try-error") {
                     sendSweetAlert(session, title = "Calculation failed", text = paste0("The product/site/year-month combination that you tried to calculate size for was invalid. Read the error message: ", strsplit(data_test, ":")[[1]][2]), type = 'error')
                   } else {
                     total_size <- 0
                     withProgress(message = "Calculation in progress", value = 0, expr = {
                       for (i in c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")) {
                         data <- try(nneo_data(product_code = Product_ID_AOP(), site_code = Field_Site_AOP(), year_month = paste0(Year_AOP(), "-", i))$data$files)
                         incProgress(amount = 1/12)
                         if (class(data) != "try-error") {
                           size <- as.numeric(data$size)
                           total_size <- total_size + sum(size)
                         }
                       }
                     })
                     if (total_size < 10^9 & total_size != 0) {
                       size_mb <- total_size * 10^-6
                       total_size <- paste0(as.character(size_mb), " MB")
                     } else if (total_size > 10^9) {
                       size_gb <- total_size * 10^-9
                       total_size <- paste0(as.character(size_gb), " GB")
                     } else if (total_size == 0) {
                       total_size <- "No data available"
                     }
                     output$AOP_size <- renderPrint(total_size)
                   }
                 }
               })
  # Download
  observeEvent(eventExpr = input$download_NEON_AOP,
               handlerExpr = {
                 if (is_AOP() != "YES") {
                   sendSweetAlert(session, title = "Download failed", text = "Please choose an AOP product.", type = 'error')
                 } else if (length(Year_AOP()) == 0) {
                   sendSweetAlert(session, title = "Download failed", text = "Please choose a year.", type = 'error')
                 } else {
                   disable(id = "download_NEON_AOP")
                   folder <- unique_folderpath(pathname = Folder_path_AOP())
                   withProgress(message = "Downloading files", value = 0, max = 1.1, expr = {
                     download <- try(byFileAOP(dpID = Product_ID_AOP(), site = Field_Site_AOP(), year = Year_AOP(), check.size = FALSE, savepath = "../NEON Downloads/"))
                   })
                   if (class(download) == "try-error") {
                     sendSweetAlert(session, title = "Download failed", text = paste0("This could be due to a faulty request or a problem with the product itself. Read the error code message: ", strsplit(download, ":")[[1]][-1]), type = 'error')
                     enable(id = "download_NEON_AOP")
                     unlink(paste0("../NEON Downloads/", Product_ID_AOP()), recursive = TRUE)
                   } else {
                     file.rename(from = paste0("../NEON Downloads/", Product_ID_AOP()), to = paste0("../NEON Downloads/", folder))
                     size <- sum(file.info(list.files(paste0("../NEON Downloads/", folder), all.files = TRUE, recursive = TRUE, full.names = T))$size) 
                     write_downloadSummary(method = "AOP", dpID = Product_ID_AOP(), dpName = NEONproducts_product$productName[NEONproducts_product$productCode == Product_ID_AOP()], site = Field_Site_AOP(), dates = Year_AOP(), package = "NA", size = utils:::format.object_size(size, units = "auto"), path = paste0("NEON Downloads/", folder))
                     enable(id = "download_NEON_AOP")
                     sendSweetAlert(session, title = "Download Complete", text = paste0("Check the 'NEON Download' directory for a folder titled ", folder, "."), type = 'success')
                   }
                 }
               })
  
  ####Tutorial####
  
  ####— Welcome tab####
  observeEvent(input$help_NEON, handlerExpr = {
    updateNavlistPanel(session, inputId = "tutorial", selected = "What is NEON?")
  })
  observeEvent(input$help_map, handlerExpr = {
    updateNavlistPanel(session, inputId = "tutorial", selected = "Using the interactive map")
  })
  observeEvent(input$help_browse, handlerExpr = {
    updateNavlistPanel(session, inputId = "tutorial", selected = "Finding data products")
  })
  observeEvent(input$help_download, handlerExpr = {
    updateNavlistPanel(session, inputId = "tutorial", selected = "Downloading data products")
  })
  ####— NEON tab####
  observeEvent(input$about_NEON, handlerExpr = {
    updateNavbarPage(session, inputId = "main", selected = "About This Project")
    updateNavlistPanel(session, inputId = "about", selected = "About NEON")
  })
  observeEvent(input$help_map2, handlerExpr = {
    updateNavlistPanel(session, inputId = "tutorial", selected = "Using the interactive map")
  })
  observeEvent(input$help_browse2, handlerExpr = {
    updateNavlistPanel(session, inputId = "tutorial", selected = "Finding data products")
  })
  observeEvent(input$help_download2, handlerExpr = {
    updateNavlistPanel(session, inputId = "tutorial", selected = "Downloading data products")
  })
  ####— Map tab####
  observeEvent(input$help_tutorial_dropdown, handlerExpr = {
    updateNavlistPanel(session, inputId = "main", selected = "Map Browser")
    confirmSweetAlert(session, inputId = "alert_dropdown", title = "Dropdown Tutorial", text = "When you click continue, the map will zoom towards the Santa Rita Experimental Range (SRER) and the dropdown will open. Click on some map features and the dropdown buttons and see what happens.", btn_labels = c("Cancel", "Continue"))
  })
  observeEvent(input$alert_dropdown, handlerExpr = {
    if (input$alert_dropdown == TRUE) {
      leafletProxy("map") %>% flyTo(lng = -110.8355, lat = 31.91068, zoom = 10)
      toggleDropdownButton(inputId = "map_dropdown")
      updateSelectInput(session, inputId = "NEONsite_dropdown", selected = "SRER")
    }
  })
  observeEvent(input$help_tutorial_map_manager, handlerExpr = {
    updateNavlistPanel(session, inputId = "main", selected = "Map Browser")
    confirmSweetAlert(session, inputId = "alert_map_manager", title = "Map Manager Tutorial", text = "When you click continue, the map will reset over the US and the map manager will appear. Play around with the various layers to see how the map is affected.", btn_labels = c("Cancel", "Continue"))
  })
  observeEvent(input$alert_map_manager, handlerExpr = {
    if (input$alert_map_manager == TRUE) {
      leafletProxy("map") %>% flyTo(lng = -98.5795, lat = 39.8283, zoom = 2.5)
      updateTabsetPanel(session, inputId = "main_data", selected = "filter")
      leafletProxy("map") %>% showGroup(legend$group)
    }
  })
  observeEvent(input$tutorial_help, handlerExpr = {
    if (input$tutorial_help == '<i class="fa fa-forward"></i>') {
      updateTabsetPanel(session, inputId = "tutorial_help", selected = "layers")
      updateNavlistPanel(session, inputId = "tutorial", selected = "Finding data products")
    }
  })
  ####— Browse Tab####
  observeEvent(input$help_tutorial_browse_site, handlerExpr = {
    updateNavlistPanel(session, inputId = "main", selected = "Map Browser")
    confirmSweetAlert(session, inputId = "alert_browse_site", title = "Browsing by Site Tutorial", text = "When you click continue, the product catalog will appear with the Santa Rita Experimental Range (SRER) selected and the filters open. Try filtering products, opening the table full screen, and then clicking cells in the table and see what happens.", btn_labels = c("Cancel", "Continue"))
  })
  observeEvent(input$alert_browse_site, handlerExpr = {
    if (input$alert_browse_site == TRUE) {
      updateTabsetPanel(session, inputId = "main_data", selected = "data")
      updateTabsetPanel(session, inputId = "data", selected = "find")
      updateAwesomeRadio(session, inputId = "NEON_browsing_type", selected = "site")
      updateAwesomeRadio(session, inputId = "NEONbrowsingstep_site", selected = "list")
      updateSelectInput(session, inputId = "NEONsite_site", selected = "SRER")
      toggleDropdownButton(inputId = "filter_site")
    }
  })
  observeEvent(input$help_tutorial_browse_product, handlerExpr = {
    updateNavlistPanel(session, inputId = "main", selected = "Map Browser")
    confirmSweetAlert(session, inputId = "alert_browse_product", title = "Browsing by Product Tutorial", text = "When you click continue, the product catalog will appear with all 180 products listed and the filters open. Try filtering products, opening the table full screen, and then clicking cells in the table and see what happens.", btn_labels = c("Cancel", "Continue"))
  })
  observeEvent(input$alert_browse_product, handlerExpr = {
    if (input$alert_browse_product == TRUE) {
      updateTabsetPanel(session, inputId = "main_data", selected = "data")
      updateTabsetPanel(session, inputId = "data", selected = "find")
      updateAwesomeRadio(session, inputId = "NEON_browsing_type", selected = "product")
      updateAwesomeRadio(session, inputId = "NEONbrowsingstep_product", selected = "list")
      toggleDropdownButton(inputId = "filter_product")
    }
  })
  observeEvent(input$help_tutorial_browse_details, handlerExpr = {
    updateNavlistPanel(session, inputId = "main", selected = "Map Browser")
    confirmSweetAlert(session, inputId = "alert_browse_details", title = "Viewing Product Details Tutorial", text = "When you click continue, the product catalog will open with the product 'Elevation of groundwater' already selected. Look through the product details and experiment with some of the buttons.", btn_labels = c("Cancel", "Continue"))
  })
  observeEvent(input$alert_browse_details, handlerExpr = {
    if (input$alert_browse_details == TRUE) {
      updateTabsetPanel(session, inputId = "main_data", selected = "data")
      updateTabsetPanel(session, inputId = "data", selected = "find")
      updateAwesomeRadio(session, inputId = "NEON_browsing_type", selected = "product")
      updateAwesomeRadio(session, inputId = "NEONbrowsingstep_product", selected = "single")
      updateTextInput(session, inputId = "NEONproductID_product", value = "DP1.20100.001")
    }
  })
  observeEvent(input$tutorial_browse, handlerExpr = {
    if (input$tutorial_browse == '<i class="fa fa-forward"></i>') {
      updateTabsetPanel(session, inputId = "tutorial_browse", selected = "site")
      updateNavlistPanel(session, inputId = "tutorial", selected = "Downloading data products")
    }
  })
  ####— Download Tab####
  observeEvent(input$tutorial_download, handlerExpr = {
    if (input$tutorial_download == '<i class="fa fa-forward"></i>') {
      confirmSweetAlert(session, inputId = "alert_done", title = "Tutorial Finished", text = "You made it through the tutorial! Press 'continue' to go and try everything out. We look foward to hearing your reactions and feedback!", btn_labels = c("Cancel", "Continue"))
    }
  })
  observeEvent(input$alert_done, handlerExpr = {
    if (input$alert_done == TRUE) {
      updateTabsetPanel(session, inputId = "tutorial_download", selected = "general")
      updateNavlistPanel(session, inputId = "tutorial", selected = "Introduction to the app")
      updateNavlistPanel(session, inputId = "main", selected = "Map Browser")
    }
  })
  
  ####FOR ME TAB####
  
  #Text for troublshooting
  #output$text_me <- renderText(as.character(input$map_marker_click))
  #Text for troublshooting 2
  #output$text_me_two <- renderText("Sub Locations" %in% as.character(input$map_marker_click))
  #Table for troubleshooting
  #output$table_me <- shiny::renderDataTable()
}