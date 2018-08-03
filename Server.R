# Shiny server
function(input, output, session) {
  
  ####INTERACTIVE MAP TAB####
  
  # Reactive value for layer control
  legend <- reactiveValues(group = c("Field Sites", "Domains", "Flightpaths", "TOS"))
  
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
                                           filter(domainCode %in% Domain_IDs()))
  Field_sites_poly_filtered <- reactive(FieldSite_poly %>% filter(siteType %in% input$fieldsite_type) %>%
                                          filter(domainCode %in% Domain_IDs()))
  Domain_included <- reactive(domain_data %>% filter(DomainName %in% input$fieldsite_domain))
  Domain_unincluded <- reactive(domain_data %>% filter(!(DomainName %in% input$fieldsite_domain)))
  TOS_data_filtered <- reactive(TOS_data %>% filter(siteType %in% input$fieldsite_type) %>%
                                  filter(domanID %in% Domain_IDs()))
  Flight_data_filtered <- reactive(flight_data %>% filter(SiteType %in% input$fieldsite_type) %>%
                                     filter(DomainID %in% Domain_IDs()) %>% filter(Year %in% input$flightpath_year))
  #### —— Plot Domains #### 
  observe({
    proxy <- leafletProxy("map")
    proxy %>%
      clearGroup(group = "Domains") %>%
      addPolygons(data = Domain_unincluded(),
                  weight = 2,
                  fillOpacity = '0.3',
                  group = "Domains",
                  popup = paste0(Domain_unincluded()$DomainName),
                  color = "gray") %>%
      addPolygons(data = Domain_included(),
                  weight = 2,
                  fillOpacity = '0.3',
                  group = "Domains",
                  popup = paste0(Domain_included()$DomainName),
                  color = "blue")
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
                                  Field_sites_point_filtered()$siteType),
                   clusterOptions = markerClusterOptions(),
                   label = paste0(Field_sites_point_filtered()$siteDescription),
                   icon = NEON_icon
        )
    }
  })
  # Boundaries
  observe({
    proxy <- leafletProxy("map")
    proxy %>% removeShape(layerId = unique(FieldSite_poly$siteCode))
    if (nrow(Field_sites_poly_filtered()) == 0) {
      proxy %>% clearGroup(group = "Field Sites")
    } else {
      for (i in 1:length(Field_sites_poly_filtered()$coordinates)) {
        if (is.array(Field_sites_poly_filtered()$coordinates[[i]])) {
          proxy %>%
            addPolygons(lng = Field_sites_poly_filtered()$coordinates[[i]][1,,1],
                        lat = Field_sites_poly_filtered()$coordinates[[i]][1,,2],
                        group = "Field Sites",
                        color = "green",
                        layerId = Field_sites_poly_filtered()$siteCode[i],
                        popup = paste0("Boundaries for ",
                                       Field_sites_poly_filtered()$siteDescription[i]),
                        fillOpacity = 0.4
            )
        } else if (is.list(Field_sites_poly_filtered()$coordinates[[i]])) {
          proxy %>%
            addPolygons(lng = Field_sites_poly_filtered()$coordinates[[i]][[1]][,1],
                        lat = Field_sites_poly_filtered()$coordinates[[i]][[1]][,2],
                        group = "Field Sites",
                        color = "green",
                        layerId = Field_sites_poly_filtered()$siteCode[i],
                        popup = paste0("Boundaries for ",
                                       Field_sites_poly_filtered()$siteDescription[i]),
                        fillOpacity = 0.4
            )
        }
      }
    }
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
                    fillOpacity = 0.2
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
                   popup = paste0("<b>Site: </b>",
                                  TOS_data_filtered()$siteID,
                                  "<br><b>Plot ID: </b>",
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
  # Hide TOS when launching app (TOS can make computer slow)
  leafletProxy("map") %>% hideGroup("TOS")
  
  
  #### NEON ####
  
  ####— NEON: Step 1- Find data ####
  ####—— 1a: By Site####
  # Variables
  NEONproducts_site <- reactive(nneo_site(x = input$NEONsite_site)$dataProducts)
  NEONproducts_product <- nneo_products() # Added this variable up here because one item in finding by "site" needed it
  # list: getting data frame of availability based on site code
  NEONproductlist_site <- reactive(as.data.frame(cbind("Product Name" = NEONproducts_site()$dataProductTitle, "Product ID" = NEONproducts_site()$dataProductCode))[order(NEONproducts_site()$dataProductTitle),])
  # single: filtering column of products for one site through ID
  NEONproductID_site <- reactive(req(
    if (gsub(pattern = " ", replacement = "", x = input$NEONproductID_site) == "") {
      "random string that will not match to anything"
    } else {
      gsub(pattern = " ", replacement = "", x = input$NEONproductID_site)
    }
  ))
  NEONproductinfo_site <- reactive(req(filter(.data = NEONproducts_site(), dataProductCode == NEONproductID_site())))
  # Display products: list
  output$NEONproductoptions_site <- renderDataTable(NEONproductlist_site(), options = list(autoWidth = TRUE))
  # Display products: single
  observeEvent(input$zoomtosite,
               leafletProxy("map") %>% flyTo(lng = FieldSite_point$siteLongitude[FieldSite_point$siteCode %in% input$NEONsite],
                                             lat = FieldSite_point$siteLatitude[FieldSite_point$siteCode %in% input$NEONsite],
                                             zoom = 10)
  )
  output$NEONproductname_site <- renderPrint(req(NEONproductinfo_site()$dataProductTitle))
  output$NEONproductdesc_site <- renderPrint(req(ifelse(is.null(req(NEONproductinfo_site()$dataProductTitle)),
                                                        yes = NULL,
                                                        no = NEONproducts_product$productDescription[NEONproducts_product$productCode %in% NEONproductID_site()]
  )))
  output$NEONproductdesign_site <- renderPrint(req(ifelse(is.null(req(NEONproductinfo_site()$dataProductTitle)),
                                                          yes = NULL,
                                                          no = NEONproducts_product$productDesignDescription[NEONproducts_product$productCode %in% NEONproductID_site()])))
  output$NEONproductnotes_site <- renderPrint(req(ifelse(is.null(req(NEONproductinfo_site()$dataProductTitle)),
                                                         yes = NULL,
                                                         no = NEONproducts_product$productRemarks[NEONproducts_product$productCode %in% NEONproductID_site()])))
  output$NEONproductdates_site <- renderPrint({
    dates <- if (length(NEONproductinfo_site()$availableMonths) == 0) {
      NA
    } else {
      NEONproductinfo_site()$availableMonths[[1]]}
    req(dates)
  })
  output$NEONproductURL_site <- renderPrint({
    urls <- if (length(NEONproductinfo_site()$availableDataUrl) == 0) {
      NA 
    } else {
      NEONproductinfo_site()$availableDataUrl[[1]]}
    req(urls)
  })
  
  ####—— 1b: By Product:####
  # Variables
  # NEONproducts_product <- nneo_products()
  # list: getting data table with products and IDs
  NEONproductlist_product <- NEONproducts_product[c("productName", "productCode")]
  names(NEONproductlist_product) <- c('Product Name', 'Product ID')
  # single: filtering one column of parent NEON products table through ID
  NEONproductID_product <- reactive(req(
    ifelse(gsub(pattern = " ", replacement = "", x = input$NEONproductID_product) == "",
           yes = "random string that will not match to anything",
           no = gsub(pattern = " ", replacement = "", x = input$NEONproductID_product))
  ))
  NEONproductinfo_product <- reactive(req(filter(.data = NEONproducts_product, productCode == NEONproductID_product())))
  # Display products: list
  output$NEON_product_options <- renderDataTable(NEONproductlist_product)
  # Display products: single
  output$NEONproductname_product <- renderPrint(req(NEONproductinfo_product()$productName))
  output$NEONproductdesc_product <- renderPrint(req(NEONproductinfo_product()$productDescription))
  output$NEONproductdesign_product <- renderPrint(req(NEONproductinfo_product()$productDesignDescription))
  output$NEONproductnotes_product <- renderPrint(req(NEONproductinfo_product()$productRemarks))
  output$ui_product<- renderUI({
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
  # Variables
  Product_ID_general <- reactive(req(gsub(pattern = " ", replacement = "", x = input$dpID_general)))
  Product_ID_specific <- reactive(req(gsub(pattern = " ", replacement = "", x = input$dpID_specific)))
  Field_Site_general <- reactive(req(
    if (input$location_NEON_general == "All (default)") {
      "all"
    } else {
      input$location_NEON_general
    })
  )
  Field_Site_specific <- reactive(req(input$location_NEON_specific))
  Package_type_general <- reactive(req(input$package_type_general))
  Package_type_specific <- reactive(req(input$package_type_specific))
  Date_specific_long <- reactive(req(as.character(input$date_NEON)))
  Date_specific_parts <- reactive(req(strsplit(Date_specific_long(), "-")[[1]]))
  Date_specific <- reactive(req(paste0(Date_specific_parts()[1], "-", Date_specific_parts()[2])))
  Folder_path_specific <- reactive(paste0("../NEON_", Field_Site_specific(), "_", Date_specific()))
  # Download NEON data: general
  observeEvent(input$download_NEON_general,
               zipsByProduct(dpID = Product_ID_general(), site = Field_Site_general(), package = Package_type_general(), check.size = FALSE, savepath = '..') &
                 sendSweetAlert(session, title = "File downloaded", text = "Check the directory containing 'Calliope View'. Go to step 2 to unzip files and make them more accesible.", type = 'success')
  )
  # Download NEON data: specific — creates a folder and adds files to folder
  observeEvent(input$download_NEON_specific,
               dir.create(path = Folder_path_specific()) &
                 getPackage(dpID = Product_ID_specific(), site_code = Field_Site_specific(), year_month = Date_specific(), package = Package_type_specific(), savepath = Folder_path_specific()) &
                 sendSweetAlert(session, title = "File downloaded", text = "Check the directory containing 'Calliope View'. Go to step 2 to unzip files and make them more accesible.", type = 'success')
  )
  
  ####— NEON: Step 3- Unzip/Join Downloads####
  # Variables
  NEON_folder_path <- reactive(req(readDirectoryInput(session, 'NEON_unzip_folder')))
  NEON_file_name <- reactive(req(input$NEON_unzip_file))
  NEON_file_path <- reactive(req(paste0("../", NEON_file_name())))
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
    unique(list.files(path = '..', pattern = ".zip"))
  }
  get.files <- function() {
    list.files(path = '..', pattern = ".zip")
  }
  NEON_unzip_files <- reactivePoll(intervalMillis = 10, session, checkFunc = has.new.files, valueFunc = get.files)
  observeEvent(NEON_unzip_files(), ignoreInit = TRUE, ignoreNULL = TRUE, {
    updateSelectInput(session, inputId = 'NEON_unzip_file', choices = NEON_unzip_files())
  })
  # Unzip data: general/specific
  observeEvent(input$unzip_NEON_folder,
               stackByTable(filepath = NEON_folder_path(), folder = TRUE) &
                 sendSweetAlert(session, title = "File unzipped", text = "The outer appearance of the folder should be the same. On the inside, there should be a new folder called 'stackedFiles' which contains the datasets.", type = "success")
  )
  # Unzip data: manual
  observeEvent(input$unzip_NEON_file,
               stackByTable(filepath = NEON_file_path(), folder = FALSE) &
                 sendSweetAlert(session, title = "File unzipped", text = paste0("There should now be a new folder titled '", strsplit(NEON_file_name(), ".zip")[[1]][1], "' with all of the datasets."), type = "success")
  )
  
  ####FOR ME TAB####
  
  #Text for troublshooting
  output$text_me <- renderText("The current working directory is:")
  #Text for troublshooting 2
  output$text_me_two <- renderText(getwd())
  #Table for troubleshooting
  #output$table_me <- renderDataTable()
}