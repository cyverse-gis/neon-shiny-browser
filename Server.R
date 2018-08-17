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
  TOS_data_filtered <- reactive(TOS_data %>% filter(siteID %in% Field_sites_point_filtered()$siteCode))
  Flight_data_filtered <- reactive(flight_data %>% filter(SiteAbb %in% Field_sites_point_filtered()$siteCode) %>% filter(Year %in% input$flightpath_year))
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
                        color = "#49E2BD",
                        layerId = Field_sites_poly_filtered()$siteCode[i],
                        popup = paste0("Boundaries for ",
                                       Field_sites_poly_filtered()$siteDescription[i]),
                        opacity = 1,
                        fillOpacity = 0,
                        highlightOptions = highlightOptions(stroke = TRUE, color = "#39ff14", weight = 7, bringToFront = TRUE)
            )
        } else if (is.list(Field_sites_poly_filtered()$coordinates[[i]])) {
          proxy %>%
            addPolygons(lng = Field_sites_poly_filtered()$coordinates[[i]][[1]][,1],
                        lat = Field_sites_poly_filtered()$coordinates[[i]][[1]][,2],
                        group = "Field Sites",
                        color = "#49E2BD",
                        layerId = Field_sites_poly_filtered()$siteCode[i],
                        popup = paste0("Boundaries for ",
                                       Field_sites_poly_filtered()$siteDescription[i]),
                        opacity = 1,
                        fillOpacity = 0.4,
                        highlightOptions = highlightOptions(stroke = TRUE, color = "#39ff14", weight = 7, bringToFront = TRUE)
            )
        }
      }
    }
  })
  
  
  #### NEON ####
  observeEvent(input$zoomtosite,
               leafletProxy("map") %>% flyTo(lng = FieldSite_point$siteLongitude[FieldSite_point$siteCode %in% input$NEONsite_zoom],
                                             lat = FieldSite_point$siteLatitude[FieldSite_point$siteCode %in% input$NEONsite_zoom],
                                             zoom = 10)
  )
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
  output$dropdown_site <- renderPrint(FieldSite_point$siteName[FieldSite_point$siteCode %in% input$NEONsite_zoom])
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
  # Variables
  Product_ID_general <- reactive(req(gsub(pattern = " ", replacement = "", x = input$dpID_general)))
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
  Folder_path_specific <- reactive(paste0("../NEON_", Field_Site_specific(), "_", Date_specific()))
  # Download NEON data: general
  observeEvent(eventExpr = input$download_NEON_general,
               handlerExpr = {
                 showNotification(ui = "Download in progess…", id = "download_general", type = "message")
                 download <- try(zipsByProduct(dpID = Product_ID_general(), site = Field_Site_general(), package = Package_type_general(), check.size = FALSE, savepath = '..'), silent = TRUE)
                 if (class(download) == "try-error") {
                   removeNotification(id = "download_general")
                   sendSweetAlert(session, title = "Download failed", text = paste0("This could be due to the data package you tried to obtain or the neonUtlities package used to pull data. Read the error code message: ", strsplit(download, ":")[[1]][2]), type = 'error')
                 } else {
                   removeNotification(id = "download_general")
                   sendSweetAlert(session, title = "File downloaded", text = "Check the directory containing 'Calliope View'. Go to step 2 to unzip files and make them more accesible.", type = 'success')
                 }
               })
  # Download NEON data: specific — creates a folder and adds files to folder
  observeEvent(eventExpr = input$download_NEON_specific,
               handlerExpr = {
                 showNotification(ui = "Download in progess…", id = "download_specific", type = "message")
                 dir.create(path = Folder_path_specific())
                 download <- try(getPackage(dpID = Product_ID_specific(), site_code = Field_Site_specific(), year_month = Date_specific(), package = Package_type_specific(), savepath = Folder_path_specific()), silent = TRUE)
                 if (class(download) == "try-error") {
                   removeNotification(id = "download_specific")
                   sendSweetAlert(session, title = "Download failed", text = paste0("This could be due to the data package you tried to obtain or the neonUtlities package used to pull data. Read the error message: ", download), type = 'error')
                 } else {
                   removeNotification(id = "download_specific")
                   sendSweetAlert(session, title = "File downloaded", text = "Check the directory containing 'Calliope View'. Go to step 2 to unzip files and make them more accesible.", type = 'success')
                 }
  })
  # Download NEON data: AOP
  output$check_AOP <- renderPrint({
    product_table <- reactive(NEONproducts_product[NEONproducts_product$productCode == Product_ID_AOP(),])
    if (nrow(product_table()) != 0) {
      if (product_table()$productScienceTeamAbbr == "AOP") {
        "YES"
      } else {
        "NO"
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
                   sendSweetAlert(session, title = "File downloaded", text = "Check the directory containing 'Calliope View'. Go to step 2 to unzip files and make them more accesible.", type = 'success')
                 }
               })
  
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
  observeEvent(eventExpr = input$unzip_NEON_folder,
               handlerExpr = {
                 showNotification(ui = "Unzip in progess…", id = "unzip_normal", type = "message")
                 unzip <- try(stackByTable(filepath = NEON_folder_path(), folder = TRUE), silent = TRUE) 
                 if (class(unzip) == "try-error") {
                   removeNotification("unzip_normal")
                   sendSweetAlert(session, title = "Unzip failed", text = paste0("Check that you are unzipping the folder from part 2. Read the error code message: ", strsplit(unzip, ":")[[1]][2]), type = "error")
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
                   sendSweetAlert(session, title = "Unzip failed", text = paste0("Check that you are unzipping the .zip file that was manually downloaded. Read the error code message: ", strsplit(unzip, ":")[[1]][2]), type = "error")
                 } else {
                   removeNotification("unzip_manual")
                   sendSweetAlert(session, title = "File unzipped", text = paste0("There should now be a new folder titled '", strsplit(NEON_file_name(), ".zip")[[1]][1], "' with all of the datasets."), type = "success")
                 }
               })
  
  ####FOR ME TAB####
  
  #Text for troublshooting
  output$text_me <- renderText(strsplit(as.character(input$year_AOP), "-")[[1]][1])
  #Text for troublshooting 2
  output$text_me_two <- renderText(getwd())
  #Table for troubleshooting
  #output$table_me <- renderDataTable()
}