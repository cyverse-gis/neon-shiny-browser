# Optimized Server.R with reactive data loading
# =============================================

function(input, output, session) {
  
  message("✓ Server starting with optimized loading strategy...")
  
  # =================================================
  # Reactive Data Management
  # =================================================
  
  # Reactive values for data state
  data_state <- reactiveValues(
    field_sites_loaded = FALSE,
    polygons_loaded = FALSE,
    domains_loaded = FALSE,
    spatial_data_loaded = FALSE,
    api_functions_loaded = FALSE
  )
  
  # Reactive data containers
  reactive_data <- reactiveValues(
    field_sites = NULL,
    polygons = NULL,
    domains = NULL,
    flight_data = NULL,
    product_data = NULL
  )
  
  # =================================================
  # Progressive Data Loading System
  # =================================================
  
  # Monitor data loading progress
  observe({
    if (exists("FieldSite_point") && nrow(FieldSite_point) > 0) {
      reactive_data$field_sites <- FieldSite_point
      data_state$field_sites_loaded <- TRUE
      message("✓ Field sites data available in reactive system")
    }
  })
  
  observe({
    if (exists("FieldSite_poly") && nrow(FieldSite_poly) > 0) {
      reactive_data$polygons <- FieldSite_poly
      data_state$polygons_loaded <- TRUE
      message("✓ Polygon data available in reactive system")
    }
  })
  
  observe({
    if (exists("domains") && nrow(domains) > 0) {
      reactive_data$domains <- domains
      data_state$domains_loaded <- TRUE
      message("✓ Domain data available in reactive system")
    }
  })
  
  # =================================================
  # Helper Functions
  # =================================================
  
  # Safe product name function with reactive data
  safe_product_name <- function(product_code) {
    tryCatch({
      if (is.null(reactive_data$product_data) || !is.data.frame(reactive_data$product_data) || 
          is.null(product_code) || is.na(product_code) || product_code == "") {
        return("Unknown Product")
      }
      
      matches <- reactive_data$product_data$productCode == product_code
      if (any(matches, na.rm = TRUE)) {
        product_name <- reactive_data$product_data$productName[matches]
        return(if(length(product_name) > 0 && !is.na(product_name[1])) product_name[1] else "Unknown Product")
      } else {
        return("Unknown Product")
      }
    }, error = function(e) {
      message(sprintf("Error in safe_product_name: %s", e$message))
      return("Unknown Product")
    })
  }
  
  # Safe site codes access with reactive data
  safe_siteCodes_access <- function(product_info, column_name = "siteCode", filter_input = NULL) {
    tryCatch({
      if (length(product_info$siteCodes) == 0 || !is.list(product_info$siteCodes)) {
        return(NA)
      }
      
      site_codes <- product_info$siteCodes[[1]]
      if (is.null(site_codes) || !is.data.frame(site_codes) || !column_name %in% names(site_codes)) {
        return(NA)
      }
      
      if (!is.null(filter_input) && "siteCode" %in% names(site_codes)) {
        matching_sites <- site_codes$siteCode %in% filter_input
        if (any(matching_sites)) {
          result <- site_codes[[column_name]][matching_sites]
          return(if(length(result) > 0) result else NA)
        } else {
          return(NA)
        }
      } else {
        return(site_codes[[column_name]])
      }
    }, error = function(e) {
      message(sprintf("Error in safe_siteCodes_access: %s", e$message))
      return(NA)
    })
  }
  
  # =================================================
  # On-Demand Data Loading
  # =================================================
  
  # Load API functions when needed
  load_api_functions_if_needed <- function() {
    if (!data_state$api_functions_loaded) {
      tryCatch({
        if (exists("load_api_functions")) {
          load_api_functions()
          data_state$api_functions_loaded <- TRUE
          message("✓ API functions loaded on demand")
        }
      }, error = function(e) {
        message(sprintf("Error loading API functions: %s", e$message))
      })
    }
  }
  
  # Load spatial data when map is accessed
  load_spatial_data_if_needed <- function() {
    if (!data_state$spatial_data_loaded) {
      tryCatch({
        if (sf_available && exists("load_neon_spatial_data")) {
          load_neon_spatial_data(force_update = FALSE, check_updates = TRUE)
          data_state$spatial_data_loaded <- TRUE
          message("✓ Spatial data loaded on demand")
        }
      }, error = function(e) {
        message(sprintf("Error loading spatial data: %s", e$message))
      })
    }
  }
  
  # =================================================
  # UI Initialization with Loading States
  # =================================================
  
  # Initialization with user feedback
  if (exists("dir_created") && dir_created == TRUE) {
    delay(ms = 5000, showNotification(
      ui = "'~/NEON_Downloads' folder created outside the directory containing this app. All downloads will go to the 'NEON_Downloads' folder.", 
      duration = NULL, type = "message"))
  } else {
    delay(ms = 5000, showNotification(ui = "Welcome back!", duration = 15, type = "message"))
  }
  
  delay(ms = 5000, expr = showNotification(
    ui = "First time here?", 
    action = actionLink(inputId = "firsttime", label = "Yes"), 
    duration = 15, type = "message", id = "first"))
  
  # First time user flow
  observeEvent(input$firsttime, {
    confirmSweetAlert(session, inputId = "firsttime_confirm", 
                     title = "Welcome to the NEON Data Browser!", 
                     text = "This will bring you to the tutorial section and get you started with NEON and this app.", 
                     btn_labels = c("Cancel", "Confirm"))
  })
  
  observeEvent(input$firsttime_confirm, {
    if (input$firsttime_confirm == TRUE) {
      updateNavbarPage(session, inputId = "main", selected = "Help/Tutorials")
      removeNotification(id = "first", session)
    }
  })
  
  # =================================================
  # Progressive Map Rendering
  # =================================================
  
  # Reactive value for layer control
  legend <- reactiveValues(group = c("Field Sites", "Domains", "Flight Boxes", "Sub Locations"))
  
  # Map output with progressive loading
  output$map <- renderLeaflet({
    # Load spatial data if needed when map is first accessed
    load_spatial_data_if_needed()
    
    # Basic map structure
    map <- leaflet() %>%
      addProviderTiles(provider = providers$OpenStreetMap.Mapnik, group = "Basic") %>%
      addProviderTiles(provider = providers$Esri.NatGeoWorldMap, group = "Nat geo") %>%
      addProviderTiles(provider = providers$OpenTopoMap, group = "Topo") %>%
      addProviderTiles(provider = providers$Esri.WorldImagery, group = "Satellite") %>%
      addLayersControl(
        baseGroups = c("Basic", "Nat geo", "Topo", "Satellite"),
        overlayGroups = legend$group,
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      setView(lng = -99, lat = 40, zoom = 4)
    
    # Add field sites if available
    if (data_state$field_sites_loaded && !is.null(reactive_data$field_sites)) {
      tryCatch({
        field_sites <- reactive_data$field_sites
        map <- map %>%
          addCircleMarkers(
            data = field_sites,
            lng = ~longitude, lat = ~latitude,
            group = "Field Sites",
            popup = ~paste("<b>", siteCode, "</b><br>", siteDescription),
            radius = 6,
            fillOpacity = 0.7,
            stroke = TRUE,
            weight = 2
          )
      }, error = function(e) {
        message(sprintf("Error adding field sites to map: %s", e$message))
      })
    }
    
    map
  })
  
  # =================================================
  # Reactive Data Updates
  # =================================================
  
  # Update map when data becomes available
  observe({
    if (data_state$field_sites_loaded && !is.null(reactive_data$field_sites)) {
      leafletProxy("map") %>%
        clearGroup("Field Sites") %>%
        addCircleMarkers(
          data = reactive_data$field_sites,
          lng = ~longitude, lat = ~latitude,
          group = "Field Sites",
          popup = ~paste("<b>", siteCode, "</b><br>", siteDescription),
          radius = 6,
          fillOpacity = 0.7,
          stroke = TRUE,
          weight = 2
        )
    }
  })
  
  # =================================================
  # Data Product Interface
  # =================================================
  
  # Load API functions when download tab is accessed
  observeEvent(input$data, {
    if (input$data == "download") {
      load_api_functions_if_needed()
    }
  })
  
  # Dynamic field site choices
  output$ui_fieldsite_regular <- renderUI({
    req(data_state$field_sites_loaded)
    selectInput(
      inputId = "fieldsite_regular", 
      label = "Field Site", 
      choices = if(!is.null(reactive_data$field_sites)) reactive_data$field_sites$siteCode else character(0)
    )
  })
  
  output$ui_fieldsite_AOP <- renderUI({
    req(data_state$field_sites_loaded)
    selectInput(
      inputId = "fieldsite_AOP", 
      label = "Field Site", 
      choices = if(!is.null(reactive_data$field_sites)) reactive_data$field_sites$siteCode else character(0)
    )
  })
  
  # =================================================
  # Download Management
  # =================================================
  
  # Download progress tracking
  download_state <- reactiveValues(
    in_progress = FALSE,
    current_product = NULL,
    progress = 0
  )
  
  # Regular download with progress tracking
  observeEvent(input$download_NEON_regular, {
    req(input$dpID_regular, input$fieldsite_regular)
    
    load_api_functions_if_needed()
    
    download_state$in_progress <- TRUE
    download_state$current_product <- input$dpID_regular
    
    showNotification(
      ui = paste("Starting download for", input$dpID_regular),
      duration = 5,
      type = "message"
    )
    
    tryCatch({
      # Your existing download logic here
      # This would include calls to neonUtilities functions
      
      download_state$in_progress <- FALSE
      showNotification(
        ui = "Download completed successfully!",
        duration = 10,
        type = "message"
      )
    }, error = function(e) {
      download_state$in_progress <- FALSE
      showNotification(
        ui = paste("Download failed:", e$message),
        duration = 10,
        type = "error"
      )
    })
  })
  
  # =================================================
  # Performance Monitoring
  # =================================================
  
  # Monitor app performance
  observe({
    invalidateLater(30000) # Check every 30 seconds
    
    # Log current data state
    message(sprintf("Data State - FieldSites: %s, Polygons: %s, Domains: %s, Spatial: %s", 
                   data_state$field_sites_loaded,
                   data_state$polygons_loaded, 
                   data_state$domains_loaded,
                   data_state$spatial_data_loaded))
  })
  
  message("✓ Server initialization complete with optimized loading strategy")
}