# UI Components for Spatial Data Management

#' Create UI for spatial data management
spatial_data_management_UI <- function() {
  tagList(
    h4("Spatial Data Management"),
    p("NEON spatial datasets are automatically cached and updated. Use the controls below to check status or force updates."),
    
    # Status display
    h5("Current Data Status"),
    DTOutput("spatial_data_status_table"),
    br(),
    
    # Update controls
    fluidRow(
      column(6, 
        h5("Update Individual Datasets"),
        selectInput("spatial_dataset_select", 
                   "Select Dataset:",
                   choices = list(
                     "Field Site Boundaries" = "field_boundaries",
                     "TOS Sampling Locations" = "tos_plots", 
                     "Domain Polygons" = "domain_polygons",
                     "Flight Boundaries" = "flight_boundaries",
                     "Aquatic Watersheds" = "aquatic_watersheds"
                   )),
        actionButton("update_single_dataset", "Update Selected Dataset", 
                    class = "btn-primary")
      ),
      column(6,
        h5("Update All Datasets"), 
        p("This will check all datasets for updates and download newer versions if available."),
        actionButton("update_all_datasets", "Update All Spatial Data",
                    class = "btn-warning"),
        br(), br(),
        actionButton("force_update_all", "Force Update All (Re-download)",
                    class = "btn-danger")
      )
    ),
    
    # Progress and messages
    br(),
    conditionalPanel(
      condition = "input.update_single_dataset > 0 || input.update_all_datasets > 0 || input.force_update_all > 0",
      h5("Update Progress"),
      verbatimTextOutput("spatial_update_messages")
    )
  )
}

#' Server logic for spatial data management
spatial_data_management_server <- function(input, output, session) {
  
  # Reactive values for update status
  update_messages <- reactiveVal("")
  
  # Display spatial data status table
  output$spatial_data_status_table <- renderDT({
    status <- get_spatial_data_status()
    
    # Convert to data frame for display
    status_df <- data.frame(
      Dataset = sapply(status, function(x) x$name),
      Cached = sapply(status, function(x) ifelse(x$cached, "✓", "✗")),
      `Last Updated` = sapply(status, function(x) x$download_date),
      `File Size` = sapply(status, function(x) {
        if (is.numeric(x$file_size)) {
          format(structure(x$file_size, class = "object_size"), units = "auto")
        } else {
          x$file_size
        }
      }),
      `Cache Location` = sapply(status, function(x) x$cache_dir),
      stringsAsFactors = FALSE
    )
    
    DT::datatable(status_df, 
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE)
  })
  
  # Update single dataset
  observeEvent(input$update_single_dataset, {
    dataset_name <- input$spatial_dataset_select
    
    update_messages(paste0("Updating ", NEON_SPATIAL_CONFIG[[dataset_name]]$name, "...\n"))
    
    # Run update in background (simulate async)
    tryCatch({
      result <- update_spatial_dataset(dataset_name)
      
      if (result[[dataset_name]]$success) {
        msg <- paste0(update_messages(), 
                     "✓ Successfully updated ", NEON_SPATIAL_CONFIG[[dataset_name]]$name, "\n")
      } else {
        msg <- paste0(update_messages(),
                     "✗ Failed to update ", NEON_SPATIAL_CONFIG[[dataset_name]]$name, 
                     ": ", result[[dataset_name]]$error, "\n")
      }
      update_messages(msg)
      
    }, error = function(e) {
      msg <- paste0(update_messages(),
                   "✗ Error updating ", NEON_SPATIAL_CONFIG[[dataset_name]]$name, 
                   ": ", e$message, "\n")
      update_messages(msg)
    })
    
    # Refresh status table
    output$spatial_data_status_table <- renderDT({
      status <- get_spatial_data_status()
      status_df <- data.frame(
        Dataset = sapply(status, function(x) x$name),
        Cached = sapply(status, function(x) ifelse(x$cached, "✓", "✗")),
        `Last Updated` = sapply(status, function(x) x$download_date),
        `File Size` = sapply(status, function(x) {
          if (is.numeric(x$file_size)) {
            format(structure(x$file_size, class = "object_size"), units = "auto")
          } else {
            x$file_size
          }
        }),
        `Cache Location` = sapply(status, function(x) x$cache_dir),
        stringsAsFactors = FALSE
      )
      DT::datatable(status_df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    })
  })
  
  # Update all datasets 
  observeEvent(input$update_all_datasets, {
    update_messages("Checking all spatial datasets for updates...\n")
    
    tryCatch({
      results <- update_all_spatial_data(force_update = FALSE)
      
      msg <- update_messages()
      for (dataset_name in names(results)) {
        result <- results[[dataset_name]]
        if (result$success) {
          msg <- paste0(msg, "✓ ", NEON_SPATIAL_CONFIG[[dataset_name]]$name, " is up to date\n")
        } else {
          msg <- paste0(msg, "✗ ", NEON_SPATIAL_CONFIG[[dataset_name]]$name, " failed: ", 
                       result$error, "\n")
        }
      }
      update_messages(msg)
      
      # Reload spatial data
      load_neon_spatial_data(force_update = FALSE, check_updates = FALSE)
      
    }, error = function(e) {
      msg <- paste0(update_messages(), "✗ Error during bulk update: ", e$message, "\n")
      update_messages(msg)
    })
  })
  
  # Force update all datasets
  observeEvent(input$force_update_all, {
    update_messages("Force updating all spatial datasets (this may take several minutes)...\n")
    
    tryCatch({
      results <- update_all_spatial_data(force_update = TRUE)
      
      msg <- update_messages()
      successful <- 0
      for (dataset_name in names(results)) {
        result <- results[[dataset_name]]
        if (result$success) {
          msg <- paste0(msg, "✓ ", NEON_SPATIAL_CONFIG[[dataset_name]]$name, " updated successfully\n")
          successful <- successful + 1
        } else {
          msg <- paste0(msg, "✗ ", NEON_SPATIAL_CONFIG[[dataset_name]]$name, " failed: ", 
                       result$error, "\n")
        }
      }
      msg <- paste0(msg, sprintf("\nUpdate complete: %d/%d datasets successful\n", 
                                successful, length(results)))
      update_messages(msg)
      
      # Reload all spatial data
      load_neon_spatial_data(force_update = FALSE, check_updates = FALSE)
      
    }, error = function(e) {
      msg <- paste0(update_messages(), "✗ Error during force update: ", e$message, "\n")
      update_messages(msg)
    })
  })
  
  # Display update messages
  output$spatial_update_messages <- renderText({
    update_messages()
  })
}