fluidPage(theme = shinytheme('cerulean'),
          title = ("NEON Data Browser"),
          useShinyjs(),
          navbarPage(tags$b("NEON Data Browser"), id ="main",
                     ####Tab 1: Includes the map, and key with features like filtering data####
                     tabPanel("Map Browser",
                              dropdownButton(tooltip = "Find an individual NEON Site and Zoom to its location", label = "Select a Site", circle = FALSE, right = FALSE, status = "primary", size = "xs", icon = icon("info-circle"), width = "38vw", inputId = "map_dropdown",
                                             selectInput(inputId = "NEONsite_dropdown", label = "Site info:", choices = FieldSite_abbs),
                                             tags$b("Name and type:"),
                                             verbatimTextOutput(outputId = "dropdown_site", placeholder = TRUE),
                                             tags$b("State:"),
                                             verbatimTextOutput(outputId = "dropdown_state",placeholder = TRUE),
                                             tags$b("Number of data products available:"),
                                             verbatimTextOutput(outputId = "dataproduct_number", placeholder = TRUE),
                                             actionButton(inputId = "addsublocs", label = "Add Sublocations"),
                                             actionButton(inputId = "zoomtosite", label = "Zoom to Site"),
                                             actionButton(inputId = "togglesite", label = "Toggle for Viewing and Download")),
                              sidebarLayout(
                                position = "left",
                                sidebarPanel(width = 5,
                                             tabsetPanel(id = "main_data",
                                               #### — NEON ####
                                               tabPanel(tags$h5("Browser"), value = "data",
                                                        tabsetPanel(id = "data",
                                                          #### —— STEP 1: Find Data####
                                                          tabPanel("Product Catalog", value = "find",
                                                                   # Simplified to product-only browsing - site-based browsing removed
                                                                   includeMarkdown('Rmd/NEON_browsing_product.Rmd'),
                                                                   awesomeRadio(inputId = "NEONbrowsingstep_product", label = NULL, choices = list("Browse Products" = "list", "Product Details" = "single"), inline = TRUE, status = "info"),
                                                                   conditionalPanel("input.NEONbrowsingstep_product == 'list'",
                                                                                    fluidRow(
                                                                                      column(8, 
                                                                                             tags$h4("NEON Data Products"),
                                                                                             tags$p("Explore NEON's ecological and environmental data products. Use the filters below to find products by topic, collection method, or theme.")
                                                                                      ),
                                                                                      column(4, tags$br(),
                                                                                             actionBttn(inputId = "expandtable_product", label = "Expand Table", style = "material-flat", icon = icon("expand"), color = "primary", size = "sm"))
                                                                                    ),
                                                                                    tags$br(),
                                                                                    fluidRow(
                                                                                      column(4, 
                                                                                             tags$b("Filter by Keywords"),
                                                                                             uiOutput(outputId = "ui_selectkeywords_product")
                                                                                      ),
                                                                                      column(4,
                                                                                             tags$b("Data Collection Method"),
                                                                                             selectInput(inputId = "selectproducttype_product", label = NULL, choices = NEON_datatypes, multiple = TRUE, )
                                                                                      ),
                                                                                      column(4,
                                                                                             tags$b("Theme"),
                                                                                             selectInput(inputId = "selectproducttheme_product", label = NULL, choices = c("Atmosphere", "Biogeochemistry", "Ecohydrology", "Land Use, Land Cover, and Land Processes", "Organisms, Populations, and Communities"), multiple = TRUE, )
                                                                                      )
                                                                                    ),
                                                                                    checkboxInput(inputId = "showfilterinfo_product", label = "Show filter help", value = FALSE),
                                                                                    tags$br(),
                                                                                    DTOutput(outputId = "NEONproductoptions_enhanced"),
                                                                                    bsModal(id = "tableexpand_product", title = "NEON Data Products - Expanded View", trigger = "expandtable_product", size = "large",
                                                                                            fluidRow(
                                                                                              column(4, 
                                                                                                     tags$b("Filter by Keywords"),
                                                                                                     uiOutput(outputId = "ui_selectkeywords_product2")
                                                                                              ),
                                                                                              column(4,
                                                                                                     tags$b("Data Collection Method"),
                                                                                                     selectInput(inputId = "selectproducttype_product2", label = NULL, choices = NEON_datatypes, multiple = TRUE, )
                                                                                              ),
                                                                                              column(4,
                                                                                                     tags$b("Theme"),
                                                                                                     selectInput(inputId = "selectproducttheme_product2", label = NULL, choices = c("Atmosphere", "Biogeochemistry", "Ecohydrology", "Land Use, Land Cover, and Land Processes", "Organisms, Populations, and Communities"), multiple = TRUE, )
                                                                                              )
                                                                                            ),
                                                                                            checkboxInput(inputId = "showfilterinfo_product2", label = "Show filter help", value = FALSE),
                                                                                            tags$br(),
                                                                                            DTOutput(outputId = "NEONproductoptions_enhanced2"))
                                                                                    ),
                                                                   conditionalPanel("input.NEONbrowsingstep_product == 'single'",
                                                                                    fluidRow(
                                                                                      column(8,
                                                                                             textInput(inputId = "NEONproductID_product", label = "Product ID:", placeholder = "e.g., DP1.00001.001")
                                                                                      ),
                                                                                      column(4,
                                                                                             tags$br(),
                                                                                             actionBttn(inputId = "toggledownload_product", label = "Download Data", color = "success", size = "sm", icon = icon("download"))
                                                                                      )
                                                                                    ),
                                                                                    htmlOutput(outputId = "nodates_download"),
                                                                                    tags$hr(),
                                                                                    tags$b("Product Name:"),
                                                                                    htmlOutput(outputId = "NEONproductname_product"),
                                                                                    tags$br(),
                                                                                    tags$b("Description:"),
                                                                                    htmlOutput(outputId = "NEONproductdesc_product"),
                                                                                    tags$br(),
                                                                                    tags$b("Available Sites:"),
                                                                                    uiOutput(outputId = "ui_selectsite"),
                                                                                    tags$br(),
                                                                                    tags$b("Additional Information:"),
                                                                                    HTML(paste0("<p style='border:1px; border-radius:5px; border-style:solid; border-color:#CCCCCC; padding: 0.5em;'>", actionLink(inputId = "moreproductinfo_product", label = HTML("<u>View detailed information</u>")), "</p>")),
                                                                                    bsModal(id = "moreinfo_product", title = "Detailed Product Information", trigger = "moreproductinfo_product", size = "large",
                                                                                            tags$b("Abstract:"),
                                                                                            htmlOutput(outputId = "NEONproductabstract_product"),
                                                                                            tags$br(),
                                                                                            tags$b("Product Design:"),
                                                                                            htmlOutput(outputId = "NEONproductdesign_product"),
                                                                                            tags$br(),
                                                                                            tags$b("Notes:"),
                                                                                            htmlOutput(outputId = "NEONproductnotes_product"),
                                                                                            tags$br(),
                                                                                            downloadButton(outputId = "fullinfo_product", label = "Download full product information", class = "btn-info")
                                                                                    ),
                                                                                    htmlOutput(outputId = "nodates_message"),
                                                                                    checkboxInput(inputId = "showURL_product", label = "Show available data URLs"),
                                                                                    conditionalPanel("input.showURL_product",
                                                                                                     tags$b("Available Data URLs:"),
                                                                                                     verbatimTextOutput("NEONproductURL_product", placeholder = TRUE)
                                                                                    )
                                                                   )
                                                                   )
                                                          ),
                                                          #### —— STEP 2: Download Data####
                                                          tabPanel("Product Download", value = "download",
                                                                   awesomeRadio(inputId = "NEON_download_type", label = tags$h5(""), choices = list("Regular" = "regular", "AOP" = "AOP"), inline = TRUE, checkbox = TRUE),
                                                                   conditionalPanel("input.NEON_download_type == 'regular'",
                                                                                    includeMarkdown('Rmd/NEON_download_regular.Rmd'),
                                                                                    fluidRow(
                                                                                      column(6, textInput(inputId = "dpID_regular", label = "Product ID")),
                                                                                      column(6, uiOutput(outputId = "ui_fieldsite_regular"))
                                                                                    ),
                                                                                    radioButtons(inputId = "target_download_regular", label = "Method of Selecting Dates", choices = list("Cell" = "cell", "Row" = "row", "Column" = "column"), inline = TRUE),
                                                                                    conditionalPanel("input.target_download_regular == 'cell'",
                                                                                                     radioButtons(inputId = "cellselection_download_regular", label = "Method of Cell Selection", choices = c("Start End", "All"), inline = TRUE)
                                                                                    ),
                                                                                    bsTooltip(id = "cellselection_download_regular", title = HTML("<b>Start End</b>, which needs only two dates, takes the first and last date selected and selects everything in between. <b>All</b> takes any number of dates, and selects all of them."), placement = "top"),
                                                                                    DTOutput(outputId = "download_dates_regular"),
                                                                                    checkboxInput(inputId = "extra_options_regular", label = "Show extra options"),
                                                                                    conditionalPanel("input.extra_options_regular",
                                                                                                     selectInput(inputId = "package_type_regular", label = "Package Type", choices = c("basic", "expanded"))),
                                                                                    passwordInput(inputId = "neon_api_token", label = "NEON API Token (optional)", 
                                                                                                  placeholder = "Enter your NEON API token for faster downloads"),
                                                                                    helpText("Get your API token from ", 
                                                                                            tags$a(href = "https://data.neonscience.org/myaccount", 
                                                                                                   target = "_blank", "your NEON account")),
                                                                                    checkboxInput(inputId = "regular_size", label = "Calculate download size"),
                                                                                    conditionalPanel("input.regular_size",
                                                                                                     actionLink(inputId = "get_regular_size", label = "Calculate size"),
                                                                                                     bsTooltip(id = "get_regular_size", title = "Note that this is an order of magnitude estimate. Due to the stacking process, your final zip file will deviate from this value.", placement = "left"),
                                                                                                     verbatimTextOutput(outputId = "regular_size", placeholder = TRUE)),
                                                                                    HTML("This download might take a while, depending on how many dates are included. You can estimate the size of the downloads above.<br>"),
                                                                                    actionButton(inputId = "download_NEON_regular", label = "Download Items", icon = icon(name = "download"))
                                                                   ),
                                                                   conditionalPanel("input.NEON_download_type == 'AOP'",
                                                                                    includeMarkdown('Rmd/NEON_download_AOP.Rmd'),
                                                                                    fluidRow(
                                                                                      column(6, textInput(inputId = "dpID_AOP", label = "Product ID")),
                                                                                      column(6, uiOutput(outputId = "ui_fieldsite_AOP"))
                                                                                    ),
                                                                                    tags$b("Is AOP?"),
                                                                                    verbatimTextOutput(outputId = "check_AOP", placeholder = TRUE),
                                                                                    DTOutput(outputId = "download_dates_AOP"),
                                                                                    checkboxInput(inputId = "AOP_size", label = "Calculate download size"),
                                                                                    conditionalPanel("input.AOP_size",
                                                                                                     actionLink(inputId = "get_AOP_size", label = "Calculate size"),
                                                                                                     verbatimTextOutput(outputId = "AOP_size", placeholder = TRUE)),
                                                                                    HTML("This download will take a long time.<br>"),
                                                                                    actionButton(inputId = "download_NEON_AOP", label = "Download items", icon = icon(name = "download")),
                                                                                    disabled(downloadLink(outputId = "transfer_NEON_AOP", label = NULL))
                                                                   )
                                                          )
                                                        )
                                               ),
                                               #### — MAP FEATURES ####
                                               tabPanel(title = tags$h5("Map Manager"), value = "filter",
                                                        includeMarkdown('Rmd/NEON_map.Rmd'),
                                                        radioButtons(inputId = "map_features", label = "Map layer:", choices = list("Field Sites"= "fieldsites", "Domains" = "domains", "Flight Boxes" = "flightpath"), inline = TRUE),
                                                        conditionalPanel("input.map_features == 'fieldsites'",
                                                                         pickerInput(inputId = "fieldsite_domain", label = "Domains:", choices = domains$Domain, selected = domains$Domain, multiple = TRUE,
                                                                                     options = list(`actions-box` = TRUE, `live-search` = TRUE, title = "Select Domains to include:", `selected-text-format`= "static")),
                                                                         selectInput(inputId = "fieldsite_type", label = "Site Type:", choices = c("CORE", "RELOCATABLE"), selected = c("CORE", "RELOCATABLE"), multiple = TRUE),
                                                                         selectInput(inputId = "fieldsite_habitat", label = "Site Habitat:", choices = c("Terrestrial", "Aquatic"), selected = c("Terrestrial", "Aquatic"), multiple = TRUE),
                                                                         uiOutput("fieldsite_state_ui"),
                                                                         uiOutput("fieldsite_sublocs_ui"),
                                                                         tags$h5("Map Legend"),
                                                                         tabsetPanel(
                                                                                            tabPanel("Terrestrial",
                                                                                                     tags$b("Hover over the checkbox to see more info:"),
                                                                                                     materialSwitch(inputId = "sublocs_tes_selectall", label = "All", value = TRUE, status = "info"),
                                                                                                     checkboxInput(inputId = "sublocs_baseplot", label = HTML("<img src='Img/distributedBaseplot.png' width=20><small> Distributed Base Plot</small>"), value = TRUE),
                                                                                                     bsTooltip(id = "sublocs_baseplot", title = baseplot_text, placement = "top"),
                                                                                                     checkboxInput(inputId = "sublocs_birdgrid", label = HTML("<img src='Img/birdGrid.png' width=20><small> Distributed Bird Grid</small>"), value = TRUE),
                                                                                                     bsTooltip(id = "sublocs_birdgrid", title = birdgrid_text, placement = "top"),
                                                                                                     checkboxInput(inputId = "sublocs_mammalgrid", label = HTML("<img src='Img/mammal.png' width=20><small> Distributed Mammal Grid</small>"), value = TRUE),
                                                                                                     bsTooltip(id = "sublocs_mammalgrid", title = mammalgrid_text, placement = "top"),
                                                                                                     checkboxInput(inputId = "sublocs_mosquitoplot", label = HTML("<img src='Img/mosquito.png' width=20><small> Distributed Mosquito Plot</small>"), value = TRUE),
                                                                                                     bsTooltip(id = "sublocs_mosquitoplot", title = mosquitoplot_text, placement = "top"),
                                                                                                     checkboxInput(inputId = "sublocs_tickplot", label = HTML("<img src='Img/tick.png' width=20><small> Distributed Tick Plot</small>"), value = TRUE),
                                                                                                     bsTooltip(id = "sublocs_tickplot", title = tickplot_text, placement = "top"),
                                                                                                     checkboxInput(inputId = "sublocs_phenologyplot", label = HTML("<img src='Img/phenology.png' width=20><small> Tower Phenology Plot</small>"), value = TRUE),
                                                                                                     bsTooltip(id = "sublocs_phenologyplot", title = phenologyplot_text, placement = "top")
                                                                                            ),
                                                                                            tabPanel("Aquatic",
                                                                                                     tags$b("Hover over the checkbox to see more info:"),
                                                                                                     materialSwitch(inputId = "sublocs_aqu_selectall", label = "All", value = TRUE, status = "info"),
                                                                                                     checkboxInput(inputId = "sublocs_well", label = HTML("<img src='Img/groundwaterWell.png' width=20><small> Groundwater Well</small>"), value = TRUE),
                                                                                                     bsTooltip(id = "sublocs_well", title = well_text, placement = "top"),
                                                                                                     checkboxInput(inputId = "sublocs_metstn", label = HTML("<img src='Img/metStation.png' width=20><small> Met. Station</small>"), value = TRUE),
                                                                                                     bsTooltip(id = "sublocs_metstn", title = metstn_text, placement = "top"),
                                                                                                     checkboxInput(inputId = "sublocs_sensor", label = HTML("<img src='Img/sensorStation.png' width=20><small> Sensor Station</small>"), value = TRUE),
                                                                                                     bsTooltip(id = "sublocs_sensor", title = sensor_text, placement = "top"),
                                                                                                     checkboxInput(inputId = "sublocs_gauge", label = HTML("<img src='Img/staffGaugeCamera.png' width=20><small> Staff gauge/camera</small>"), value = TRUE),
                                                                                                     bsTooltip(id = "sublocs_gauge", title = gauge_text, placement = "top"),
                                                                                                     checkboxInput(inputId = "sublocs_reach", label = HTML("<img src='Img/reach-icon.png' width=20><small> Sampling Reach Boundary</small>"), value = TRUE),
                                                                                                     bsTooltip(id = "sublocs_reach", title = reach_text, placement = "top"),
                                                                                                     checkboxInput(inputId = "sublocs_riparian", label = HTML("<img src='Img/riparianAssessment.png' width=20><small> Riparian Assessment</small>"), value = TRUE),
                                                                                                     bsTooltip(id = "sublocs_riparian", title = riparian_text, placement = "top")
                                                                                                     )
                                                                                          )
                                                                         ),
                                                        conditionalPanel("input.map_features == 'domains'",
                                                                         pickerInput(inputId = "domains_display", label = "Domains:", choices = domains$Domain, selected = domains$Domain, multiple = TRUE,
                                                                                     options = list(`actions-box` = TRUE, `live-search` = TRUE, title = "Select Domains to include:", `selected-text-format`= "static"))),
                                                        conditionalPanel("input.map_features == 'flightpath'",
                                                                         selectInput(inputId = "flightpath_year", label = "Year", choices = c(2016, 2017, 2025), selected = c(2025), multiple = TRUE))
                                               ),
                                               #### — SPATIAL DATA MANAGEMENT ####
                                               tabPanel(title = tags$h5("Spatial Data"), value = "spatial_data",
                                                        spatial_data_management_UI()
                                               )
                                             )
                                ),
                                mainPanel(width = 7,
                                          leafletOutput(outputId = "map", width = 'auto', height = '85vh')
                                )
                              )
                     ),
                     #### Tab 2: Description of this Project ####
                     tabPanel("About This Project",
                              navlistPanel(widths = c(3,9), id = "about",
                                           tabPanel("Overview",
                                                    includeMarkdown('Rmd/Project_description.Rmd')),
                                           tabPanel("Credits",
                                                    includeMarkdown('Rmd/Project_credits.Rmd')),
                                           tabPanel("About NEON",
                                                    includeMarkdown('Rmd/NEON_about.Rmd'))
                              )),
                     ####Tab 3: Tutorial/Help####
                     tabPanel("Help/Tutorials",
                              navlistPanel(widths = c(3,9), id = "tutorial",
                                           tabPanel("Introduction to the app",
                                                    includeMarkdown("Rmd/Help_intro.Rmd")),
                                           tabPanel("What is NEON?",
                                                    includeMarkdown("Rmd/Help_NEON.Rmd")),
                                           tabPanel("Using the interactive map",
                                                    includeMarkdown("Rmd/Help_map.Rmd"),
                                                    tabsetPanel(id = "tutorial_help",
                                                                tabPanel(tags$b("Layers"), value = "layers",
                                                                         includeMarkdown("Rmd/Help_map_layers.Rmd")),
                                                                tabPanel(tags$b("Finding sites/adding sublocations"),
                                                                         includeMarkdown("Rmd/Help_map_dropdown.Rmd")),
                                                                tabPanel(tags$b("Controlling map display "),
                                                                         includeMarkdown("Rmd/Help_map_display.Rmd")),
                                                                tabPanel(tags$b("Other tips"),
                                                                         includeMarkdown("Rmd/Help_map_tips.Rmd")),
                                                                tabPanel(icon("forward"))
                                                    )),
                                           tabPanel("Finding data products",
                                                    includeMarkdown("Rmd/Help_browse.Rmd"),
                                                    tabsetPanel(id = "tutorial_browse",
                                                                tabPanel(tags$b("Browsing by site"), value = "site",
                                                                         includeMarkdown("Rmd/Help_browse_site.Rmd")),
                                                                tabPanel(tags$b("Browsing by product"),
                                                                         includeMarkdown("Rmd/Help_browse_product.Rmd")),
                                                                tabPanel(tags$b("View Details about Product"),
                                                                         includeMarkdown("Rmd/Help_browse_details.Rmd")),
                                                                tabPanel(icon("forward"))
                                                    )),
                                           tabPanel("Downloading data products",
                                                    includeMarkdown("Rmd/Help_download.Rmd"),
                                                    tabsetPanel(id = "tutorial_download",
                                                                tabPanel(tags$b("Regular Download"), value = "regular",
                                                                         includeMarkdown("Rmd/Help_download_regular.Rmd")),
                                                                tabPanel(tags$b("AOP Download"),
                                                                         includeMarkdown("Rmd/Help_download_AOP.Rmd")),
                                                                tabPanel(tags$b("Characteristics of Downloads"),
                                                                         includeMarkdown("Rmd/Help_download_chars.Rmd")),
                                                                tabPanel(icon("forward"))
                                                    )
                                           ))
          )
)
