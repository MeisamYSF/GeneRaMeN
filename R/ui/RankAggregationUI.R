################################################################################
################################################################################
#########                                                             ##########
#########            GeneRaMeN UI for rank aggregation tab            ##########
#########                                                             ##########
################################################################################
################################################################################

tabPanel("Rank aggregation",
         sidebarLayout(
           
           sidebarPanel(
             # actionButton("show", "Show modal dialog"),
             radioButtons("study", strong("Select a pre-loaded dataset:"),
                          width = '100%',
                          choices = c("None" = "None",
                                      "SARS-CoV-2 screens" = "SARS2_v4",
                                      "Flavivirus screens" = "Flavi_v4",
                                      "Seasonal Coronavirus screens" = "SeasonalCorona_v2"
                                      # "Picornavirus screens" = "Picorna_v4"
                                      ),
                          selected = "None"),
             
             helpText(
               "Select 'None' if you don't want to append your file to these pre-loaded datasets"
             ),
             
             tags$hr(),
             
             fileInput("userFile", strong("Upload your own dataset:"),
                       multiple = F,
                       accept = c(".xlsx")
             ),
             helpText(
               "For file format instruction refer to the 'Tutorial' tab"
             ),
             
             tags$hr(),
             
             checkboxInput("metaData", label = "I want to include meta-data (Optional)", value = FALSE),
             
             conditionalPanel(
               condition = "input.metaData",
               fileInput("userMetaFile", strong("Upload your meta data file:"),
                         multiple = F,
                         accept = c(".xlsx")
               ),
               helpText(
                 "Note that meta-data files for pre-selected datasets are already included in the app. For file format instruction refer to the 'Tutorial' tab"
               )
             ),
             
             tags$hr(),
             
             radioButtons("aggMethod", strong("Select rank aggregation method:"),
                          width = '100%',
                          choices = c("Robust Rank Aggregation" = "RRA",
                                      # "Bayesian Iterative Robust Rank Aggregation (BIRRA)" = "BIRRA",
                                      "Geometric Mean" = "geom.mean",
                                      "Mean" = "mean"),
                          selected = "RRA"),
             
             numericInput(inputId = "nTop",
                          label = strong("Specify the number of top-hits to be considered from each ranked list:"),
                          value = 5000),
             
             actionButton("submit", "Submit!", class = "btn-success")
           ),
           
           mainPanel(
             
             tabsetPanel(
               
               id = "aggregationPanel",
               
               tabPanel("Input overview",
                        DT::DTOutput("studyList", "100%") %>% withSpinner(type = getOption("spinner.type", default = 4))
               ),
               
               tabPanel("Aggregated ranks",
                        textOutput("parallel"),
                        p(class = 'text-center', downloadButton('downloadRRA', 'Download table!')),
                        DT::DTOutput("RRA", "100%") %>% withSpinner(type = getOption("spinner.type", default = 4))
               ),
               
               tabPanel("Scatter plot",
                        sliderInput("nScPlotTop", "Number of top hits to be highlighted:",
                                    min = 1, max = 100, value = 10),
                        hr(),
                        plotOutput("scatterPlot", width = "100%", height = "700px") %>% withSpinner(type = getOption("spinner.type", default = 4)),
                        br(),
                        hr(),
                        fluidRow(column(3, numericInput("wScatter", label = "Width", value = 10)),
                                 column(3, numericInput("hScatter", label = "Height", value = 10)),
                                 column(3, numericInput("ppiScatter", label = "Resolution", value = 300)),
                                 column(3, dropdown(
                                   downloadBttn(
                                     outputId = "scPlotPDF",
                                     label="PDF",
                                     color = "default",
                                     style = "fill",
                                     size='sm',
                                     block=TRUE
                                   ),
                                   downloadBttn(
                                     outputId = "scPlotPNG",
                                     label="PNG",
                                     color = "default",
                                     style = "fill",
                                     size='sm',
                                     block=TRUE
                                   ),
                                   downloadBttn(
                                     outputId = "scPlotTIFF",
                                     label="TIFF",
                                     color = "default",
                                     style = "fill",
                                     size='sm',
                                     block=TRUE
                                   ),
                                   circle=FALSE,
                                   label="Download plot",
                                   status="default"
                                 )
                                 )
                        )
               ),
               
               tabPanel("Gene rank finder", DT::DTOutput("rankFinder", "100%") %>% withSpinner(type = getOption("spinner.type", default = 4))),
               
               tabPanel("Heatmap",
                        sliderInput("nHeatmap", "Number of the top hits:",
                                    min = 1, max = 100, value = 20
                        ),
                        hr(),
                        # plotOutput("heatmap", height="auto") %>% withSpinner(type = getOption("spinner.type", default = 4)),
                        uiOutput("heatmapUI"),
                        br(),
                        hr(),
                        fluidRow(column(3, numericInput("wHeatmap", label = "Width", value = 10)),
                                 column(3, numericInput("hHeatmap", label = "Height", value = 10)),
                                 column(3, numericInput("ppiHeatmap", label = "Resolution", value = 300)),
                                 column(3, dropdown(
                                   downloadBttn(
                                     outputId = "heatmapPDF",
                                     label="PDF",
                                     color = "default",
                                     style = "fill",
                                     size='sm',
                                     block=TRUE
                                   ),
                                   downloadBttn(
                                     outputId = "heatmapPNG",
                                     label="PNG",
                                     color = "default",
                                     style = "fill",
                                     size='sm',
                                     block=TRUE
                                   ),
                                   downloadBttn(
                                     outputId = "heatmapTIFF",
                                     label="TIFF",
                                     color = "default",
                                     style = "fill",
                                     size='sm',
                                     block=TRUE
                                   ),
                                   circle=FALSE,
                                   label="Download plot",
                                   status="default"
                                 )
                                 )
                        )
               ),
               
               tabPanel("Enrichment analysis",
                        
                        fluidRow(column(3, numericInput("nEnrich", label = "Number of top hits:", value = 100)),
                                 column(3, selectInput("enrichDB", label = "Query database:", 
                                                       choices = list("KEGG pathway enrichment" = 'KEGG',
                                                                      # "Reactome pathway enrichment" = "REAC",
                                                                      "GO - Biological Process" = 'GO:BP',
                                                                      "GO - Molecular Function" = 'GO:MF',
                                                                      "GO - Cellular Component"= 'GO:CC'), 
                                                       selected = "KEGG")),
                                 column(3, numericInput("pvalEnrich", label = "Adjusted p-value cutoff:", value = 0.05)),
                                 column(3, actionButton("submitEnrich", "Submit!", class = "btn-success"))
                                 ),
                        
                        helpText(
                          p("All gene set over-representation analysis are powered by", a("g:Profiler", href = "https://doi.org/10.1093/nar/gkz369"), "server.")
                        ),
                        
                        hr(),
                        
                        uiOutput("enrichUI") %>% withSpinner(type = getOption("spinner.type", default = 4))
               ),
             )
           )
           
         )
)