################################################################################
################################################################################
#########                                                             ##########
#########            GeneRaMeN UI for rank uniqueness tab             ##########
#########                                                             ##########
################################################################################
################################################################################

tabPanel("Rank uniqueness",
         sidebarLayout(
           
           sidebarPanel(
             
             radioButtons("studyHetero", strong("Select a pre-loaded dataset:"),
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
             
             fileInput("userFileHetero", strong("Upload your own dataset:"),
                       multiple = F,
                       accept = c(".xlsx")
             ),
             
             helpText(
               "For file format instruction refer to the 'Tutorial' tab"
             ),
             
             tags$hr(),
             
             checkboxInput("metaDataHetero", label = "I want to include meta-data (Optional)", value = FALSE),
             
             conditionalPanel(
               condition = "input.metaDataHetero",
               fileInput("userMetaFileHetero", strong("Upload your meta data file:"),
                         multiple = F,
                         accept = c(".xlsx")
               ),
               helpText(
                 "Note that meta-data files for pre-selected datasets are already included in the app. For file format instruction refer to the 'Tutorial' tab"
               )
             ),
             
             tags$hr(),
             
             uiOutput("contrastPanel"),

           ),
           
           mainPanel(
             
             tabsetPanel(
               
               id = "uniquePanel",
               
               tabPanel("Input overview",
                        
                        DT::DTOutput("studyListHetero", "100%") %>% withSpinner(type = getOption("spinner.type", default = 4))
               ),
               
               tabPanel("Unique ranks",
                        
                        fluidRow(
                          column(width = 12, p(class = 'text-center', downloadButton('downloadTable', 'Download table!'))),
                          # column(width = 6, p(class = 'text-center', downloadButton('downloadTableDown', 'Download table!')))
                        ),
                        fluidRow(
                          column(width = 12, DT::DTOutput("tableHetero", "100%") %>% withSpinner(type = getOption("spinner.type", default = 4))),
                          # column(width = 6, DT::DTOutput("tableHeteroDown", "100%") %>% withSpinner(type = getOption("spinner.type", default = 4)))
                        )
               ),
               
               # tabPanel("Scatter Plot",
               # 
               #          plotlyOutput("plotlyUp") %>% withSpinner(type = getOption("spinner.type", default = 4)),
               #          plotlyOutput("plotlyDown") %>% withSpinner(type = getOption("spinner.type", default = 4)),
               # ),
               
               tabPanel("Heatmap",
                        fluidRow(column(3, sliderInput("nHeatmapHeteroUp", "Number of the unique ranks to be visualized for 1st group:",
                                                       min = 0, max = 50, value = 10)),
                                 column(3, sliderInput("nHeatmapHeteroDown", "Number of the unique ranks to be visualized for 2nd group:",
                                             min = 0, max = 50, value = 10)),
                                 column(2, checkboxInput("clustHeatmapCol", label = "Cluster the studies/columns", value = TRUE)),
                                 column(2, checkboxInput("clustHeatmapRow", label = "Cluster the genes/rows", value = TRUE)),
                                 column(2, numericInput("pval", label = "P-value cutoff", value = 0.05, min = 0, max = 1, step = NA))
                                 ),
                        hr(),
                        uiOutput("heatmapHeteroUI"),
                        br(),
                        hr(),
                        fluidRow(column(3, numericInput("wHeatmapHetero", label = "Width", value = 10)),
                                 column(3, numericInput("hHeatmapHetero", label = "Height", value = 10)),
                                 column(3, numericInput("ppiHeatmapHetero", label = "Resolution", value = 300)),
                                 column(3, dropdown(
                                   downloadBttn(
                                     outputId = "heatmapHeteroPDF",
                                     label="PDF",
                                     color = "default",
                                     style = "fill",
                                     size='sm',
                                     block=TRUE
                                   ),
                                   downloadBttn(
                                     outputId = "heatmapHeteroPNG",
                                     label="PNG",
                                     color = "default",
                                     style = "fill",
                                     size='sm',
                                     block=TRUE
                                   ),
                                   downloadBttn(
                                     outputId = "heatmapHeteroTIFF",
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
                        )
             )
           )
         )
)