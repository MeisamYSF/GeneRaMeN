################################################################################
##################### GeneRaMeN UI for rank uniqueness tab #####################
################################################################################

tabPanel("Rank uniqueness",
         sidebarLayout(
           
           sidebarPanel(
             
             radioButtons("studyHetero", strong("Please select which dataset you want to use:"),
                          width = '100%',
                          choices = c("None" = "None",
                                      "SARS-CoV-2 screens" = "SARS2_v3",
                                      "Flavivirus screens" = "Flavi_v3",
                                      "Seasonal Coronavirus screens" = "SeasonalCorona_v1",
                                      "Picornavirus screens" = "Picorna_v3"),
                          selected = "None"),
             
             helpText(
               "Select 'None' if you don't want to append your lists to any of these pre-loaded datasets"
             ),
             
             tags$hr(),
             
             fileInput("userFileHetero", strong("Upload your own dataset:"),
                       multiple = F,
                       accept = c(".xlsx")
             ),
             
             helpText(
               "Refer to Tutorial tab for file format instructions."
             ),
             
             tags$hr(),
             
             checkboxInput("metaDataHetero", label = "Include meta-data in the plots (Optional)", value = FALSE),
             
             conditionalPanel(
               condition = "input.metaDataHetero",
               fileInput("userMetaFileHetero", strong("Upload your meta data file:"),
                         multiple = F,
                         accept = c(".xlsx")
               ),
               helpText(
                 "Refer to Tutorial tab for file format instructions."
               )
             ),
             
             tags$hr(),
             
             uiOutput("contrastPanel"),

           ),
           
           mainPanel(
             
             tabsetPanel(
               
               id = "uniquePanel",
               
               tabPanel("Input Overview",
                        
                        DT::DTOutput("studyListHetero", "100%") %>% withSpinner(type = getOption("spinner.type", default = 4))
               ),
               
               tabPanel("Unique Ranks",
                        
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
                                 column(3, checkboxInput("clustHeatmapCol", label = "Cluster the studies/columns", value = TRUE)),
                                 column(3, checkboxInput("clustHeatmapRow", label = "Cluster the genes/rows", value = TRUE))),
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