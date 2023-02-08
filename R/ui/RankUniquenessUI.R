################################################################################
#################### Gene-RaMeN ui for rank uniqueness tab #####################
################################################################################

tabPanel("Rank uniqueness",
         sidebarLayout(
           
           sidebarPanel(
             
             radioButtons("studyHetero", strong("Please select which dataset you want to use:"),
                          width = '100%',
                          choices = c("None" = "None",
                                      "SARS-CoV-2 screens" = "SARS2",
                                      "Flavivirus screens" = "Flavi",
                                      "Picornavirus screens" = "Picorna"),
                          selected = "None"),
             
             helpText(
               "select 'None' if you don't want to append your lists to any of these pre-loaded datasets"
             ),
             
             tags$hr(),
             
             fileInput("userFileHetero", strong("Upload your own dataset:"),
                       multiple = F,
                       accept = c(".xlsx")
             ),
             
             tags$hr(),
             
             uiOutput("contrastPanel")
             
           ),
           
           mainPanel(
             
             tabsetPanel(
               
               tabPanel("Input overview",
                        
                        DT::DTOutput("studyListHetero", "100%") %>% withSpinner(type = getOption("spinner.type", default = 4))
               ),
               
               tabPanel("Unique Ranks",
                        
                        fluidRow(
                          column(width = 6, p(class = 'text-center', downloadButton('downloadTableUp', 'Download table!'))),
                          column(width = 6, p(class = 'text-center', downloadButton('downloadTableDown', 'Download table!')))
                        ),
                        fluidRow(
                          column(width = 6, DT::DTOutput("tableHeteroUp", "100%") %>% withSpinner(type = getOption("spinner.type", default = 4))),
                          column(width = 6, DT::DTOutput("tableHeteroDown", "100%") %>% withSpinner(type = getOption("spinner.type", default = 4)))
                        )
               ),
               
               tabPanel("Heatmap",
                        sliderInput("nHeatmapHeteroUp", "Number of the unique ranks to be visualized for 1st group:",
                                    min = 0, max = 20, value = 5
                        ),
                        sliderInput("nHeatmapHeteroDown", "Number of the unique ranks to be visualized for 2nd group:",
                                    min = 0, max = 20, value = 5
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