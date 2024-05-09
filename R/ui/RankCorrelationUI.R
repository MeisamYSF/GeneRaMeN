################################################################################
################################################################################
#########                                                             ##########
#########            GeneRaMeN UI for rank correlation tab            ##########
#########                                                             ##########
################################################################################
################################################################################

tabPanel("Rank correlation",
         sidebarLayout(
           
           sidebarPanel(
             
             radioButtons("studyCorr", strong("Select a pre-loaded dataset:"),
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
             
             fileInput("userFileCorr", strong("Upload your own dataset:"),
                       multiple = F,
                       accept = c(".xlsx")
             ),
             
             helpText(
               "For file format instruction refer to the 'Tutorial' tab"
             ),
             
             tags$hr(),
             
             checkboxInput("metaDataCorr", label = "I want to include meta-data (Optional)", value = FALSE),
             
             conditionalPanel(
               condition = "input.metaDataCorr",
               fileInput("userMetaFileCorr", strong("Upload your meta data file:"),
                         multiple = F,
                         accept = c(".xlsx")
               ),
               helpText(
                 "Note that meta-data files for pre-selected datasets are already included in the app. For file format instruction refer to the 'Tutorial' tab"
               )
             ),
             
             tags$hr(),
             
             # radioButtons("corrMethod", strong("Select rank correlation identification method:"),
             #              width = '100%',
             #              choices = c("Pearson correlation" = "pearson",
             #                          "Spearman correlation" = "spearman"),
             #              selected = "pearson"),
             # 
             numericInput(inputId = "nTopCorr",
                          label = strong("Specify the maximum rank threshold:"),
                          value = 5000),
             
             br(),
             
             actionButton("submitCorr", "Submit!", class = "btn-success")
             
           ),
           
           mainPanel(
             
             tabsetPanel(
               
               id = "corrPanel",
               
               tabPanel("Input overview",
                        
                        DT::DTOutput("studyListCorr", "100%") %>% withSpinner(type = getOption("spinner.type", default = 4))
               ),
               
               tabPanel("Correlation plot",
                        
                        uiOutput("corrGeneSelect") %>% withSpinner(type = getOption("spinner.type", default = 4)),
                        hr(),
                        fluidRow(column(6, DTOutput("corrList")),
                                 column(6, DTOutput("antiCorrList"))
                                 )
               ),
               
               tabPanel("Line graph",
                        
                        uiOutput("corrGeneSelectLine") %>% withSpinner(type = getOption("spinner.type", default = 4)),
                        hr(),
                        plotlyOutput("corPlot", height = "200%") %>% withSpinner(type = getOption("spinner.type", default = 4))
     
               ),
               
               tabPanel("Heatmap",
                        
                        uiOutput("corrGeneSelectHeat") %>% withSpinner(type = getOption("spinner.type", default = 4)),
                        hr(),
                        uiOutput("heatmapCorrUI")
               )
             )
           )
         )
)