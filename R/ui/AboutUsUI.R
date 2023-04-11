################################################################################
####################### Gene-RaMeN ui for about us tab #########################
################################################################################

tabPanel("About us",
         
         fluidRow(
           column(6,
                  tags$img(src = "Logo.png", width = "100%")
           ),
           column(6,
                  p("Gene Ranks Meta-aNalyzer", strong("(Gene-RaMeN)"),"is a web-tool developed using R/Shiny to perform meta-analysis and combine genome-wide screening data from multiple studies."),
                  br(),
                  p("It generates a high confidence list of hits based on the", em("Robust Rank"), "calculated by the RRA algorithm"),
                  p("Gene-RaMeN is open source and free, and its source code can be found at GitHub <Link>."),
                  br(),
                  p("For more information visit", a("YSO LAB website.", href = "https://sites.google.com/view/ysolab")))
         )
)