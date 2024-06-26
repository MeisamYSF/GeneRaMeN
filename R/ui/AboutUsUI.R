################################################################################
################################################################################
#########                                                             ##########
#########                GeneRaMeN UI for about us tab                ##########
#########                                                             ##########
################################################################################
################################################################################

tabPanel("About us",
         
         fluidRow(
           column(6,
                  tags$img(src = "Logo.png", width = "100%")
           ),
           column(6,
                  p(HTML(paste0(strong("Gene"), strong("Ra"), "nks ", strong("Me"), "ta A", strong("n"), "alyzer")), strong("(GeneRaMeN)"),
                    "is a web-tool to analyze, aggregate, and compare ranked gene lists generated from a variety of high throughput biological assays.",
                    "GeneRaMeN can provide the user with both homogeneity and heterogeneity meta-analysis, with simple yet powerful methods optimized for ranked gene data.",
                    style = "font-size:20px;"),
                  br(),
                  p("GeneRaMeN is open source and free for all academic and educational purposes, our source code can be found at",
                    a("GitHub", href = "https://github.com/MeisamYSF/GeneRaMeN"),
                    "under a",
                    a("GPLv2", href = "https://choosealicense.com/licenses/gpl-2.0/"),
                    "licesnce. GeneRaMeN app can also be downloaded and run locally on any computer/system with R installed.",
                    style = "font-size:20px;"),
                  br(),
                  p("If you have used GeneRaMeN in your work, please cite our article at <link>",
                    style = "font-size:20px;"),
                  br(),
                  p("For more information about us, please visit ",
                    a("YSO LAB website.", href = "https://sites.google.com/view/ysolab"),
                    "If you have questions about the tool or bug reports, please contact Dr. Yaw Shin Ooi (yawshin.ooi@duke-nus.edu.sg)",
                    style = "font-size:20px;"))
         )
)