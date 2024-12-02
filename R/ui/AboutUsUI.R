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
                    "is a web-tool to analyze, aggregate, and compare ranked gene lists generated from a variety of high throughput biological assays.", style = "font-size:20px;"),
                    p("\n •	GeneRaMeN’s “Rank Aggregation” allows users to identify high confident hits consistently present in multiple genetic screening hit lists.", style = "font-size:20px;"),
                    p("\n •	GeneRaMeN’s “Rank Correlation” enables rank correlation analyses to determine groups of genes which show similar or opposite trends to a hit of interest", style = "font-size:20px;"),
                    p("\n •	GeneRaMeN’s “Rank Uniqueness” permits users to uncover top hits that uniquely associated with a specific subset of the datasets.", style = "font-size:20px;"),
                  br(),
                  p("GeneRaMeN is open source and free for all academic and educational purposes, our source code can be found at",
                    a("GitHub", href = "https://github.com/MeisamYSF/GeneRaMeN"),
                    "under a",
                    a("GPLv2", href = "https://choosealicense.com/licenses/gpl-2.0/"),
                    "licesnce. GeneRaMeN app can also be downloaded and run locally on any computer system with R installed.",
                    style = "font-size:20px;"),
                  br(),
                  p("If you have used GeneRaMeN in your work, please cite our article at",
                  a("Briefings in Bioinformatics", href = "https://doi.org/10.1093/bib/bbae452"),
                    style = "font-size:20px;"),
                  br(),
                  p("For more information about us, please visit ",
                    a("YSO Lab website.", href = "https://sites.google.com/view/ysolab"),
                    "If you have questions about the tool or bug reports, please contact Dr. Yaw Shin Ooi (yawshin.ooi@duke-nus.edu.sg)",
                    style = "font-size:20px;"))
         )
)