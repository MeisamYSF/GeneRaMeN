################################################################################
################################################################################
#########                                                             ##########
#########               GeneRaMeN server function (main)              ##########
#########                                                             ##########
################################################################################
################################################################################

### Uncomment for deploying to shinyapps.io | REMOVE BEFORE PUBLICATION
# setrepositories for bioc first:
# rsconnect::deployApp(appDir = "~/Documents/GitHub/GeneRaMeN", appName = "Dev-Meisam", account = "ysolab")
# configureApp("Dev-Meisam-v6", appDir = "~/Documents/GitHub/Gene-RaMeN/", size="xxxlarge", account = "ysolab")
# rsconnect::configureApp("Dev-Meisam", size="xxxlarge", account = "ysolab")

server <- function(input, output, session) {
  
  ### Setting limits for the size of file user is allowed to submit -- currently 100 MB
  options(shiny.maxRequestSize = 100*1024^2)
  options(rsconnect.max.bundle.size=3145728000)
  
  ### Loading the server for rank aggregation tab
  source('R/server/RankAggregationServer.R', local = TRUE)$value
  
  ### Loading the server for rank uniqueness tab
  source('R/server/RankUniquenessServer.R', local = TRUE)$value
}
