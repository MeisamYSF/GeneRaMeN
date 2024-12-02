################################################################################
################################################################################
#########                                                             ##########
#########               GeneRaMeN server function (main)              ##########
#########                                                             ##########
################################################################################
################################################################################

server <- function(input, output, session) {
  
  ### Setting limits for the size of file user is allowed to submit -- currently 100 MB
  options(shiny.maxRequestSize = 100*1024^2)
  options(rsconnect.max.bundle.size=3145728000)
  
  ### Loading the server for rank aggregation tab
  source('R/server/RankAggregationServer.R', local = TRUE)$value
  
  ### Loading the server for rank uniqueness tab
  source('R/server/RankUniquenessServer.R', local = TRUE)$value
  
  ### Loading the server for rank uniqueness tab
  source('R/server/RankCorrelationServer.R', local = TRUE)$value
}

################################################################################
################################################################################

### Uncomment for deploying to shinyapps.io
### setrepositories for bioc first:
# rsconnect::deployApp(appDir = "~/GeneRaMeN", appName = "GeneRaMeN", account = "ysolab")
# rsconnect::configureApp("GeneRaMeN", size="xxxlarge", account = "ysolab")