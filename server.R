################################################################################
######################## Gene RaMeN server function ############################
################################################################################

### Uncomment for deploying to shinyapps.io | REMOVE BEFORE PUBLICATION
# setrepositories for bioc first:
# rsconnect::deployApp(appDir = "~/Documents/GitHub/Gene-RaMeN", appName = "Dev-Meisam", account = "ysolab")
# rsconnect::configureApp("Dev-Meisam", size="xxxlarge", account = "ysolab")

server <- function(input, output, session) {
  
  ### Setting limits for the size of file user is allowed to submit -- 50 MB
  options(shiny.maxRequestSize = 50*1024^2)
  
  ### Loading the server for rank aggregation tab
  source('R/server/RankAggregationServer.R', local = TRUE)$value
  
  ### Loading the server for rank uniqueness tab
  source('R/server/RankUniquenessServer.R', local = TRUE)$value
}
