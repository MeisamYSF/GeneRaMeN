################################################################################
########################## Gene-RaMeN ui function ##############################
################################################################################

ui <-
  fluidPage(
    navbarPage(
      # titlePanel(title = div(
      #                img(
      #                  src = "RaMeN.png",
      #                  height = 20,
      #                  width = 20,
      #                  style = "margin:5px 5px"
      #                ),
      #                "Gene-RaMeN \u03b1 v0.5.0"
      #              )
      # ),
      strong("Gene-RaMeN \u03b1 v0.5.0"),
      # id = "navbar",
      # inverse = TRUE,
      theme = bs_theme(version = 4, bootswatch ="cosmo"),
      
      # Loading the ui for rank aggregation part
      source('R/ui/RobustHitsUI.R', local = TRUE)$value
      
    )
  )