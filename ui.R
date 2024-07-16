################################################################################
########################### GeneRaMeN ui function ##############################
################################################################################

ui <-
  fluidPage(
    navbarPage(
      
      strong("GeneRaMeN \u03b2 v1.2.0"), #2024-07-14

      theme = bs_theme(version = 4, bootswatch ="cosmo"),
      
      # Loading the ui for rank aggregation tab
      source('R/ui/RankAggregationUI.R', local = TRUE)$value,
      
      # Loading the ui for rank uniqueness tab
      source('R/ui/RankUniquenessUI.R', local = TRUE)$value,
      
      # Loading the ui for rank correlation tab
      source('R/ui/RankCorrelationUI.R', local = TRUE)$value,
      
      # Loading the ui for tutorial tab
      source('R/ui/TutorialsUI.R', local = TRUE)$value,
      
      # Loading the ui for more information tab
      source('R/ui/AboutUsUI.R', local = TRUE)$value
    )
  )
