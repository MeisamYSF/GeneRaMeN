################################################################################
################## Gene-RaMeN server for rank uniqueness tab ###################
################################################################################

### Reactive conductor to read the selected pre-loaded dataset from server
dataInputHeteroPre <- reactive({
  screenList <- list()
  if(input$studyHetero == "None")
    return(screenList)
  else
    tmpName <- paste0(input$studyHetero, ".rds")
  screenList <- readRDS(paste0("Data/", tmpName))
})

### Reactive conductor to read the input excel file from the user
dataInputHeteroUser <- reactive({
  userFile <- input$userFileHetero
  # validate(need(tools::file_ext(userFile$datapath) == "xlsx", "Please upload an excel file"))
  if(is.null(userFile))
    return(list())
  else
    userFile$datapath %>% readxl::excel_sheets() %>% purrr::set_names() %>% map(read_excel, path = userFile$datapath)
})

### Reactive conductor to combine data from user and pre-loaded
dataHetero <- reactive({
  
  inputPre <- dataInputHeteroPre()
  inputUser <- dataInputHeteroUser()
  screenList <- c(inputPre, inputUser)
})

### Data table output of all studies for overview
output$studyListHetero <- DT::renderDT({
  
  screenList <- dataHetero()
  
  # Check for Null value
  if(length(screenList) == 0)
    return(NULL)
  else
    DT::datatable(tibble("Study Number" = 1:length(screenList),
                         "Study" = names(screenList)),
                  rownames = F,
                  
                  options = list(
                    # Aligning all columns text to center
                    columnDefs = list(list(className = 'dt-center', targets = "_all")),
                    # Customizing the top colnames row -- black color
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                      "}")
                  )
    )
})

output$contrastPanel <- renderUI({
  
  screenList <- dataHetero()
  
  validate(need(length(screenList) != 0, "Waiting to choose a dataset ..."))
  
  fluidPage(
    br(),
    numericInput(inputId = "nTopHetero",
                 label = strong("Specify the number of top-hits to be considered from each ranked list:"),
                 value = 2000),
    
    multiInput(
      inputId = "groupsHetero", label = strong("Please contrast the two groups:"),
      choices = names(screenList),
      selected = NULL
    ),
    
    hr(),
    
    actionButton("submitHetero", "Submit!", class = "btn-success")
  )
})

### Reactive conductor for cleaning/standardizing the screening data
metaScreenHetero <- eventReactive(input$submitHetero, {
  
  screenList <- dataHetero()
  
  for(i in 1:length(screenList)) {
    # To standardize the Gene name aliases based on official gene symbol
    screenList[[i]][[1]] <- unlist(mapIds(org.Hs.eg.db, keys=screenList[[i]][[1]], column="SYMBOL", keytype="ALIAS", multiVals="first"))
    # To add the gene rankings to gene names as a new variable
    screenList[[i]][[2]] <- 1:length(screenList[[i]][[1]])
    # To remove all other columns except Gene name and ranks
    screenList[[i]]<- tibble(screenList[[i]][,1:2])
    # To remove gene names which could not be attributed to an official gene symbol
    screenList[[i]] <- na.omit(screenList[[i]])
    # Renaming
    colnames(screenList[[i]]) <- c("Gene", names(screenList[i]))
    # To remove duplicate gene names if any from the datasets
    screenList[[i]] <- distinct(screenList[[i]], Gene, .keep_all= TRUE)
  }
  screenList
})

### Reactive conductor to generate contrast class vector -- to be used by Rank Products
groupHet <- reactive({
  screenList <- dataHetero()
  ifelse(names(screenList) %in% input$groupsHetero, 1, 0)
})

### Reactive conductor to calculate differential hits by Rank Products
rankProdReact <- eventReactive(input$submitHetero, {
  
  screenList <- metaScreenHetero()
  screenClass <- groupHet()
  
  # To add the gene rankings to gene names as a new variable
  for(i in 1:length(screenList)) {
    screenList[[i]][[2]] <- c(1:isolate(input$nTopHetero), rep(isolate(input$nTopHetero), length(screenList[[i]][[1]])-isolate(input$nTopHetero)))
  }
  
  # validate(need(sum(screenClass) != 0), "Please select at least one study per group!")
  
  if(sum(screenClass) == 1) {
    
    tmpComb <- list()
    
    for(i in 1:length(screenList)) {
      tmpComb[[i]] <- screenList[[i]][[1]]
    }
    tmpComb <- tmpComb[screenClass != T]
    aggHits <- aggregateRanks(tmpComb, method = "RRA")
    aggHits <- tibble(1:dim(aggHits)[1], aggHits)
    colnames(aggHits) <- c("allAggRank", "Gene", "Score")
    
    screenDiff <- aggHits %>% mutate(allAggRank = if_else(allAggRank < isolate(input$nTopHetero), as.integer(allAggRank), isolate(input$nTopHetero))) %>%
      left_join(screenList[[which(screenClass == 1)]], by = "Gene") %>%
      mutate(Markers = !!as.name(names(screenList[[which(screenClass == 1)]])[2]) - allAggRank) %>%
      arrange(Markers) %>% 
      mutate(Markers = 1:length(Markers)) %>%
      dplyr::select(Gene, Markers)
  }
  else {
    # Merging all datasets into a single dataframe which shows a given gene ranks in each of the studies
    combinedHits <- screenList %>% purrr::reduce(full_join, by = "Gene")
    # RP
    screenRP <- RankProducts(as.matrix(combinedHits[,-1]), cl = screenClass, rand = 1, na.rm = T)
    screenDiff <- tibble(combinedHits$Gene, screenRP[["RPrank"]][,2])
    colnames(screenDiff) <- c("Gene", "Markers")
    screenDiff 
  }
  
  screenDiff %>% dplyr::arrange(Markers) %>% dplyr::rename("Rank for uniqueness" = Markers)
  
  })

# rankProdUp <- reactive({
#   screenDiff <- rankProdReact()
#   screenDiff <- screenDiff %>%
#     dplyr::arrange(Markers1) %>%
#     dplyr::select(Markers1, Gene) %>%
#     dplyr::rename("Rank for 1st group uniqueness" = Markers1)
# })

# rankProdDown <- reactive({
#   screenDiff <- rankProdReact()
#   screenDiff <- screenDiff %>%
#     dplyr::arrange(Markers2) %>%
#     dplyr::select(Markers2, Gene) %>%
#     dplyr::rename("Rank for uniqueness" = Markers2)
# })

output$tableHetero <- renderDT({
  
  tmpTable <- rankProdReact()
  
  datatable(tmpTable,
            
            rownames = F,
            
            options = list(
              # Aligning all columns text to center
              columnDefs = list(list(className = 'dt-center', targets = "_all")),
              # Customizing the top colnames row -- black color
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                "}")
            )
  )
})

# output$tableHeteroDown <- renderDT({
#   tmpTable <- rankProdDown()
#   
#   datatable(tmpTable,
#             
#             rownames = F,
#             
#             options = list(
#               # Aligning all columns text to center
#               columnDefs = list(list(className = 'dt-center', targets = "_all")),
#               # Customizing the top colnames row -- black color
#               initComplete = JS(
#                 "function(settings, json) {",
#                 "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
#                 "}")
#             )
#   )
# })

### Download handler for the marker ranks population table 1
output$downloadTable <- downloadHandler(
  
  filename = function() {
    paste0(Sys.Date(), "-geneRaMeN.csv")
  }, 
  content = function(file) {
    
    tmpTable <- rankProdReact()
    
    write.csv(tmpTable, file, row.names = F)
  }                                     
)

### Download handler for the marker ranks population table 2
# output$downloadTableDown <- downloadHandler(
#   
#   filename = function() {
#     paste0(Sys.Date(), "-geneRaMeN.csv")
#   }, 
#   content = function(file) {
#     
#     tmpTable <- rankProdDown()
#     
#     write.csv(tmpTable, file, row.names = F)
#   }                                     
# )

### Heatmap presentation of ranks of all top genes plus clustering
heatmapHeteroReact <- reactive({
  
  screenList <- metaScreenHetero()
  screenClass <- groupHet()
  
  # To add the gene rankings to gene names as a new variable
  for(i in 1:length(screenList)) {
    screenList[[i]][[2]] <- c(1:isolate(input$nTopHetero), rep(isolate(input$nTopHetero), length(screenList[[i]][[1]])-isolate(input$nTopHetero)))
  }
  
  markersList <- rankProdReact() %>% 
    slice_head(n = input$nHeatmapHetero) %>% 
    dplyr::select(Gene) %>% 
    unlist()
  
  # tmpDown <- rankProdDown() %>% 
  #   slice_head(n = input$nHeatmapHeteroDown) %>% 
  #   dplyr::select(Gene) %>% 
  #   unlist()
  
  # Merging all datasets into a single dataframe which shows a given gene ranks in each of the studies
  combinedHits <- screenList %>% purrr::reduce(full_join, by = "Gene")
  combinedHits <- combinedHits %>% 
    filter(Gene %in% markersList ) %>%
    
    data.frame()
  rownames(combinedHits) <- combinedHits$Gene
  
  annoteDf <- data.frame(Group = factor(screenClass))
  rownames(annoteDf) <- colnames(combinedHits[-1])
  
  tmpPlot <- pheatmap(combinedHits[,-1],
                      cluster_rows = F,
                      cluster_cols = input$clustHeatmap,
                      annotation_col = annoteDf ,
                      fontsize = 12,
                      border_color = NA,
                      color = colorRampPalette(c("#66b2ff","black"))(200),
                      # display_numbers = TRUE
  )
  
  plotObj$heatmapHetero <- tmpPlot
  tmpPlot
})

### Heatmap output visualization
output$heatmapHetero <- renderPlot(heatmapHeteroReact())

plotHeightHetero <- reactive(200 + (20*input$nHeatmapHetero))
output$heatmapHeteroUI <- renderUI({
  plotOutput("heatmapHetero", height = plotHeightHetero()) %>% withSpinner(type = getOption("spinner.type", default = 4))
})

### Download handlers for the heatmap
output$heatmapHeteroPDF <- downloadHandler(
  filename = function() {
    paste0(Sys.Date(), "-geneRaMeN.pdf")
  },
  content = function(file){
    pdf(file, width=input$wHeatmapHetero, height=input$hHeatmapHetero)
    print(plotObj$heatmapHetero)
    dev.off()
  }
)
output$heatmapHeteroPNG <- downloadHandler(
  filename = function() {
    paste0(Sys.Date(), "-geneRaMeN.png")
  },
  content = function(file){
    png(file, width=input$wHeatmapHetero, height=input$hHeatmapHetero, units="in", res=input$ppiHeatmapHetero)
    print(plotObj$heatmapHetero)
    dev.off()
  }
)
output$heatmapHeteroTIFF <- downloadHandler(
  filename = function() {
    paste0(Sys.Date(), "-geneRaMeN.tiff")
  },
  content = function(file){
    tiff(file, width=input$wHeatmapHetero, height=input$hHeatmapHetero, units="in", res=input$ppiHeatmapHetero)
    print(plotObj$heatmapHetero)
    dev.off()
  }
)