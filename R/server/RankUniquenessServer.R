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
                 value = 5000),
    
    multiInput(
      inputId = "groupsHetero", label = strong("Please contrast the two groups:"),
      choices = names(screenList),
      selected = NULL
    ),
    
    tags$hr(),
    
    radioButtons("uniqueMethod", strong("Please select the algorithm you want to use for rank uniqueness identification:"),
                 width = '100%',
                 choices = c("Robust Rank Aggregation" = "RRA",
                             "Rank Product (Geometric Mean)" = "RP",
                             "Rank Sum (Mean)" = "RS"),
                 selected = "RRA"),
    
    hr(),
    
    actionButton("submitHetero", "Submit!", class = "btn-success")
  )
})

### Reactive conductor for cleaning/standardizing the screening data
metaScreenHetero <- eventReactive(input$submitHetero, {
  
  screenList <- dataHetero()
  
  if(!is.null(input$userFileHetero)) {
    
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
  }
  screenList
})

### Reactive conductor to generate contrast class vector -- to be used by Rank Products
groupHet <- reactive({
  # screenList <- dataHetero()
  screenList <- metaScreenHetero()
  ifelse(names(screenList) %in% input$groupsHetero, 1, 0)
})

observeEvent(input$submitHetero, {
  updateTabsetPanel(session = session, inputId = "uniquePanel", selected = "Unique Ranks")
})

### Reactive conductor to calculate differential hits by Rank Products
rankUniqReact <- eventReactive(input$submitHetero, {
  
  screenList <- metaScreenHetero()
  screenClass <- groupHet()
  
  # To add the gene rankings to gene names as a new variable
  for(i in 1:length(screenList)) {
    screenList[[i]][[2]] <- c(1:isolate(input$nTopHetero), rep(isolate(input$nTopHetero), length(screenList[[i]][[1]])-isolate(input$nTopHetero)))
  }
  
  ### Calculation of the markers based on RRA, RP, or RS algorithms:
  
  if(sum(screenClass) == 0) stop("Please select at least one study for each group!")
  
  else {
    
    if(sum(screenClass) == 1) {
      
      if(input$uniqueMethod == "RRA") {
        
        tmpComb <- list()
        
        for(i in 1:length(screenList)) {
          tmpComb[[i]] <- screenList[[i]][[1]]
        }

        tmpComb <- tmpComb[screenClass != T]
        aggHits <- aggregateRanks(tmpComb, method = "RRA")
        aggHits <- tibble(1:dim(aggHits)[1], aggHits)
        colnames(aggHits) <- c("aggRank", "Gene", "Score")
        
        screenDiff <- aggHits %>% 
          mutate(aggRank = if_else(aggRank < isolate(input$nTopHetero), as.integer(aggRank), isolate(input$nTopHetero))) %>%
          left_join(screenList[[which(screenClass == 1)]], by = "Gene") %>%
          mutate(delta = !!as.name(names(screenList[[which(screenClass == 1)]])[2]) - aggRank) %>%
          arrange(delta) %>% 
          mutate(Rank = 1:length(delta)) %>%
          na.omit() %>%
          dplyr::select(Gene, Rank, delta)
      }
      
      else {
        
        if(input$uniqueMethod == "RP") {
          
          combinedHits <- screenList %>% purrr::reduce(full_join, by = "Gene")
          tmpComb <- combinedHits[,-1]
          tmpComb <- (is.na(tmpComb))*floor(rowMeans(tmpComb, na.rm=TRUE))[row(tmpComb)] + replace(tmpComb, is.na(tmpComb), 0)
          tmpComb <- tmpComb[screenClass != T]
          aggHits <- apply(tmpComb, 1, function(x) exp(mean(log(x), na.rm = TRUE)))
          
          aggHits <- tibble(combinedHits$Gene, aggHits) %>% 
            dplyr::arrange(aggHits) %>% 
            mutate("Rank" = 1:length(aggHits))
          colnames(aggHits) <- c("Gene", "Score", "Rank1")
          
          screenDiff <- aggHits %>% 
            mutate(Rank1 = if_else(Rank1 < isolate(input$nTopHetero), as.integer(Rank1), isolate(input$nTopHetero))) %>%
            left_join(screenList[[which(screenClass == 1)]], by = "Gene") %>%
            mutate(delta = !!as.name(names(screenList[[which(screenClass == 1)]])[2]) - Rank1) %>%
            arrange(delta) %>% 
            mutate(Rank = 1:length(delta)) %>%
            na.omit() %>%
            dplyr::select(Gene, Rank, delta)
        }
        
        if(input$uniqueMethod == "RS") {
          
          combinedHits <- screenList %>% purrr::reduce(full_join, by = "Gene")
          tmpComb <- combinedHits[,-1]
          tmpComb <- (is.na(tmpComb))*floor(rowMeans(tmpComb, na.rm=TRUE))[row(tmpComb)] + replace(tmpComb, is.na(tmpComb), 0)
          tmpComb <- tmpComb[screenClass != T]
          aggHits <- apply(tmpComb, 1, mean, na.rm = TRUE)
          
          aggHits <- tibble(combinedHits$Gene, aggHits) %>% 
            dplyr::arrange(aggHits) %>% 
            mutate("Rank" = 1:length(aggHits))
          colnames(aggHits) <- c("Gene", "Score", "Rank1")
          
          screenDiff <- aggHits %>% 
            mutate(Rank1 = if_else(Rank1 < isolate(input$nTopHetero), as.integer(Rank1), isolate(input$nTopHetero))) %>%
            left_join(screenList[[which(screenClass == 1)]], by = "Gene") %>%
            mutate(delta = !!as.name(names(screenList[[which(screenClass == 1)]])[2]) - Rank1) %>%
            arrange(delta) %>% 
            mutate(Rank = 1:length(delta)) %>%
            na.omit() %>%
            dplyr::select(Gene, Rank, delta)
        }
      }
    }
    
    if(sum(screenClass) > 1) {
      
      if(input$uniqueMethod == "RRA") {
        
        tmpComb <- list()
        
        for(i in 1:length(screenList)) {
          tmpComb[[i]] <- screenList[[i]][[1]]
        }
        tmpComb1 <- tmpComb[screenClass != T]
        aggHits1 <- aggregateRanks(tmpComb1, method = "RRA")
        aggHits1 <- tibble(1:dim(aggHits1)[1], aggHits1)
        colnames(aggHits1) <- c("Rank1", "Gene", "Score")
        aggHits1 <- aggHits1 %>% 
           mutate(Rank1 = if_else(Rank1 < isolate(input$nTopHetero), as.integer(Rank1), isolate(input$nTopHetero)))
        
        tmpComb2 <- tmpComb[screenClass == T]
        aggHits2 <- aggregateRanks(tmpComb2, method = "RRA")
        aggHits2 <- tibble(1:dim(aggHits2)[1], aggHits2)
        colnames(aggHits2) <- c("Rank2", "Gene", "Score")
        aggHits2 <- aggHits2 %>% 
         mutate(Rank2 = if_else(Rank2 < isolate(input$nTopHetero), as.integer(Rank2), isolate(input$nTopHetero)))
        
        screenDiff <- aggHits1 %>%
          left_join(aggHits2, by = "Gene") %>%
          mutate(delta = Rank2 - Rank1) %>%
          arrange(delta) %>% 
          mutate(Rank = 1:length(delta)) %>%
          na.omit() %>%
          dplyr::select(Gene, Rank, delta)
      }
      
      else {
        
        if(input$uniqueMethod == "RP") {
          
          combinedHits <- screenList %>% purrr::reduce(full_join, by = "Gene")
          tmpComb <- combinedHits[,-1]
          tmpComb <- (is.na(tmpComb))*floor(rowMeans(tmpComb, na.rm=TRUE))[row(tmpComb)] + replace(tmpComb, is.na(tmpComb), 0)
          
          tmpComb1 <- tmpComb[screenClass != T]
          aggHits1 <- apply(tmpComb1, 1, function(x) exp(mean(log(x), na.rm = TRUE)))
          aggHits1 <- tibble(combinedHits$Gene, aggHits1) %>% 
            dplyr::arrange(aggHits1) %>% 
            mutate("Rank" = 1:length(aggHits1))
          colnames(aggHits1) <- c("Gene", "Score", "Rank1")
          
          tmpComb2 <- tmpComb[screenClass == T]
          aggHits2 <- apply(tmpComb2, 1, function(x) exp(mean(log(x), na.rm = TRUE)))
          aggHits2 <- tibble(combinedHits$Gene, aggHits2) %>% 
            dplyr::arrange(aggHits2) %>% 
            mutate("Rank" = 1:length(aggHits2))
          colnames(aggHits2) <- c("Gene", "Score", "Rank2")
          
          screenDiff <- aggHits1 %>% 
            mutate(Rank1 = if_else(Rank1 < isolate(input$nTopHetero), as.integer(Rank1), isolate(input$nTopHetero))) %>%
            left_join(aggHits2 %>%
                        mutate(Rank2 = if_else(Rank2 < isolate(input$nTopHetero), as.integer(Rank2), isolate(input$nTopHetero))), by = "Gene") %>%
            # left_join(aggHits2) %>%
            mutate(delta = Rank2 - Rank1) %>%
            arrange(delta) %>% 
            mutate(Rank = 1:length(delta)) %>%
            na.omit() %>%
            dplyr::select(Gene, Rank, delta)
        }
        
        if(input$uniqueMethod == "RS") {
          
          combinedHits <- screenList %>% purrr::reduce(full_join, by = "Gene")
          tmpComb <- combinedHits[,-1]
          tmpComb <- (is.na(tmpComb))*floor(rowMeans(tmpComb, na.rm=TRUE))[row(tmpComb)] + replace(tmpComb, is.na(tmpComb), 0)
          
          tmpComb1 <- tmpComb[screenClass != T]
          aggHits1 <- apply(tmpComb1, 1, mean, na.rm = TRUE)
          aggHits1 <- tibble(combinedHits$Gene, aggHits1) %>% 
            dplyr::arrange(aggHits1) %>% 
            mutate("Rank" = 1:length(aggHits1))
          colnames(aggHits1) <- c("Gene", "Score", "Rank1")
          
          tmpComb2 <- tmpComb[screenClass == T]
          aggHits2 <- apply(tmpComb2, 1, mean, na.rm = TRUE)
          aggHits2 <- tibble(combinedHits$Gene, aggHits2) %>% 
            dplyr::arrange(aggHits2) %>% 
            mutate("Rank" = 1:length(aggHits2))
          colnames(aggHits2) <- c("Gene", "Score", "Rank2")
          
          screenDiff <- aggHits1 %>% 
            mutate(Rank1 = if_else(Rank1 < isolate(input$nTopHetero), as.integer(Rank1), isolate(input$nTopHetero))) %>%
            left_join(aggHits2 %>%
                        mutate(Rank2 = if_else(Rank2 < isolate(input$nTopHetero), as.integer(Rank2), isolate(input$nTopHetero))), by = "Gene") %>%
            # left_join(aggHits2) %>%
            mutate(delta = Rank2 - Rank1) %>%
            arrange(delta) %>% 
            mutate(Rank = 1:length(delta)) %>%
            na.omit() %>%
            dplyr::select(Gene, Rank, delta)
        }
      }
    }
  }
  
  screenDiff
  
})

# # Merging all datasets into a single dataframe which shows a given gene ranks in each of the studies
# combinedHits <- screenList %>% purrr::reduce(full_join, by = "Gene")
# # RP
# screenRP <- RankProducts(as.matrix(combinedHits[,-1]), cl = screenClass, rand = 123, na.rm = T, logged = F)
# 
# pfp <- as.matrix(screenRP$pfp)
# rank <- as.matrix(screenRP$RPrank)
# 
# screenDiff <- tibble(combinedHits$Gene, rank[,1], pfp[,1], rank[,2], pfp[,2])
# colnames(screenDiff) <- c("Gene", "Rank for marking 2nd group", "PFP for marking 2nd group", "Rank for marking 1st group", "PFP for marking 1st group")
# screenDiff 

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

x <- reactive({
  screenDiff <- rankUniqReact()
  rng <- dim(screenDiff)[1] - 1
  screenDiff %>% mutate(pval = ifelse(delta >= 0, 
                                      EnvStats::ptri(delta, min = -rng, max = rng, mode = 0),
                                      EnvStats::ptri(-delta, min = -rng, max = rng, mode = 0)))
})


output$tableHetero <- renderDT({
  
  # tmpTable <- rankUniqReact()
  tmpTable <- x()
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
    
    tmpTable <- rankUniqReact()
    
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

### Scatterplots

# output$plotlyUp <- renderPlotly({
#   
#   plotlyObj <- rankProdReact() 
#   plotlyObj <- plotlyObj %>%
#     dplyr::arrange(`Rank for marking 2nd group`) %>%
#     slice_head(n = input$nTopHetero)
#   
#   plot_ly(
#     data = plotlyObj,
#     x = ~`Rank for marking 2nd group`,
#     y = ~`PFP for marking 2nd group`,
#     type = "scatter",
#     mode = "markers",
#     opacity = 0.5,
#     marker = list(color = "darkblue", size = 8),
#     hovertext = ~ paste0(Gene, "<br>PFP Score: ", round(`PFP for marking 2nd group`, digits = 3)),
#     hoverinfo = "text"
#   )
# })
# 
# output$plotlyDown <- renderPlotly({
#   
#   plotlyObj <- rankProdReact() 
#   plotlyObj <- plotlyObj %>%
#     dplyr::arrange(`Rank for marking 1st group`) %>%
#     slice_head(n = input$nTopHetero)
#   
#   plot_ly(
#     data = plotlyObj,
#     x = ~`Rank for marking 1st group`,
#     y = ~`PFP for marking 1st group`,
#     type = "scatter",
#     mode = "markers",
#     opacity = 0.5,
#     marker = list(color = "red", size = 8),
#     hovertext = ~ paste0(Gene, "<br>PFP Score: ", round(`PFP for marking 1st group`, digits = 3)),
#     hoverinfo = "text"
#   )
# })


### Heatmap presentation of ranks of all top genes plus clustering
heatmapHeteroReact <- reactive({
  
  screenList <- metaScreenHetero()
  screenClass <- groupHet()
  
  # To add the gene rankings to gene names as a new variable
  for(i in 1:length(screenList)) {
    screenList[[i]][[2]] <- c(1:isolate(input$nTopHetero), rep(isolate(input$nTopHetero), length(screenList[[i]][[1]])-isolate(input$nTopHetero)))
  }
  
  markers1 <- rankUniqReact() %>% 
    slice_head(n = input$nHeatmapHeteroUp) %>% 
    dplyr::select(Gene) %>% 
    unlist()
  
  markers2 <- rankUniqReact() %>% 
    slice_tail(n = input$nHeatmapHeteroDown) %>% 
    dplyr::select(Gene) %>% 
    unlist()
  
  # Merging all datasets into a single dataframe which shows a given gene ranks in each of the studies
  combinedHits <- screenList %>% purrr::reduce(full_join, by = "Gene")
  combinedHits <- combinedHits %>% 
    filter(Gene %in% c(markers1, markers2)) %>%
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
                      color = colorRampPalette(c("#66b2ff","black"))(200)
                      # display_numbers = TRUE
  )
  
  plotObj$heatmapHetero <- tmpPlot
  tmpPlot
})

### Heatmap output visualization
output$heatmapHetero <- renderPlot(heatmapHeteroReact())

plotHeightHetero <- reactive(200 + (20*(input$nHeatmapHeteroUp+input$nHeatmapHeteroDown)))
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