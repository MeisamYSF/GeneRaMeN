################################################################################
################################################################################
#########                                                             ##########
#########         GeneRaMeN server function (rank uniqueness)         ##########
#########                                                             ##########
################################################################################
################################################################################

### Reactive element to read the selected pre-loaded datasets
dataInputHeteroPre <- reactive({
  screenList <- list()
  if(input$studyHetero == "None")
    return(screenList)
  else
    tmpName <- paste0(input$studyHetero, ".rds")
  screenList <- readRDS(paste0("Data/PresetData/", tmpName))
})

### Reactive element to read the meta data for the pre-loaded datasets - selected in the UI
metaDataInputHeteroPre <- reactive({
  metaDataPre <- NULL
  if(input$studyHetero != "None") {
    tmpName <- paste0(input$studyHetero, "_meta.xlsx")
    metaDataPre <- read_excel(paste0("Data/PresetMetaData/", tmpName))
  }
  else return(metaDataPre)
})

### Reactive element to read the input data file from the user
dataInputHeteroUser <- reactive({
  userFile <- input$userFileHetero
  # validate(need(tools::file_ext(userFile$datapath) == "xlsx", "Please upload an excel file"))
  if(is.null(userFile))
    return(list())
  else
    userFile$datapath %>% readxl::excel_sheets() %>% purrr::set_names() %>% map(read_excel, path = userFile$datapath)
})

### Reactive element to read the input meta-data file from the user (if needed), and combine it with the preset meta-data (if needed)
annotateMetaDataHetero <- reactive({
  inputPre <- dataInputHeteroPre()
  inputUser <- dataInputHeteroUser()
  screenList <- c(inputPre, inputUser)
  
  if (input$metaDataHetero) {
    inputPreMeta <- metaDataInputHeteroPre()
    
    if(is.null(input$userFileHetero))
      metaDataTable <- inputPreMeta
    else {
      
      if(is.null(input$userMetaFileHetero)) stop("Please upload a meta-data file")
      
      # validate(need(input$userMetaFile), "Please upload a meta-data file")
      userMetaFileHetero <- input$userMetaFileHetero
      inputUserMeta <- read_excel(userMetaFileHetero$datapath)
      
      # if(dim(inputPreMeta) != dim(inputUserMeta)) stop("The uploaded meta data file is not compatible.")
      # if(dim(inputUser)[1] != dim(inputUserMeta)[1]) stop("The uploaded meta data file is not compatible.")
      # validate(need(dim(inputPreMeta)[2] == dim(inputUserMeta)[2], "The uploaded meta data file is not compatible."))
      # validate(need(dim(inputUser)[1] == dim(inputUserMeta)[1], "The uploaded meta data file is not compatible."))
      
      metaDataTable <- rbind(inputPreMeta, inputUserMeta)
    }
  }
  else
    return(NULL)
})

### Reactive element to combine data from user and pre-loaded
dataHetero <- reactive({
  
  inputPre <- dataInputHeteroPre()
  inputUser <- dataInputHeteroUser()
  screenList <- c(inputPre, inputUser)
})

### Data table output for overview of all studies (plus their meta-data if selected)
output$studyListHetero <- DT::renderDT({
  
  screenList <- dataHetero()
  metaData <- annotateMetaDataHetero()
  
  ### Check if user has uploaded data or selected a pre-loaded dataset
  if(length(screenList) == 0)
    return(NULL)
  else {
    
    if (input$metaDataHetero)
      
      ### Output with meta-data
      overViewTable <- cbind(tibble("Study Number" = 1:length(screenList),
                                    "Study" = names(screenList)),
                             metaData[,-1])
    else
      
      # Output without meta-data
      overViewTable <- tibble("Study Number" = 1:length(screenList),
                              "Study" = names(screenList))
    
    DT::datatable(overViewTable,
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
  }
})

### UI output for showing a list of study items to be selected by user for the rank uniqueness analysis
output$contrastPanel <- renderUI({
  
  screenList <- dataHetero()
  
  validate(need(length(screenList) != 0, "Waiting to choose a dataset ..."))
  
  fluidPage(
    
    multiInput(
      inputId = "groupsHetero", label = strong("Please contrast the two groups:"),
      choices = names(screenList),
      selected = NULL
    ),
    
    tags$hr(),
    
    radioButtons("uniqueMethod", strong("Select rank uniqueness identification method:"),
                 width = '100%',
                 choices = c("Student's T Test" = "TTest",
                             "Robust Rank Aggregation" = "RRA",
                             "Geometric Mean" = "RP",
                             "Mean" = "RS"),
                 selected = "TTest"),
    
    numericInput(inputId = "nTopHetero",
                 label = strong("Specify the maximum rank threshold:"),
                 value = 5000),
    br(),
    actionButton("submitHetero", "Submit!", class = "btn-success")
  )
})

### Reactive element for data cleaning and standardizing the studies
metaScreenHetero <- eventReactive(input$submitHetero, {
  
  screenList <- dataHetero()
  
  ### to check if user only used the pre-loaded datasets so standardizing is not needed
  if(!is.null(input$userFileHetero)) {
    
    for(i in 1:length(screenList)) {
      ### To standardize the Gene name aliases based on official gene symbol
      screenList[[i]][[1]] <- unlist(mapIds(org.Hs.eg.db, keys=screenList[[i]][[1]], column="SYMBOL", keytype="ALIAS", multiVals="first"))
      ### To add the gene rankings to gene names as a new variable
      screenList[[i]][[2]] <- 1:length(screenList[[i]][[1]])
      ### To remove all other columns except Gene name and ranks
      screenList[[i]]<- tibble(screenList[[i]][,1:2])
      ### To remove gene names which could not be attributed to an official gene symbol
      screenList[[i]] <- na.omit(screenList[[i]])
      ### Renaming
      colnames(screenList[[i]]) <- c("Gene", names(screenList[i]))
      ### To remove duplicate gene names if any from the datasets
      screenList[[i]] <- distinct(screenList[[i]], Gene, .keep_all= TRUE)
    }
  }
  screenList
})

### Reactive element to generate contrast class vector
groupHet <- reactive({
  # screenList <- dataHetero()
  screenList <- metaScreenHetero()
  ifelse(names(screenList) %in% input$groupsHetero, 1, 0)
})

### Observer to automatically jump to the unique ranks tab after the user press "submit"
observeEvent(input$submitHetero, {
  updateTabsetPanel(session = session, inputId = "uniquePanel", selected = "Unique ranks")
})

### Reactive element to calculate the unique markers upon submision
rankUniqReact <- eventReactive(input$submitHetero, {
  
  screenList <- metaScreenHetero()
  screenClass <- groupHet()
  
  ### To add the gene rankings to gene names as a new variable
  for(i in 1:length(screenList)) {
    screenList[[i]][[2]] <- c(1:isolate(input$nTopHetero), rep(isolate(input$nTopHetero), length(screenList[[i]][[1]])-isolate(input$nTopHetero)))
  }
  
  ### To check if user has selected the contrast panel before submission
  if(sum(screenClass) == 0) stop("Please select at least one study for each group!")
  
  else {
    
    ### If user has selected only one item in the group
    if(sum(screenClass) == 1) {
      
      ### Calculation of the markers based on RRA
      if(input$uniqueMethod == "RRA") {
        
        tmpComb <- list()
        
        for(i in 1:length(screenList)) {
          tmpComb[[i]] <- screenList[[i]][[1]]
        }
        
        tmpComb <- tmpComb[screenClass != T]
        tmpRankMat <- rankMatrix(tmpComb, N = input$nTopHetero)
        tmpRankMat[tmpRankMat > 1] <- 1
        aggHits <- aggregateRanks(rmat = tmpRankMat, method = "RRA")
        aggHits <- tibble(1:dim(aggHits)[1], aggHits)
        colnames(aggHits) <- c("aggRank", "Gene", "Score")
        
        screenDiff <- aggHits %>% 
          mutate(aggRank = if_else(aggRank < isolate(input$nTopHetero), as.integer(aggRank), isolate(input$nTopHetero))) %>%
          left_join(screenList[[which(screenClass == 1)]], by = "Gene") %>%
          mutate(EffectSize = aggRank - !!as.name(names(screenList[[which(screenClass == 1)]])[2])) %>%
          arrange(desc(EffectSize)) %>% 
          mutate(Rank = 1:length(EffectSize)) %>%
          na.omit() %>%
          dplyr::select(Rank, Gene, EffectSize)
      }
      
      else {
        
        ### Calculation of the markers based on geometric mean
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
            mutate(EffectSize = Rank1 - !!as.name(names(screenList[[which(screenClass == 1)]])[2])) %>%
            arrange(desc(EffectSize)) %>%
            mutate(Rank = 1:length(EffectSize)) %>%
            na.omit() %>%
            dplyr::select(Rank, Gene, EffectSize)
        }
        
        ### Calculation of the markers based on mean
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
            mutate(EffectSize = Rank1 - !!as.name(names(screenList[[which(screenClass == 1)]])[2])) %>%
            arrange(desc(EffectSize)) %>%
            mutate(Rank = 1:length(EffectSize)) %>%
            na.omit() %>%
            dplyr::select(Rank, EffectSize)
        }
        
        if(input$uniqueMethod == "TTest") {
          
          combinedHits <- screenList %>% purrr::reduce(full_join, by = "Gene")
          tmpComb <- combinedHits[,-1]
          tmpComb <- (is.na(tmpComb))*floor(rowMeans(tmpComb, na.rm=TRUE))[row(tmpComb)] + replace(tmpComb, is.na(tmpComb), 0)
          tmpComb1 <- tmpComb[screenClass != T] %>% as.matrix() %>% t()
          tmpComb2 <- tmpComb[screenClass == T] %>% as.matrix() %>% t()
          
          tmpPval <- matrix(data = NA, nrow = dim(tmpComb)[1])
          for (i in 1:dim(tmpComb)[1]) {
            try({
              tmpPval[i] <- unlist(t.test(tmpComb1[, i], tmpComb2[, i], var.equal = T, alternative = "greater")$p.value)
            })
          }
          # tmpPval[is.na(tmpPval)] <- 1
          effectSize <- rowMeans(tmpComb[screenClass != T]) - rowMeans(tmpComb[screenClass == T])
          screenDiff <- cbind(1:nrow(combinedHits),
                              data.frame(combinedHits$Gene, EffectSize, tmpPval, p.adjust(tmpPval)) %>% arrange(desc(EffectSize)))
          
          colnames(screenDiff) <- c("Rank", "Gene", "EffectSize", "pValue", "FDR")
        }
      }
    }
    
    ### If user has selected more than one item in the group
    if(sum(screenClass) > 1) {
      
      ### Calculation of the markers based on RRA
      if(input$uniqueMethod == "RRA") {
        
        tmpComb <- list()
        
        for(i in 1:length(screenList)) {
          tmpComb[[i]] <- screenList[[i]][[1]]
        }
        tmpComb1 <- tmpComb[screenClass != T]
        tmpRankMat1 <- rankMatrix(tmpComb1, N = input$nTopHetero)
        tmpRankMat1[tmpRankMat1 > 1] <- 1
        aggHits1 <- aggregateRanks(rmat = tmpRankMat1, method = "RRA")
        aggHits1 <- tibble(1:dim(aggHits1)[1], aggHits1)
        colnames(aggHits1) <- c("Rank1", "Gene", "Score")
        aggHits1 <- aggHits1 %>%
          mutate(Rank1 = if_else(Rank1 < isolate(input$nTopHetero), as.integer(Rank1), isolate(input$nTopHetero)))
        
        tmpComb2 <- tmpComb[screenClass == T]
        tmpRankMat2 <- rankMatrix(tmpComb2, N = input$nTopHetero)
        tmpRankMat2[tmpRankMat2 > 1] <- 1
        aggHits2 <- aggregateRanks(rmat = tmpRankMat2, method = "RRA")
        aggHits2 <- tibble(1:dim(aggHits2)[1], aggHits2)
        colnames(aggHits2) <- c("Rank2", "Gene", "Score")
        aggHits2 <- aggHits2 %>%
          mutate(Rank2 = if_else(Rank2 < isolate(input$nTopHetero), as.integer(Rank2), isolate(input$nTopHetero)))
        
        screenDiff <- aggHits1 %>%
          left_join(aggHits2, by = "Gene") %>%
          mutate(EffectSize = Rank1 - Rank2) %>%
          arrange(desc(EffectSize)) %>%
          mutate(Rank = 1:length(EffectSize)) %>%
          na.omit() %>%
          dplyr::select(Rank, Gene, EffectSize)
      }
      
      else {
        
        ### Calculation of the markers based on geometric mean
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
            mutate(EffectSize = Rank1 - Rank2) %>%
            arrange(desc(EffectSize)) %>%
            mutate(Rank = 1:length(EffectSize)) %>%
            na.omit() %>%
            dplyr::select(Rank, Gene, EffectSize)
        }
        
        ### Calculation of the markers based on mean
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
            mutate(EffectSize = Rank1 - Rank2) %>%
            arrange(desc(EffectSize)) %>%
            mutate(Rank = 1:length(EffectSize)) %>%
            na.omit() %>%
            dplyr::select(Rank, Gene, EffectSize)
        }
        
        ### Calculation of the markers based on RRA
        if(input$uniqueMethod == "TTest") {
          
          combinedHits <- screenList %>% purrr::reduce(full_join, by = "Gene")
          tmpComb <- combinedHits[,-1]
          tmpComb <- (is.na(tmpComb))*floor(rowMeans(tmpComb, na.rm=TRUE))[row(tmpComb)] + replace(tmpComb, is.na(tmpComb), 0)
          tmpComb1 <- tmpComb[screenClass != T] %>% as.matrix() %>% t()
          tmpComb2 <- tmpComb[screenClass == T] %>% as.matrix() %>% t()
          
          tTestObj <- ttestsModified(tmpComb1, tmpComb2, alternative = "greater")
          EffectSize <- rowMeans(tmpComb[screenClass != T]) - rowMeans(tmpComb[screenClass == T])
          
          # tmpPval <- matrix(data = NA, nrow = dim(tmpComb)[1])
          # for (i in 1:dim(tmpComb)[1]) {
          #   try({
          #     tmpPval[i] <- unlist(t.test(tmpComb1[, i], tmpComb2[, i], var.equal = T, alternative = "greater")$p.value)
          #   })
          # }
          
          # screenDiff <- data.frame(combinedHits$Gene, effectSize, data.frame(tTestObj)$pvalue) %>% arrange(desc(effectSize))
          
          
          screenDiff <- cbind(1:nrow(combinedHits),
                              data.frame(combinedHits$Gene, EffectSize, data.frame(tTestObj)$pvalue, p.adjust(data.frame(tTestObj)$pvalue)) %>%
                                arrange(desc(EffectSize)))
          
          colnames(screenDiff) <- c("Rank", "Gene", "EffectSize", "pValue", "FDR")
        }
      }
    }
  }
  
  screenDiff
  
})

### Data table output for viewing the marker genes
output$tableHetero <- renderDT({
  
  tmpTable <- rankUniqReact()
  datatable(tmpTable,
            
            rownames = F,
            
            options = list(
              ### Aligning all columns text to center
              columnDefs = list(list(className = 'dt-center', targets = "_all")),
              ### Customizing the top colnames row -- black color
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                "}")
            )
  )
})

### Download handler for the marker genes list
output$downloadTable <- downloadHandler(
  
  filename = function() {
    paste0(Sys.Date(), "-RankUniqueness-GeneRaMeN.csv")
  }, 
  content = function(file) {
    
    tmpTable <- rankUniqReact()
    
    write.csv(tmpTable, file, row.names = F)
  }                                     
)

### Reactive element for heatmap presentation of ranks of top amrker genes
heatmapHeteroReact <- reactive({
  
  screenList <- metaScreenHetero()
  screenClass <- groupHet()
  metaData <- annotateMetaDataHetero()
  
  ### To add the gene rankings to gene names as a new variable
  for(i in 1:length(screenList)) {
    screenList[[i]][[2]] <- c(1:isolate(input$nTopHetero), rep(isolate(input$nTopHetero), length(screenList[[i]][[1]])-isolate(input$nTopHetero)))
  }
  
  if(input$uniqueMethod == "TTest") {
    ### Gene markers of first group
    markers1 <- rankUniqReact() %>% 
      filter(pValue < input$pval) %>%
      slice_head(n = input$nHeatmapHeteroUp) %>% 
      dplyr::select(Gene) %>% 
      unlist()
    
    ### Gene markers of second group
    markers2 <- rankUniqReact() %>% 
      filter(pValue > 1 - input$pval) %>%
      slice_tail(n = input$nHeatmapHeteroDown) %>% 
      dplyr::select(Gene) %>% 
      unlist()
  }
  else {
    ### Gene markers of first group
    markers1 <- rankUniqReact() %>%
      slice_head(n = input$nHeatmapHeteroUp) %>% 
      dplyr::select(Gene) %>% 
      unlist()
    
    ### Gene markers of second group
    markers2 <- rankUniqReact() %>%
      slice_tail(n = input$nHeatmapHeteroDown) %>% 
      dplyr::select(Gene) %>% 
      unlist()
  }
  
  ### Merging all datasets into a single dataframe which shows a given gene ranks in each of the studies -- to be used by pheatmap
  combinedHits <- screenList %>% purrr::reduce(full_join, by = "Gene")
  combinedHits <- combinedHits %>% 
    filter(Gene %in% c(markers1, markers2)) %>%
    data.frame()
  rownames(combinedHits) <- combinedHits$Gene
  
  ### Adding meta-data to the annotation (if selected)
  if (input$metaDataHetero) {
    annoteDf <- cbind(metaData[,-1], data.frame(Group = factor(screenClass)))
    rownames(annoteDf) <- colnames(combinedHits[-1])
    
    tmpPlot <- pheatmap(combinedHits[,-1],
                        cluster_rows = input$clustHeatmapRow,
                        cluster_cols = input$clustHeatmapCol,
                        annotation_col = annoteDf,
                        fontsize = 12,
                        border_color = NA,
                        color = colorRampPalette(c("#66b2ff","black"))(200)
                        # display_numbers = TRUE
    )
  }
  else {
    annoteDf <- data.frame(Group = factor(screenClass))
    rownames(annoteDf) <- colnames(combinedHits[-1])
    
    tmpPlot <- pheatmap(combinedHits[,-1],
                        cluster_rows = input$clustHeatmapRow,
                        cluster_cols = input$clustHeatmapCol,
                        fontsize = 12,
                        annotation_col = annoteDf,
                        border_color = NA,
                        color = colorRampPalette(c("#66b2ff","black"))(200)
                        # display_numbers = TRUE
    )
  }
  
  plotObj$heatmapHetero <- tmpPlot
  tmpPlot
})

### Heatmap output visualization
output$heatmapHetero <- renderPlot(heatmapHeteroReact())

### Reactive element to dynamically change the size of the heatmap based on numbers of heats selected to be shown
plotHeightHetero <- reactive(200 + (20*(input$nHeatmapHeteroUp+input$nHeatmapHeteroDown)))

### UI output for the heatmap to show top markers of each group
output$heatmapHeteroUI <- renderUI({
  plotOutput("heatmapHetero", height = plotHeightHetero()) %>% withSpinner(type = getOption("spinner.type", default = 4))
})

### Download handler for the heatmap -- PDF
output$heatmapHeteroPDF <- downloadHandler(
  filename = function() {
    paste0(Sys.Date(), "-RankUniqueness-GeneRaMeN.pdf")
  },
  content = function(file){
    pdf(file, width=input$wHeatmapHetero, height=input$hHeatmapHetero)
    print(plotObj$heatmapHetero)
    dev.off()
  }
)

### Download handler for the heatmap -- PNG
output$heatmapHeteroPNG <- downloadHandler(
  filename = function() {
    paste0(Sys.Date(), "-RankUniqueness-GeneRaMeN.png")
  },
  content = function(file){
    png(file, width=input$wHeatmapHetero, height=input$hHeatmapHetero, units="in", res=input$ppiHeatmapHetero)
    print(plotObj$heatmapHetero)
    dev.off()
  }
)

### Download handler for the heatmap -- TIFF
output$heatmapHeteroTIFF <- downloadHandler(
  filename = function() {
    paste0(Sys.Date(), "-RankUniqueness-GeneRaMeN.tiff")
  },
  content = function(file){
    tiff(file, width=input$wHeatmapHetero, height=input$hHeatmapHetero, units="in", res=input$ppiHeatmapHetero)
    print(plotObj$heatmapHetero)
    dev.off()
  }
)