################################################################################
################################################################################
#########                                                             ##########
#########        GeneRaMeN server function for rank correlation       ##########
#########                                                             ##########
################################################################################
################################################################################

### Reactive element to read the pre-loaded datasets
dataInputPreCorr <- reactive({
  screenList <- list()
  if(input$studyCorr == "None")
    return(screenList)
  else
    tmpName <- paste0(input$studyCorr, ".rds")
  screenList <- readRDS(paste0("Data/PresetData/", tmpName))
})

### Reactive element to read the meta data for the pre-loaded datasets - selected in the UI
metaDataInputPreCorr <- reactive({
  metaDataPre <- NULL
  if(input$studyCorr == "None")
    return(metaDataPre)
  else
    tmpName <- paste0(input$studyCorr, "_meta.xlsx")
  inputPreMeta <- read_excel(paste0("Data/PresetMetaData/", tmpName))
})

## Reactive element to read the input data file from the user
dataInputUserCorr <- reactive({
  userFile <- input$userFileCorr
  # validate(need(tools::file_ext(userFile$datapath) == "xlsx", "Please upload an excel file"))
  if(is.null(userFile))
    return(list())
  else
    userFile$datapath %>% readxl::excel_sheets() %>% purrr::set_names() %>% map(read_excel, path = userFile$datapath)
})

### Reactive element to read the input meta-data file from the user (if needed), and combine it with the preset meta-data (if needed)
annotateMetaDataCorr <- reactive({
  inputPre <- dataInputPreCorr()
  inputUser <- dataInputUserCorr()
  screenList <- c(inputPre, inputUser)
  
  if (input$metaDataCorr) {
    inputPreMeta <- metaDataInputPreCorr()
    
    if(is.null(input$userFileCorr))
      metaDataTable <- inputPreMeta
    else {
      
      if(is.null(input$userMetaFileCorr)) stop("Please upload a meta-data file")
      
      # validate(need(input$userMetaFile), "Please upload a meta-data file")
      userMetaFile <- input$userMetaFileCorr
      inputUserMeta <- read_excel(userMetaFile$datapath)
      
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

### Data table output for overview of all studies (plus their meta-data if selected)
output$studyListCorr <- DT::renderDT({
  
  inputPreCorr <- dataInputPreCorr()
  inputUserCorr <- dataInputUserCorr()
  screenList <- c(inputPreCorr, inputUserCorr)
  metaDataTableCorr <- annotateMetaDataCorr()
  
  ### Check if user has uploaded data or selected a pre-loaded dataset
  if(length(screenList) == 0)
    return(NULL)
  else {
    
    ### Output with meta-data
    if (input$metaDataCorr) {
      overViewTable <- cbind(tibble("Study Number" = 1:length(screenList),
                                    "Study" = names(screenList)),
                             metaDataTableCorr[,-1])
    }
    else
      
      ### Output without meta-data
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

# ### Data table output for overview of all studies (plus their meta-data if selected)
# output$studyListCorr <- DT::renderDT({
#   inputPreCorr <- dataInputPreCorr()
#   inputUserCorr <- dataInputUserCorr()
#   screenList <- c(inputPreCorr, inputUserCorr)
#   # metaDataTableCorr <- annotateMetaDataCorr()
#   
#   ### Check if user has uploaded data or selected a pre-loaded dataset
#   if(length(screenList) == 0)
#     return(NULL)
#   else {
#     
#     ### Output with meta-data
#     # if (input$metaDataCorr) {
#     #   overViewTable <- cbind(tibble("Study Number" = 1:length(screenList),
#     #                                 "Study" = names(screenList)),
#     #                          metaDataTable[,-1])
#     # }
#     # else
#     
#     ### Output without meta-data
#     overViewTable <- tibble("Study Number" = 1:length(screenList),
#                             "Study" = names(screenList))
#     
#     DT::datatable(overViewTable,
#                   rownames = F,
#                   
#                   options = list(
#                     # Aligning all columns text to center
#                     columnDefs = list(list(className = 'dt-center', targets = "_all")),
#                     # Customizing the top colnames row -- black color
#                     initComplete = JS(
#                       "function(settings, json) {",
#                       "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
#                       "}")
#                   )
#     )
#   }
# })

### Reactive conductor for cleaning/standardizing the screening data
metaScreenCorr <- eventReactive(input$submitCorr, {
  
  inputPreCorr <- dataInputPreCorr()
  inputUserCorr <- dataInputUserCorr()
  screenList <- c(inputPreCorr, inputUserCorr)
  
  if(!is.null(input$userFileCorr)) {
    
    for(i in 1:length(screenList)) {
      # To standardize the Gene name aliases based on official gene symbol
      screenList[[i]][[1]] <- unlist(AnnotationDbi::mapIds(org.Hs.eg.db::org.Hs.eg.db, keys=screenList[[i]][[1]], column="SYMBOL", keytype="ALIAS", multiVals="first"))
      # To add the gene rankings to gene names as a new variable
      screenList[[i]][[2]] <- 1:length(screenList[[i]][[1]])
      # To remove all other columns except Gene name and ranks
      screenList[[i]]<- tidyr::tibble(screenList[[i]][,1:2])
      # To remove gene names which could not be attributed to an official gene symbol
      screenList[[i]] <- na.omit(screenList[[i]])
      # Renaming
      colnames(screenList[[i]]) <- c("Gene", names(screenList[i]))
      # To remove duplicate gene names if any from the datasets
      screenList[[i]] <- dplyr::distinct(screenList[[i]], Gene, .keep_all= TRUE)
    }
  }
  screenList
})

### Reactive conductor merging all ranks per gene in a single df -- to be used as table output and heatmap 
allRanksCorr <- reactive({
  
  screenList <- metaScreenCorr()
  combinedHits <- screenList %>% purrr::reduce(full_join, by = "Gene")
  combinedHits
})

### Observer to automatically jump to the correlation ranks tab after the user press "submit"
observeEvent(input$submitCorr, {
  updateTabsetPanel(session = session, inputId = "corrPanel", selected = "Correlation plot")
})

### Reactive element to calculate the gene correlations
rankCorrReact <- reactive({
  
  combinedHits <- allRanksCorr()
  
  combinedHitsTmp <- combinedHits[,-1]
  combinedHitsTmp[combinedHitsTmp > input$nTopCorr] <- input$nTopCorr
  rankCorr <- WGCNA::cor(as.matrix(t(combinedHitsTmp)),
                         use = "everything",
                         # method = input$corrMethod
                         method = "pearson")
})

### UI output for prompting user to select a gene
output$corrGeneSelect <- renderUI({
  
  combinedHits <- allRanksCorr()
  
  fluidPage(
    fluidRow(
      column(6,
             
             pickerInput(
               inputId = "geneCorrID",
               label = "Pick your gene of interest:", 
               choices = base::sort(combinedHits$Gene), 
               selected = NULL,
               multiple = F,
               choicesOpt = list(
                 disabled = base::sort(combinedHits$Gene) %in% (combinedHits %>%
                                                                  filter_all(any_vars(is.na(.))) %>%
                                                                  dplyr::select(Gene) %>%
                                                                  unlist()
                 )
               ),
               options = pickerOptions(liveSearch = TRUE))
      ),
      column(6,
             p(class = 'text-center', downloadButton('downloadCorr', 'Download table!')),
      ),
    ),
    fluidRow(
      helpText(
        "Correlations calculation is disabled for genes with missing values."
      )
    )
  )
})

### UI output for prompting user to select a gene
output$corrGeneSelectLine <- renderUI({
  
  combinedHits <- allRanksCorr()
  
  fluidRow(
    column(4,
           pickerInput(
             inputId = "geneCorrIDLine",
             label = "Pick your gene of interest:", 
             choices = base::sort(combinedHits$Gene), 
             selected = input$geneCorrID,
             multiple = F,
             choicesOpt = list(
               disabled = base::sort(combinedHits$Gene) %in% (combinedHits %>%
                                                                filter_all(any_vars(is.na(.))) %>%
                                                                dplyr::select(Gene) %>%
                                                                unlist()
               )
             ),
             options = pickerOptions(liveSearch = TRUE))
    ),
    column(4,
           sliderInput("nCorrLine", "Number of top correlated genes to be visualized:",
                       min = 0, max = 20, value = 5)),
    column(4,
           sliderInput("nAntiCorrLine", "Number of top anti-correlated genes to be visualized:",
                       min = 0, max = 20, value = 5)),
  )
})

### UI output for prompting user to select a gene
output$corrGeneSelectHeat <- renderUI({
  
  combinedHits <- allRanksCorr()
  
  fluidRow(
    column(4,
           
           pickerInput(
             inputId = "geneCorrIDHeat",
             label = "Pick your gene of interest:", 
             choices = base::sort(combinedHits$Gene), 
             selected = input$geneCorrID,
             multiple = F,
             choicesOpt = list(
               disabled = base::sort(combinedHits$Gene) %in% (combinedHits %>%
                                                                filter_all(any_vars(is.na(.))) %>%
                                                                dplyr::select(Gene) %>%
                                                                unlist()
               )
             ),
             options = pickerOptions(liveSearch = TRUE))
    ),
    column(4,
           sliderInput("nCorrHeat", "Number of top correlated genes to be visualized:",
                       min = 0, max = 20, value = 5)),
    column(4,
           sliderInput("nAntiCorrHeat", "Number of top anti-correlated genes to be visualized:",
                       min = 0, max = 20, value = 5)),
  )
})

corrAntiCorrList <- reactive({
  
  rankCorr <- rankCorrReact()
  combinedHits <- allRanksCorr()
  combinedHitsCorr <- data.frame("Gene" = combinedHits$Gene,
                                 "CorCoeficient" = rankCorr[, which(combinedHits$Gene %in% input$geneCorrID)])
  
})

# corrAntiCorrListRanks <- reactive({
#   
#   combinedHitsCorr <- corrAntiCorrList()
#   combinedHits <- allRanksCorr()
#   combinedHitsTmp <- combinedHits[,-1]
#   combinedHitsTmp[combinedHitsTmp > input$nTopCorr] <- input$nTopCorr
#   combinedHits <- data.frame("Gene" = combinedHits$Gene, combinedHitsTmp)
#   
#   corrAntiCorr <- combinedHits |>
#     pivot_longer(-Gene, names_to = "Study", values_to = "Rank") #%>%
#     # mutate(hits = case_when(
#     #   Gene == input$geneCorrID ~ input$geneCorrID,
#     #   Gene %in% unlist(arrange(combinedHitsCorr, CorCoeficient) %>% dplyr::select(Gene) %>% slice_head(n = input$nAntiCorr)) ~ paste(input$geneCorrID, "AntiCor"), 
#     #   Gene %in% unlist(arrange(combinedHitsCorr, desc(CorCoeficient)) %>% dplyr::filter(!Gene %in% input$geneCorrID) %>% dplyr::select(Gene) %>% slice_head(n = input$nCorr)) ~ paste(input$geneCorrID, "Cor"),
#     #   .default = "Background")) %>%
#     # mutate(hits = factor(hits, levels = c(input$geneCorrID, paste(input$geneCorrID, "Cor"), paste(input$geneCorrID, "AntiCor"), "Background")))
#   
# })

rowCallback <- c(
  "function(row, data){",
  "  for(var i=0; i<data.length; i++){",
  "    if(data[i] === null){",
  "      $('td:eq('+i+')', row).html('NA')",
  "        .css({'color': 'rgb(151,151,151)', 'font-style': 'italic'});",
  "    }",
  "  }",
  "}"  
)

output$corrList <-  DT::renderDT({
  combinedHitsCorr <- corrAntiCorrList()
  combinedHitsCorr <- dplyr::arrange(combinedHitsCorr, desc(CorCoeficient)) %>%
    dplyr::filter(Gene != input$geneCorrID) %>%
    dplyr::mutate("correlation ranks" = 1:(dim(combinedHitsCorr)[1] - 1), .before = Gene)
  
  DT::datatable(combinedHitsCorr,
                rownames = FALSE,
                options = list(
                  # Aligning all columns text to center
                  columnDefs = list(list(className = 'dt-center', targets = "_all")),
                  rowCallback = JS(rowCallback),
                  # Customizing the top colnames row -- black color
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                    "}")
                )
  )
})

output$antiCorrList <-  DT::renderDT({
  combinedHitsCorr <- corrAntiCorrList()
  combinedHitsCorr <- dplyr::arrange(combinedHitsCorr, CorCoeficient) %>%
    dplyr::filter(Gene != input$geneCorrID) %>%
    dplyr::mutate("anti-correlation ranks" = 1:(dim(combinedHitsCorr)[1] -1), .before = Gene)
  
  DT::datatable(combinedHitsCorr,
                rownames = FALSE,
                options = list(
                  # Aligning all columns text to center
                  columnDefs = list(list(className = 'dt-center', targets = "_all")),
                  rowCallback = JS(rowCallback),
                  # Customizing the top colnames row -- black color
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                    "}")
                )
  )
})

### Download handler for the correlations data for the selected gene
output$downloadCorr <- downloadHandler(
  
  filename = function() {
    paste0(Sys.Date(), "-", input$geneCorrID, "-correlation-geneRaMeN.csv")
  }, 
  content = function(file) {
    combinedHitsCorr <- corrAntiCorrList()
    corrList <- dplyr::arrange(combinedHitsCorr, desc(CorCoeficient)) %>%
      dplyr::filter(Gene != input$geneCorrID) %>%
      dplyr::mutate("correlation ranks" = 1:(dim(combinedHitsCorr)[1] - 1), .before = Gene)
    write.csv(corrList, file, row.names = F)
  }                                     
)

corPlotReact <- reactive({
  
  rankCorr <- rankCorrReact()
  combinedHits <- allRanksCorr()
  combinedHitsCorr <- data.frame("Gene" = combinedHits$Gene,
                                 "CorCoeficient" = rankCorr[, which(combinedHits$Gene %in% input$geneCorrIDLine)])
  
  combinedHitsTmp <- combinedHits[,-1]
  combinedHitsTmp[combinedHitsTmp > input$nTopCorr] <- input$nTopCorr
  combinedHits <- data.frame("Gene" = combinedHits$Gene, combinedHitsTmp)
  
  corrAntiCorr <- combinedHits |>
    pivot_longer(-Gene, names_to = "Study", values_to = "Rank") %>%
    mutate(hits = case_when(
      Gene == (input$geneCorrIDLine) ~ input$geneCorrIDLine,
      Gene %in% unlist(arrange(combinedHitsCorr, CorCoeficient) %>% dplyr::select(Gene) %>% slice_head(n = input$nAntiCorrLine)) ~ paste(input$geneCorrIDLine, "AntiCor"), 
      Gene %in% unlist(arrange(combinedHitsCorr, desc(CorCoeficient)) %>% dplyr::filter(!Gene %in% input$geneCorrIDLine) %>% dplyr::select(Gene) %>% slice_head(n = input$nCorrLine)) ~ paste(input$geneCorrIDLine, "Cor"),
      .default = "Background")) %>%
    mutate(hits = factor(hits, levels = c(input$geneCorrIDLine, paste(input$geneCorrIDLine, "Cor"), paste(input$geneCorrIDLine, "AntiCor"), "Background")))
  
  # tmpPlot <-  ggplot() + 
  #   geom_line(data = corrAntiCorr %>% filter(hits == "Background"),
  #             aes(x = Study, y = log10(Rank), group = Gene), 
  #             color="grey", 
  #             alpha = 0.4) +
  #   geom_line(data = corrAntiCorr %>% filter(hits != "Background"),
  #             aes(x = Study, y = log10(Rank), group = Gene, color = hits)) + 
  #   theme_bw() +
  #   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  
  tmpPlot  <- plot_ly(corrAntiCorr %>% filter(hits != "Background") %>% group_by(Gene), x = ~Study, y = ~log10(Rank), color = ~hits) 
  tmpPlot <- tmpPlot %>% add_lines(mode = 'lines+markers',
                                   # connectgaps = TRUE,
                                   hovertext = ~ paste0("Gene: ", Gene, "\nStudy: ", Study, "\nRank: ", Rank),
                                   hoverinfo = "text")
  
  plotObj$corLine <- tmpPlot
  tmpPlot
})

output$corPlot <- renderPlotly(corPlotReact())

### Heatmap presentation of ranks of all top genes plus clustering
corrHeatmap <- reactive({
  
  combinedHits <- allRanksCorr()
  rankCorr <- rankCorrReact()
  corrList <- data.frame("Gene" = combinedHits$Gene,
                         "CorCoeficient" = rankCorr[, which(combinedHits$Gene %in% input$geneCorrIDHeat)])
  
  combinedHitsTmp <- combinedHits[,-1]
  combinedHitsTmp[combinedHitsTmp > input$nTopCorr] <- input$nTopCorr
  combinedHits <- data.frame("Gene" = combinedHits$Gene, combinedHitsTmp)
  
  corrGenes <- corrList %>% arrange(desc(CorCoeficient)) %>% dplyr::filter(!Gene %in% input$geneCorrIDHeat) %>% slice_head(n=input$nCorrHeat) %>% dplyr::select(Gene) %>% unlist() %>% unname()
  antiCorrGenes <- corrList %>% arrange(CorCoeficient) %>% slice_head(n=input$nAntiCorrHeat) %>% dplyr::select(Gene) %>% unlist() %>% unname()
  
  combinedHitsHeatmapCorr <- combinedHits %>% filter(Gene %in% c(input$geneCorrIDHeat, corrGenes, antiCorrGenes)) %>%
    mutate(Gene = factor(Gene, levels = c(input$geneCorrIDHeat, corrGenes, antiCorrGenes))) %>% 
    arrange(Gene)
  
  HeatmapCorrTemp <- combinedHitsHeatmapCorr[,-1]
  rownames(HeatmapCorrTemp) <- combinedHitsHeatmapCorr$Gene
  
  
  annote_df <- data.frame("Gene" = combinedHitsHeatmapCorr$Gene, 
                          "Cluster" = c("Selected Gene", 
                                        rep("Correlated Genes", input$nCorrHeat), 
                                        rep("Anti-Correlated Genes", input$nAntiCorrHeat)))
  rownames(annote_df) <- annote_df$Gene
  annote_df <- annote_df %>% dplyr::select(Cluster)
  
  if (input$metaDataCorr) {
    annoteDfCol <- annotateMetaDataCorr()
    
    tmpNames <- unlist(annoteDfCol[, 1])
    annoteDfCol <- data.frame(annoteDfCol[, -1])
    rownames(annoteDfCol) <- colnames(HeatmapCorrTemp)
    
    tmpPlot <- pheatmap(HeatmapCorrTemp, 
                        cluster_rows = FALSE, 
                        gaps_row = c(1, (input$nCorrHeat +1)), 
                        annotation_row = annote_df,
                        annotation_col = annoteDfCol,
                        fontsize = 13,
                        border_color = NA,
                        show_rownames = T,
                        na_col = "grey70", 
                        color = colorRampPalette(c("yellow2","black"))(200),
                        # color = colorRampPalette(c("#66b2ff","black"))(200),
                        display_numbers = FALSE)
  }
  else {
    tmpPlot <- pheatmap(HeatmapCorrTemp, 
                        cluster_rows = FALSE, 
                        gaps_row = c(1, (input$nCorrHeat +1)), 
                        annotation_row = annote_df,
                        fontsize = 13,
                        border_color = NA,
                        show_rownames = T,
                        na_col = "grey70", 
                        color = colorRampPalette(c("yellow2","black"))(200),
                        # color = colorRampPalette(c("#66b2ff","black"))(200),
                        display_numbers = FALSE)
  }
  
  plotObj$heatmapCorr <- tmpPlot
  tmpPlot
})

### Heatmap output visualization
output$heatmapCorr <- renderPlot(corrHeatmap())

plotHeightCorr <- reactive(250 + (20*(input$nCorrHeat + input$nAntiCorrHeat)))

output$heatmapCorrUI <- renderUI({
  fluidPage(
    plotOutput("heatmapCorr", height = plotHeightCorr()) %>% withSpinner(type = getOption("spinner.type", default = 4)),
    br(),
    hr(),
    fluidRow(column(3, numericInput("wHeatmapCorr", label = "Width", value = 10)),
             column(3, numericInput("hHeatmapCorr", label = "Height", value = 10)),
             column(3, numericInput("ppiHeatmapCorr", label = "Resolution", value = 300)),
             column(3, dropdown(
               downloadBttn(
                 outputId = "heatmapCorrPDF",
                 label="PDF",
                 color = "default",
                 style = "fill",
                 size='sm',
                 block=TRUE
               ),
               downloadBttn(
                 outputId = "heatmapCorrPNG",
                 label="PNG",
                 color = "default",
                 style = "fill",
                 size='sm',
                 block=TRUE
               ),
               downloadBttn(
                 outputId = "heatmapCorrTIFF",
                 label="TIFF",
                 color = "default",
                 style = "fill",
                 size='sm',
                 block=TRUE
               ),
               circle=FALSE,
               label="Download plot",
               status="default"
             )
             )
    )
  )
})

### Download handlers for the heatmap
output$heatmapCorrPDF <- downloadHandler(
  filename = function() {
    paste0(Sys.Date(), "-geneRaMeN.pdf")
  },
  content = function(file){
    pdf(file, width=input$wHeatmapCorr, height=input$hHeatmapCorr)
    print(plotObj$heatmapCorr)
    dev.off()
  }
)
output$heatmapCorrPNG <- downloadHandler(
  filename = function() {
    paste0(Sys.Date(), "-geneRaMeN.png")
  },
  content = function(file){
    png(file, width=input$wHeatmapCorr, height=input$hHeatmapCorr, units="in", res=input$ppiHeatmapCorr)
    print(plotObj$heatmapCorr)
    dev.off()
  }
)
output$heatmapCorrTIFF <- downloadHandler(
  filename = function() {
    paste0(Sys.Date(), "-geneRaMeN.tiff")
  },
  content = function(file){
    tiff(file, width=input$wHeatmapCorr, height=input$hHeatmapCorr, units="in", res=input$ppiHeatmapCorr)
    print(plotObj$heatmapCorr)
    dev.off()
  }
)

