################################################################################
######################## Gene-RaMeN server function ############################
################################################################################

server <- function(input, output, session) {
  
  ### Setting limits for the size of file user is allowed to submit -- 50 MB
  options(shiny.maxRequestSize = 50*1024^2)
  
  ### Reactive conductor to read the selected pre-loaded dataset from server
  dataInputPre <- reactive({
    screenList <- list()
    if(input$study == "None")
      return(screenList)
    else
      tmpName <- paste0(input$study, ".rds")
    screenList <- readRDS(paste0("Data/", tmpName))
  })
  
  ### Reactive conductor to read the input excel file from the user
  dataInputUser <- reactive({
    userFile <- input$userFile
    # validate(need(tools::file_ext(userFile$datapath) == "xlsx", "Please upload an excel file"))
    if(is.null(userFile))
      return(list())
    else
      userFile$datapath %>% readxl::excel_sheets() %>% purrr::set_names() %>% map(read_excel, path = userFile$datapath)
  })
  
  ### Listing table of studies for overview
  output$studyList <- DT::renderDT({
    
    inputPre <- dataInputPre()
    inputUser <- dataInputUser()
    screenList <- c(inputPre, inputUser)
    
    validate(need(length(screenList) != 0, "Waiting to submit a dataset ..."))
    
    # Check for Null value
    # if(length(screenList) == 0)
    #   return(NULL)
    # else
    DT::datatable(tibble("Screen Number" = 1:length(screenList),
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
  
  ### Reactive conductor for cleaning/standardizing the screening data
  metaScreen <- eventReactive(input$submit, {
    
    inputPre <- dataInputPre()
    inputUser <- dataInputUser()
    screenList <- c(inputPre, inputUser)
    
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
  
  ### Reactive conductor to generate a data-frame containing all studies' gene names -- to be used by RRA and GO
  listCombinedGene <- reactive({
    
    screenList <- metaScreen()
    tmpComb <- list()
    
    for(i in 1:length(screenList)) {
      tmpComb[[i]] <- screenList[[i]][[1]]
    }
    
    names(tmpComb) <- names(screenList)
    tmpComb
  })
  
  ### Reactive conductor to calculate the robust ranks by RRA or geometric mean
  aggHitsRRA <- eventReactive(input$submit, {
    
    req(input$aggMethod %in% c('RRA', 'geom.mean'))
    
    tmpComb <- listCombinedGene()
    aggHits <- aggregateRanks(tmpComb, N = input$nTop, method = input$aggMethod)
    aggHits[,'Score'] <- -log10(aggHits[, 'Score'])
    aggHits <- tibble(1:dim(aggHits)[1], aggHits)
    colnames(aggHits) <- c("Aggregated Rank", "Gene", "Score")
    aggHits
  })
  
  ### Reactive conductor to calculate the robust ranks by BIRRA
  aggHitsBIRRA <- eventReactive(input$submit, {
    
    req(input$aggMethod == 'BIRRA')
    
    screenList <- metaScreen()
    screenList <- lapply(screenList, slice_head, n=input$nTop)
    combinedHits <- screenList %>% purrr::reduce(full_join, by = "Gene")
    combinedHits[is.na(combinedHits)] <- input$nTop
    rownames(combinedHits) <- combinedHits$Gene
    aggHits <- BIRRA(combinedHits[,-1])
    aggHits <- order(aggHits, decreasing = T)
    aggHits <- combinedHits[aggHits, 1]
    aggHits <- tibble("Aggregated Rank" = 1:dim(aggHits)[1], aggHits)
  })
  
  ### Data table showing the aggregated ranks
  output$RRA <- DT::renderDT({
    
    if (isolate(input$aggMethod) %in% c("RRA", "geom.mean")) {
      aggHits <- aggHitsRRA()
      aggHits <- aggHits %>% dplyr::filter(Score > 0)
    } else
      aggHits <- aggHitsBIRRA()

    aggHits$ENTREZ <- mapIds(org.Hs.eg.db, keys=aggHits$Gene, column="ENTREZID", keytype="SYMBOL", multiVals="first")

    datatable(aggHits %>%
                mutate("Links" = paste0("<a href='https://orcs.thebiogrid.org/Gene/", ENTREZ, "'>BioGrid</a> - ",
                                        "<a href='https://www.ncbi.nlm.nih.gov/gene/", ENTREZ, "'>NCBI</a> - ",
                                        "<a href='https://www.genecards.org/cgi-bin/carddisp.pl?gene=", Gene, "'>GeneCards</a>")) %>%
                dplyr::select(-ENTREZ),
              
              rownames = F,
              options = list(
                # Aligning all columns text to center
                columnDefs = list(list(className = 'dt-center', targets = "_all")),
                # Customizing the top colnames row -- black color
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                  "}")
              ),
              escape = FALSE)
    
  })
  
  ### Download handler for the aggregated ranks data from the table
  output$downloadRRA = downloadHandler(
    
    filename = function() {
      paste0(Sys.Date(), "-geneRaMeN.csv")
    }, 
    content = function(file) {
      if (isolate(input$aggMethod) %in% c("RRA", "geom.mean"))
        aggHits <- aggHitsRRA()
      else
        aggHits <- aggHitsBIRRA()
      
      write.csv(aggHits, file, row.names = F)
    }                                     
  )
  
  ### Reactive conductors to be used for scatter plot download
  plotObj <- reactiveValues()
  
  ### Reactive conductors for the scatter plot
  scPlotReact <- reactive({
    
    validate(
      need(isolate(input$aggMethod) != 'BIRRA', 'Scatter plot can not be visualized for the results of "BIRRA" algorithm.')
    )
    
    aggHits <- aggHitsRRA()
    tmpPlot <- aggHits %>%
      mutate(Random=sample(seq(1, length(Score))),
             TopHit=ifelse(Score > Score[input$nScPlotTop],'Yes', 'No')) %>%
      ggplot(aes(Random, Score, label= Gene)) +
      geom_point(size=5,aes(color=TopHit)) + 
      geom_text_repel(aes(label=ifelse(TopHit=='Yes',Gene,'')), size = 5) + 
      theme_classic() +
      xlab("Genes") +
      ylab("-log10(RRA Score)")
    
    plotObj$scatterPlot <- tmpPlot
    tmpPlot
  })
  
  ### Scatter plot output visualization
  output$scatterPlot <- renderPlot(scPlotReact())
  
  ### Download handlers for the RRA aggregated ranks scatter plot -- Panel 1
  output$downloadPlotPDF <- downloadHandler(
    
    filename = function() {
      paste0(Sys.Date(), "-geneRaMeN.pdf")
    },
    content = function(file){
      pdf(file, width=input$w, height=input$h, units="in")
      print(plotObj$scatterPlot)
      dev.off()
    }
  )
  ### Download handlers for the RRA aggregated ranks scatter plot -- Panel 2
  output$downloadPlotPNG <- downloadHandler(
    
    filename = function() {
      paste0(Sys.Date(), "-geneRaMeN.png")
    }, 
    content = function(file){
      png(file,width=input$w,height=input$h,units="in",res=input$ppi)
      print(plotObj$scatterPlot)
      dev.off()
    }
  )
  ### Download handlers for the RRA aggregated ranks scatter plot -- Panel 3
  output$downloadPlotJPEG <- downloadHandler(
    
    filename = function() {
      paste0(Sys.Date(), "-geneRaMeN.jpeg")
    }, 
    content = function(file){
      jpeg(file,width=input$w,height=input$h,units="in",res=input$ppi)
      print(plotObj$scatterPlot)
      dev.off()
    }
  )
  ### Download handlers for the RRA aggregated ranks scatter plot -- Panel 4
  output$downloadPlotTIFF <- downloadHandler(
    
    filename = function() {
      paste0(Sys.Date(), "-geneRaMeN.tiff")
    },
    content = function(file){
      tiff(file,width=input$w,height=input$h,units="in",res=input$ppi)
      print(plotObj$scatterPlot)
      dev.off()
    }
  )
  
  ### Reactive conductor merging all ranks per gene in a single df -- to be used as table output and heatmap 
  allRanks <- reactive({
    
    if (isolate(input$aggMethod) %in% c("RRA", "geom.mean"))
      aggHits <- aggHitsRRA()
    else
      aggHits <- aggHitsBIRRA()
    
    screenList <- metaScreen()
    # Appending robust rank as a study to the list of all studies
    screenList <- append(screenList, list(dplyr::select(aggHits, "Gene", "Aggregated Rank")), after = 0)
    combinedHits <- screenList %>% purrr::reduce(full_join, by = "Gene") %>% arrange("Aggregated Rank")
    combinedHits
  })
  
  ### Showing all the ranks from individual studies combined
  output$rankFinder <- DT::renderDT({
    
    datatable(allRanks(), 
              rownames = F,
              options = list(
                scrollX = TRUE,
                scrollCollapse=TRUE,
                # Aligning all columns text to center
                columnDefs = list(list(className = 'dt-center', targets = "_all")),
                # Customizing the top colnames row -- black color
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                  "}"))
    )
  })
  
  # Heatmap presentation of ranks of all top genes plus clustering
  heatmapReact <- reactive({
    combinedHits <- data.frame(allRanks())
    rownames(combinedHits) <- combinedHits$Gene
  
    tmpPlot <- combinedHits %>% 
      slice_head(n = input$nHeatmap) %>%
      dplyr::select(-Gene, -"Aggregated.Rank") %>%
      pheatmap(cluster_rows = F,
               fontsize = 12,
               border_color = NA,
               color = colorRampPalette(c("#66b2ff","black"))(200)
               # cellheight = 15
               )

    # # Uncomment for a ggplot2 graphic alternative
    # combinedHits$Gene <- fct_rev(factor(combinedHits$Gene, levels = combinedHits$Gene))
    # tmpPlot <- combinedHits %>%
    #   slice_head(n = input$nHeatmap) %>%
    #   pivot_longer(cols = -"Gene", names_to = "Study", values_to = "Ranks") %>%
    #   ggplot(aes(Study, Gene)) +
    #   geom_tile(aes(fill = Ranks)) +
    #   scale_fill_continuous(high = "black", low = "#56B1F7") +
    #   ggpubr::rotate_x_text(angle = 45)

    plotObj$heatmap <- tmpPlot
    tmpPlot
  })
  
  ### Heatmap output visualization
  output$heatmap <- renderPlot(heatmapReact())
  
  plotHeight <- reactive(100 + (20*input$nHeatmap))
  output$heatmapUI <- renderUI({
    plotOutput("heatmap", height = plotHeight())
  })
  
  output$plot.ui <- renderUI({
    plotOutput("plots", height = plotHeight())
  })
  
  ### Download handlers for the heatmap
  output$heatmapPDF <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "-geneRaMeN.pdf")
    },
    content = function(file){
      pdf(file, width=input$wHeat, height=input$h, units="in")
      print(plotObj$heatmap)
      dev.off()
    }
  )
  output$heatmapPNG <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "-geneRaMeN.png")
    },
    content = function(file){
      png(file,width=input$w,height=input$h,units="in",res=input$ppi)
      print(plotObj$heatmap)
      dev.off()
    }
  )
  output$heatmapJPEG <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "-geneRaMeN.jpeg")
    },
    content = function(file){
      jpeg(file,width=input$w,height=input$h,units="in",res=input$ppi)
      print(plotObj$heatmap)
      dev.off()
    }
  )
  output$heatmapTIFF <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "-geneRaMeN.tiff")
    },
    content = function(file){
      tiff(file,width=input$w,height=input$h,units="in",res=input$ppi)
      print(plotObj$heatmap)
      dev.off()
    }
  )

}
