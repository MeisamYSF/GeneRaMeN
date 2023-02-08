################################################################################
####################### Gene-RaMeN ui for tutorials tab ########################
################################################################################

tabPanel("Tutorial",
         p(strong("Selecting datasets:")),
         br(),
         p("We have already provided several pre-loaded datasets comprising numerous CRISPR/Cas9 or haploid genetic screenings which aimed to answer the same biological question. If you wish to use any of these datasets just click on the button next to its name. If you wish to upload and analyze your own dataset you can select 'None'. Please note that in case you do select a preloaded dataset and proceed to upload your data as well, the application would assume that you want to append your data to the preloaded dataset and will combine the data."),
         br(),
         p(strong("Custom dataset uploaded by user:")),
         br(),
         p("Currently the application only accepts one excel file to be uploaded as the custom dataset by the user. This excel file would consist of multiple sheets with each sheet containing the final ranked results from a screen you wish to include in your analysis. Note that in each of the sheets, the first column must be the 'Gene name' or 'Symbol' of the hits, and this must be already SORTED. As Gene-RaMeN is agnostic to whether your current ranking is correct or not, it is on user to double check that all hit lists are correctly sorted."),
         br(),
         p(strong("Number of top hits to be considered")),
         br(),
         p("The Gene-RaMeN app calculates aggregated ranks based on RRA algorithm. While you can opt in to include the entire ranking lists from each of the screens to be fed to the algorithm (in case of human genome-wide screenings this would be ~20,000), it is strongly recommended to put a cut-off for the number of hits you include. This is due to the fact that in most of the original bioinformatic tools used to generate the ranks, the final list would include all the genes anyway, so the actual ranking beyond a certain scoring threshold does not have any biological meanings, in some cases it would be alphabetical. The more stringent the selection is the less number of Hits should be included for robust ranking. The default value set for the app is to consider top 2000 genes, which means if a gene is not ranked in the top 2000 hits of ANY of the screens it would not be included in rank aggregation")
)