################################################################################
################################################################################
#########                                                             ##########
#########               GeneRaMeN UI for tutorials tab                ##########
#########                                                             ##########
################################################################################
################################################################################

tabPanel("Tutorial",
         
         # CSS script for enabling separate scroll bar for each side of line
         tags$style(
           HTML(".scrollable-column {
           height: 700px;
           overflow-y: auto;
                }")
         ),
         
         # CSS script to put a line in between the two main sections (columns)
         tags$head(
           tags$style(
             HTML(".divider-line {
             border-right: 1px solid #ccc;
             height: 100%;
             margin-right: 1px;
                  }")
           )
         ),
         
         fluidRow(
           
           # Rank aggregation tutorial column
           column(6,
                  fluidRow(
                    column(12,
                           class = "scrollable-column",
                           h2(strong("Rank Aggregation Analysis")),
                           br(),
                           p(strong("1. Selecting datasets:")),
                           p("If you wish to use any of the pre-loaded screening datasets as demo, just click on the button next to the dataset name. If you wish to upload and analyze your own dataset you can select 'None' and proceed to the next section 'uploading your own dataset'. Please note that in case you do select a pre-loaded dataset and proceed to upload your data as well, GeneRaMeN would assume that you want to append your data and will combine it with the selected dataset."),
                           br(),
                           tags$img(src = "Asset 2.png",
                                    width = "60%",
                                    style = "display: block; margin-left: auto; margin-right: auto;"),
                           br(),
                           p(strong("2. Uploading your own dataset:")),
                           p("GeneRaMeN allows the user to upload one excel file for the input ranked gene lists, containing any number of sheets. Each sheet needs to correspond to a separate ranked gene list which you want to be included in the analysis. There is no limitation on the format of each list as long as the first column contains the 'Gene names/Gene symbols' (Gene summary output from MAGeCK can readily be pasted into the excel sheets and used). Please note that this list must already be SORTED. Since GeneRaMeN reads the first column as gene rankings it is agnostic to whether your current ranking is correctly sorted or not, therefore it is on the user to make sure all hit lists are correctly sorted. To download an example of data file templates accepted by GeneRaMeN please click",
                             a("here", href = "https://github.com/MeisamYSF/GeneRaMeN/raw/main/SampleDataFile_GeneRaMeN.xlsx")),
                           p("Please note that GeneRaMeN currently accepts Human gene lists, as all gene lists undergo standardization to their latest HGNC official gene name."),
                           br(),
                           tags$img(src = "Asset 3.png",
                                    width = "60%",
                                    style = "display: block; margin-left: auto; margin-right: auto;"),
                           br(),
                           p(strong("3. Including meta-data:")),
                           p("Additional information like cell line, disease phenotype, etc. can be supplemented to the studies included in the analysis. Meta-data such as cell line and screening method is included for the pre-loaded datasets, but in case the user has opted to upload their own datasets and want to visualize their custom meta-data, they must upload a meta-data file which is compatible with the uploaded dataset (same number of studies, and with same names). To download an example of meta-data file template acceptable by GeneRaMeN please click",
                             a("here", href = "https://github.com/MeisamYSF/GeneRaMeN/raw/main/SampleMetaDataFile_GeneRaMeN.xlsx")),
                           br(),
                           tags$img(src = "Asset 4.png",
                                    width = "60%",
                                    style = "display: block; margin-left: auto; margin-right: auto;"),
                           br(),
                           p(strong("4. Rank aggregation settings and submission:")),
                           p("Once the input datasets are provided, the user has to determine which rank aggregation method they want to use. Robust rank aggregation (RRA) is recommended.",
                             "It is strongly recommended to put a cut-off for the maximum rank in the analysis to ensure all rankings have biological meanings, however, if the user does not want to put such cap, they can set it to the total number of genes and keep all original ranks included. The default value is set at 5000. When all fields are filled press the submit button to run rank aggregation on your data.",
                             "Note that regardless of the maximum rank value all genes are going to be included for the analysis. The maximum rank cut-off determines that all genes beyond the certain value should be assumed to all have the same maximum rank."),
                           br(),
                           tags$img(src = "Asset 5.png",
                                    width = "60%",
                                    style = "display: block; margin-left: auto; margin-right: auto;"),
                           br(),
                           p(strong("5. Input overview:")),
                           p("Once the input data is selected/uploaded by the user, they can readily see an overview of the studies that they are about to analyze using GeneRaMeN. If meta-data is included that information for each study would also be displayed."),
                           br(),
                           tags$img(src = "Asset 6.png",
                                    width = "80%",
                                    style = "display: block; margin-left: auto; margin-right: auto;"),
                           br(),
                           p(strong("6. Aggregated ranks list:")),
                           p("Upon clicking the submit button the user is guided to the “Aggregated ranks” tab in which the results of the rank aggregation is shown. The data table includes gene names, their aggregated ranks and scores as calculated by the method of choice, as well as links for the gene pages in main databases including BioGRID, NCBI, GeneCards, and DGIdb. The aggregated ranks and scores of genes of interest could be searched through the search box. The entire table can be downloaded in csv format by clicking on the 'Download table!' button on top."),
                           br(),
                           tags$img(src = "Asset 7.png",
                                    width = "80%",
                                    style = "display: block; margin-left: auto; margin-right: auto;"),
                           br(),
                           p(strong("6. Scatter plot visualization of aggregated ranks:")),
                           p("Genetic screening results are normally visualized through scatter plots in which the y axis corresponds to the enrichment/significance scores. The number of top genes to be highlighted and labled across the plot can be fine-tuned by the bar at the top. Plots can be downloaded in desired sizes and formats (PNG, PDF, and JPEG) from the download panel at the bottom."),
                           br(),
                           tags$img(src = "Asset 8.png",
                                    width = "80%",
                                    style = "display: block; margin-left: auto; margin-right: auto;"),
                           br(),
                           p(strong("7. Gene rank finder:")),
                           p("The data table includes the original rankings of each gene across all datasets. This table can be searched for genes of interest via the search box."),
                           br(),
                           tags$img(src = "Asset 9.png",
                                    width = "80%",
                                    style = "display: block; margin-left: auto; margin-right: auto;"),
                           br(),
                           p(strong("8. Heatmap showing the original ranks of top aggregated genes:")),
                           p("The original rankings of the top aggregated genes can be visualized via heatmaps. The studies would be clustered as well to see which lists were more similar to each other regarding the particular set of top aggregated genes. If meta-data are included, they’d be shown as annotation bars at the top of the heatmap. The number of top genes to be included can be fine-tuned by the bar at the top. Plots can be downloaded in desired sizes and formats (PNG, PDF, and JPEG) from the download panel at the bottom."),
                           br(),
                           tags$img(src = "Asset 10.png",
                                    width = "80%",
                                    style = "display: block; margin-left: auto; margin-right: auto;"),
                           br(),
                           p(strong("9. Gene ontology and pathway enrichment analysis of top aggregated genes:")),
                           p("Select the number of top aggregated genes to be included in the analysis at top left. GeneRaMeN can submit these genes as queries to the g:profiler database to perform gene ontology (Biological process, Molecular function, Cellular compartment) and KEGG pathway enrichment analysis. P-value threshold of the overrepresentation analysis can be adjusted by user to filter results significance based on their desired cut-off. Results would be shown in data tables containing term name and size, as well as its enrichment p value and precision and a list of enriched genes associated with each term. The entire table can be downloaded in csv format by clicking on the “Download table!” button on top. These results are also shown as bubble plots, which can be downloaded in desired sizes and formats (PNG, PDF, and JPEG) from the download panel at the bottom."),
                           br(),
                           tags$img(src = "Asset 11.png",
                                    width = "80%",
                                    style = "display: block; margin-left: auto; margin-right: auto;"),
                           br(),
                           ),
                    
                    # vertical line
                    # column(
                    #   1,
                    #   div(class = "divider-line")
                    # ),
                  )),
           
           # Rank uniqueness tutorial column
           column(6,
                  class = "scrollable-column",
                  h2(strong("Rank Uniqueness Analysis")),
                  br(),
                  p(strong("1. Selecting datasets:")),
                  p("If you wish to use any of the pre-loaded screening datasets as demo, just click on the button next to the dataset name. If you wish to upload and analyze your own dataset you can select 'None' and proceed to the next section “uploading your own dataset”.",
                    "Please note that in case you do select a pre-loaded dataset and proceed to upload your data as well, GeneRaMeN would assume that you want to append your data and will combine it with the selected dataset."),
                  br(),
                  tags$img(src = "Asset 2.png",
                           width = "60%",
                           style = "display: block; margin-left: auto; margin-right: auto;"),
                  br(),
                  p(strong("2. Uploading your own dataset:")),
                  p("GeneRaMeN allows the user to upload one excel file for the input ranked gene lists, containing any number of sheets. Each sheet needs to correspond to a separate ranked gene list which you want to be included in the analysis. There is no limitation on the format of each list as long as the first column contains the “Gene names/Gene symbols” (Gene summary output from MAGeCK can readily be pasted into the excel sheets and used). Please note that this list must already be SORTED. Since GeneRaMeN reads the first column as gene rankings it is agnostic to whether your current ranking is correctly sorted or not, therefore it is on the user to make sure all hit lists are correctly sorted. To download an example of data file templates accepted by GeneRaMeN please click",
                    a("here", href = "https://github.com/MeisamYSF/GeneRaMeN/raw/main/SampleDataFile_GeneRaMeN.xlsx"),
                    "Please note that GeneRaMeN currently accepts Human gene lists, as all gene lists undergo standardization to their latest HGNC official gene name."),
                  br(),
                  tags$img(src = "Asset 3.png",
                           width = "60%",
                           style = "display: block; margin-left: auto; margin-right: auto;"),
                  br(),
                  p(strong("3. Including meta-data:")),
                  p("Additional information like cell line, disease phenotype, etc. can be supplemented to the studies included in the analysis. Meta-data such as cell line and screening method is included for the pre-loaded datasets, but in case the user has opted to upload their own datasets and want to visualize their custom meta-data, they must upload a meta-data file which is compatible with the uploaded dataset (same number of studies, and with same names). To download an example of meta-data file template acceptable by GeneRaMeN please click ",
                    a("here", href = "https://github.com/MeisamYSF/GeneRaMeN/raw/main/SampleMetaDataFile_GeneRaMeN.xlsx"),
                    "Note that this step is OPTIONAL and for visualization purposes only (see outputs sections in the following), thus has no impact on the analysis outcome."),
                  br(),
                  tags$img(src = "Asset 4.png",
                           width = "60%",
                           style = "display: block; margin-left: auto; margin-right: auto;"),
                  br(),
                  p(strong("4. Contrast panel:")),
                  p("Upon selection/upload of the dataset, GeneRaMeN prompts the user to divide the studies in two groups to be analyzed for uniqueness markers. To select a study just click on its name in the widget. At least one study needs to be selected."),
                  br(),
                  tags$img(src = "Asset 13.png",
                           width = "60%",
                           style = "display: block; margin-left: auto; margin-right: auto;"),
                  br(),
                  p(strong("5.  Rank uniqueness settings and submission:")),
                  p("Once the input datasets are provided, the user must determine which rank uniqueness method they want to use. Multiple one-way two sample t tests are default method but users may opt for parallel rank aggregations using any of the rank aggregation methods as above section.",
                    "It is strongly recommended to put a cut-off for the maximum rank in the analysis to ensure all rankings have biological meanings, however, if the user does not want to put such cap, they can set it to the total number of genes and keep all original ranks included. The default value is set at 5000. When all fields are filled press the submit button to run rank uniqueness on your data.",
                    "Note that regardless of the maximum rank value all genes are going to be included for the analysis. The maximum rank cut-off determines that all genes beyond the certain value should be assumed to all have the same maximum rank."),
                  br(),
                  tags$img(src = "Asset 14.png",
                           width = "60%",
                           style = "display: block; margin-left: auto; margin-right: auto;"),
                  br(),
                  p(strong("6. Input overview:")),
                  p("Once the input data is selected/uploaded by the user, they can readily see an overview of the studies that they are about to analyze using GeneRaMeN. If meta-data is included that information for each study would also be displayed."),
                  br(),
                  tags$img(src = "Asset 15.png",
                           width = "80%",
                           style = "display: block; margin-left: auto; margin-right: auto;"),
                  br(),
                  p(strong("7. Unique ranks list:")),
                  p("Upon clicking the submit button the user is guided to the “Unique ranks” tab in which the results of the rank uniqueness are shown. The data table includes gene names and their unique ranks as well as effect sizes and p values depending on the method of choice. The aggregated ranks and scores of genes of interest could be searched through the search box. The entire table can be downloaded in csv format by clicking on the “Download table!” button on top.",
                    "Note that the unique ranks are for the selected subset (right group on the contrast panel) and are sorted descending based on the effect sizes. In case you would like to see the top unique genes of the unselected group you can sort the effect sizes ascending (by clicking on the arrows next to the column name)."),
                  br(),
                  tags$img(src = "Asset 16.png",
                           width = "80%",
                           style = "display: block; margin-left: auto; margin-right: auto;"),
                  br(),
                  p(strong("8. Heatmap showing the original ranks of top unique genes:")),
                  p("The original rankings of the top unique genes can be visualized via heatmaps. The number of top unique genes to be included for each subset can be fine-tuned by the bar at the top. The heatmap would be clustered for both rows and columns (genes and studies) by default but user can change these parameters. Since the top unique genes are selected based on effect sizes the user has the option to filter genes below any desired p value cutoff. If meta-data are included, they’d be shown as annotation bars at the top of the heatmap. Plots can be downloaded in desired sizes and formats (PNG, PDF, and JPEG) from the download panel at the bottom."),
                  br(),
                  tags$img(src = "Asset 17.png",
                           width = "80%",
                           style = "display: block; margin-left: auto; margin-right: auto;"),
                  br(),
           ),
         )
)