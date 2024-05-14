[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![GPLv2 license](https://img.shields.io/badge/License-GPLv2-blue.svg)](http://perso.crans.org/besson/LICENSE.html)

# GeneRaMeN
Gene Rank Meta aNalyzer (GeneRaMeN) is an R/Shiny app designed for integration, comparison, and meta analysis of multiple ranked gene lists, with a primary focus on genome-wide CRISPR screening outputs. GeneRaMeN consists of three main functionalities: 

1. Rank aggregation: It enables the user to combine multiple ranked gene lists into one consensus list, using rank aggregation algorithms like RRA. *It brings us what is **homogenous** across our datasets*.

2. Rank uniqueness: It enables the user to compare a subset of the ranked gene lists with the rest, basically using rank aggregation algorithms on each of the subsets and then comparing them with eachother. *It brings us what is **heterogenous** across our datasets*.

3. Rank correlation: It enables the user to observe correlation/anti-correlations in gene ranks, finding genes that show a similar or opposite trend with a gene of interest.

GeneRaMeN can be accessed on our [shinyapps web server](https://ysolab.shinyapps.io/GeneRaMeN/), or be downloaded from here and run locally.
