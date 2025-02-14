<!-- badges: start -->

WIP

This repository is a supplementary material for: *Andreev R., Novenko E., 2025. Quantitative pollen-based reconstructions of climate characteristics and forest coverage for Northern Central Siberia: evaluation of different techniques*

<!-- badges: end -->

# Siberia

## Quantitative palaeocological reconstructions

This RStudio project provides a workflow for reconstruction performance, as given in the research paper. All of them should be compiled one by one and run completely.

Scripts return:

-   code outputs (output/performance)

-   performance metrics of transfer functions, and p-values (by [palaeoSig](https://github.com/richardjtelford/palaeoSig)) in the environment as a "summary" table

-   scatter plots: fitted & observed, fitted & residuals for each model (plots), scree plots for the Modern Analogue Technique k number of analogues

-   correlation matrices between reconstructions (plots)

## Code Contributor:

Rodion Andreev

[dorionio40\@gmail.com](mailto:dorionio40@gmail.com)

-   [![ORCID](https://img.shields.io/badge/ORCID-0000--0003--0443--9849-brightgreen)](https://orcid.org/0000-0003-0443-9849)

## How to use:

Two options:

-   Start a RStudio project and compile scripts one by one. We recommend compiling them completely.

-   Uncomment a "setwd(..." line in 01_data_loading.R, add your path and compile scripts one by one.

    For now, the repository works with the given data, original data from the paper is not yet provided (except for EMPD2 modern pollen samples). In order to use it with custom data, you should load it as Excel sheets (.xlsx) in directories:

1.  **Fossil data**: `data/fossil/fossil_%object_name%.xlsx`
2.  **Chronology**: `data/fossil/ages_%object_name%.xlsx`
3.  **Modern training set**: `data/modern/modern.xlsx`
4.  **Ecological parameters (e.g. climate) for training set samples**: `data/parameters/climate.xlsx`
5.  **Meteorological data for comparison**: store it in `data/modern/`

You can find examples of table formatting in the corresponding directories.

## How to cite:

**The paper is under submission.**

*Andreev R., Novenko E., 2025. Quantitative pollen-based reconstructions of climate characteristics and forest coverage for Northern Central Siberia: evaluation of different techniques*

## System preferences:

This project was run on R version 4.3.3 (2024-02-29)

Platform: x86_64-apple-darwin20 (64-bit)

Running under: macOS Sonoma 14.5

### List of packages we used

``` r
 [1] tidypaleo_0.1.3    lubridate_1.9.3    forcats_1.0.0      stringr_1.5.1     
 [5] dplyr_1.1.4        purrr_1.0.2        readr_2.1.5        tidyr_1.3.1       
 [9] tibble_3.2.1       tidyverse_2.0.0    ggcorrplot_0.1.4.1 ggplot2_3.5.0
```
