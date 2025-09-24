<!-- badges: start -->

This repository is a supplementary material for: R. Andreev, E. Novenko. Quantitative pollen-based reconstructions of climate characteristics and forest coverage for Northern Central Siberia: evaluation of different techniques

[![](https://img.shields.io/badge/DOI-10.1016%2Fj.quaint.2025.109879-green)](https://doi.org/10.1016/j.quaint.2025.109879)

<!-- badges: end -->

# Siberia

## Quantitative palaeocological reconstructions

This RStudio project provides a workflow for reconstruction performance, as given in the research paper. All of them should be compiled one by one and run completely.

Scripts return:

-   code outputs (output/performance)

-   performance metrics of transfer functions, and p-values (by [palaeoSig](https://github.com/richardjtelford/palaeoSig)) in the environment as a "summary" table

-   scatter plots: fitted & observed, fitted & residuals for each model (plots), scree plots for the Modern Analogue Technique k number of analogues

-   correlation matrices between reconstructions (plots)

## Code Contributor

Rodion Andreev

[dorionio40\@gmail.com](mailto:dorionio40@gmail.com)

-   [![ORCID](https://img.shields.io/badge/ORCID-0000--0003--0443--9849-brightgreen)](https://orcid.org/0000-0003-0443-9849)

## How to use

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

## How to cite

R. Andreev, E. Novenko. Quantitative pollen-based reconstructions of climate characteristics and forest coverage for Northern Central Siberia: evaluation of different techniques. Quaternary International, Volume 739, 2025, 109879. <https://doi.org/10.1016/j.quaint.2025.109879>

## Acknowledgements

The studies were supported by grant of the Ministry of Science and Higher Education of Russian Federation (agreement № 075-15-2024-554 of April 24, 2024).

## System preferences

This project was run on R version 4.5.1

Platform: x86_64-apple-darwin20 (64-bit)

Running under: macOS Sonoma 14.5
