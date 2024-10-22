<!-- badges: start -->
WIP
<!-- badges: end -->

# Siberia
## Siberia Reconstruction Scripts

This repository is a supplementary material for:
*Andreev R.A., Novenko E.Yu. (2024) ...*

This RStudio Project provide a continuous workflow for reconstructions, given in ... All of them should be compiled one by one and ran completely. Scripts return:

* code outputs (output/performance)
 
* transfer functions performance metrics and p-values (by [palaeoSig](https://github.com/richardjtelford/palaeoSig)) in environment as 	"summary" table as well as in nice table format (output/summary)
 
* scatterplots: fitted&observed, fitted&residuals for each model (plots)
 
* correlation matrices between reconstructions (plots)

* nice summary tables

## Code Contributor:

Rodion Andreev [dorionio40@gmail.com](mailto:dorionio40@gmail.com)

* [![ORCID](https://img.shields.io/badge/ORCID-0000--0003--0443--9849-brightgreen)](https://orcid.org/0000-0003-0443-9849)
* ![Static Badge](https://img.shields.io/badge/%D0%98%D0%A1%D0%A2%D0%98%D0%9D%D0%90-239760642-orange?link=https%3A%2F%2Fistina.msu.ru%2Fworkers%2F239760642%2F)

## How to use:

Two options:

1. Start a RStudio project and compile scripts one by one. We recommend to compile them completely.

2. Uncomment a "setwd(..." line in 01_data_loading.R, add your path and compile scripts one by one

## How to cite:

WIP

## System requirements:

This project was run on R version 4.3.3 (2024-02-29)

Platform: x86_64-apple-darwin20 (64-bit)

Running under: macOS Sonoma 14.5

### List of packages we used

```r
 [1] tidypaleo_0.1.3    lubridate_1.9.3    forcats_1.0.0      stringr_1.5.1     
 [5] dplyr_1.1.4        purrr_1.0.2        readr_2.1.5        tidyr_1.3.1       
 [9] tibble_3.2.1       tidyverse_2.0.0    ggcorrplot_0.1.4.1 ggplot2_3.5.0
```
