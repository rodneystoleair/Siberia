## This script provides data for the training (modern) and the prediction
## (fossil) from .xlsx sheets,
## as well as present data for the palaeoenvironmental reconstructions. Also
## the script does preparation of loaded tables.
## Provided scripts are written to perform reconstructions for multiple
## cores and environmental parameters.
## Excel sheets names containing data should follow this format:
## Fossil data: 'fossil_%core_name%.xlsx'
## Chronology: 'ages_%core_name%.xlsx'
## Training set: 'modern.xlsx'
## Reconstruction parameters, e.g. climate: 'climate.xlsx'
## But you are free to change these names by code re-writing.

# 0. Starting: if you're not using a RStudio project
# setwd('.../Siberia reconstruction/')

# 1. Dependencies ----
library('readxl')
library('writexl')
library('tidyverse')
source('R/loading_functions.R')

# 2. Data loading ---- 
# 2.1. Loading from xlsx-sheets

# Fossil data (prediction set)
name = readline('Write the exact core name for a reconstruction: ')

param_type = readline('Press 1 for climate, 2 for woody cover')
if (param_type == 1){
  param_type2 = 'climate'
} else if (param_type == 2){
  param_type2 = 'woody'
} else {
  cat('Wrong parameter type')
}

fossil = read_excel(paste0('data/fossil/fossil_', name, '.xlsx'))
  # gather(variable, value, -depth) |>
  # spread(depth, value) |>
  # transform(variable = as.numeric(variable)) |>
  # arrange(variable)
depth = fossil$depth
ages = read_excel(paste0('data/fossil/ages_', name, '.xlsx'), col_names = F) |> 
  pull()

# Modern data (training set)
modern = read_excel('data/modern/modern.xlsx',
                    col_types = c('text', rep('numeric', 51))) |>
  as.data.frame() |>
  arrange(points)

# Parameters
# Woody cover
cover = read_excel('data/parameters/woody_cover.xlsx',
                   col_types = c('text', rep('numeric', 4), 'text', 'text')) |>
  as.data.frame() |>
  arrange(points)

# Climate
climate = read_excel('data/parameters/climate.xlsx',
                     col_types = c('text', rep('numeric', 4))) |> 
  arrange(points)

## Biome selection, specific for data used in paper, because biome data of our 
## training set locates with modern woody cover data (cover.xlsx).
## Everything specific for paper data and unnecessarily used for a custom data 
## is marked with doubled hash tag.
modern = left_join(modern,
                   cover,
                   by = 'points') |>
  select(-km50:-type1) |>
  filter(!str_detect(type2, 'putorana')) |>
  select(-type2)
cover = select(cover, -type1:-type2)

## Column names switch
row.names(modern) = modern$points

if (param_type2 == 'climate'){
  parameters_modern = semi_join(climate, modern, by = 'points') |>
    inner_join(cover) |>
    select(-km50, -km10, -km20, -km5)
} else if (param_type2 == 'woody'){
  parameters_modern = semi_join(climate, modern, by = 'points') |>
    inner_join(cover) |>
    select(points, km50, km20, km10, km5)
} else {
  cat('Didnt find parameter type')
}

parameters_types = colnames(parameters_modern)[-1]

modern = select(modern, -points)

# Excluding unnecessary taxa. You can select them. 
fossil = fossil |>
  select(-`Menuanthes trifoliata`, -`Cyperaceae`)
modern = modern |>
  select(-`Menuanthes trifoliata`, -`Cyperaceae`)

# Count percentages of each taxa for modern and fossil datasets
modern = modern / rowSums(modern) * 100
fossil = fossil / rowSums(fossil) * 100

row.names(fossil) = depth
fossil = select(fossil, -depth)

# Data frame creation for final reconstructions. They will be bounded to each
# other in the end of work.
recons = data.frame(depth)
