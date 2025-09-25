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

# 2. Data loading ---- 
# 2.1. Loading from xlsx-sheets

# Fossil data (prediction set)
name = readline('Write the exact core name for a reconstruction: ')
fossil = read_excel(paste0('data/fossil/fossil_', name, '.xlsx')) |>
  gather(variable, value, -depth) |>
  spread(depth, value) |>
  transform(variable = as.numeric(variable)) |>
  arrange(variable)
depth = fossil$variable
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
row.names(fossil) = fossil$variable
colnames(fossil) = colnames(modern)

# parameters_modern = semi_join(climate, modern, by = 'points') |>
#   inner_join(cover) |>
#  select(-km50, -km10, -km20, -km5)

## Params join
parameters_modern = semi_join(climate, modern, by = 'points') |>
  inner_join(cover) |>
  select(points, km50, km20, km10, km5)

parameters_types = colnames(parameters_modern)[-1]

fossil = select(fossil, -points)
modern = select(modern, -points)

# Excluding unnecessary taxa. You can select them. 
fossil = fossil |> 
  select(-`Menuanthes trifoliata`, -`Cyperaceae`)
modern = modern |> 
  select(-`Menuanthes trifoliata`, -`Cyperaceae`)

# Data frame creation for final reconstructions. They will be bounded to each
# other in the end of work.
recons = data.frame(depth)

igarka_GMLevkovskaya = neotoma2::get_sites(sitename = 'Igarka Peat Exposure') |>
  neotoma2::get_downloads() |>
  neotoma2::samples() |>
  select(age, units, value, variablename, depth) |>
  pivot_wider(names_from = variablename,
              values_from = value) |> 
  remove_na() |> 
  # turn all NA values to 0
  mutate(across(where(is.numeric), ~replace_na(., 0)))
