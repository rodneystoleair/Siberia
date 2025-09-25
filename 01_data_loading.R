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

# Modern data (training set)
modern = read_excel('data/modern/modern_empd_example.xlsx') |>
  as_tibble() |>
  arrange(points)

# Parameters
# Climate
climate = read_excel('data/parameters/climate_empd.xlsx') |> 
  as_tibble() |> 
  arrange(points)

## Params join
parameters_modern = semi_join(climate, modern, by = 'points')

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
