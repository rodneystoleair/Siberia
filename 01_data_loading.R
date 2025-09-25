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

# 1. Dependencies ----
library('readxl')
library('writexl')
library('tidyverse')
source('R/loading_functions.R')

# 2. Data loading ---- 
# 2.1. Loading from xlsx-sheets

# Fossil data: Igarka Peat Exposure from Neotoma
# Левковская Г.В., Кинд Н.В., Завельский Ф.С., Форова В.С.
# Абсолютный возраст торфяников района г. Игарка и расчленение голоцена Западной
# Сибири // Бюллетень Комиссии по изучению четвертичного периода № 39. 1970. 
fossil = neotoma2::get_sites(sitename = 'Igarka Peat Exposure') |>
  neotoma2::get_downloads() |>
  neotoma2::samples() |>
  select(age, value, variablename, depth) |>
  pivot_wider(names_from = variablename,
              values_from = value) |> 
  remove_na() |> 
  # turn all NA values to 0
  mutate(across(where(is.numeric), ~replace_na(., 0))) |> 
  mutate(Pinus = `Pinus sylvestris` + `Pinus sibirica`) |> 
  select(-`Pinus sylvestris`, -`Pinus sibirica`) |> 
  rename(`Betula sect. Albae` = `Betulaceae undiff.`,
         `Abies sibirica` = Abies) |> 
  # Arrange columns alphabetically except depth
  select(depth, sort(tidyselect::peek_vars())) |> 
  as.data.frame()

# Ages
ages = fossil |> 
  select(depth, age) |>
  arrange(depth)
fossil = fossil |> 
  select(-age)

# Modern data (training set)
modern = read_excel('data/modern/modern_empd_example.xlsx') |>
  as.data.frame() |>
  arrange(points)

# Filtering taxa by the taxa list
taxa_list = c('Abies sibirica', 'Alnus', 'Artemisia', 'Betula sect. Albae',
              'Cyperaceae', 'Ericaceae', 'Larix', 'Picea', 'Pinus', 'Poaceae',
              'Salix')

fossil = fossil |> 
  select(depth, all_of(taxa_list))
modern = modern |> 
  select(points, all_of(taxa_list))

row.names(modern) = modern$points
modern = modern |> select(-points)
row.names(fossil) = fossil$depth
fossil = fossil |> select(-depth)

# Parameters
# Climate
parameters_modern = read_excel('data/modern/climate_empd.xlsx') |>
  as.data.frame() |>
  arrange(points)

parameters_types = colnames(parameters_modern)[-1] # excluding points

# Data frame creation for final reconstructions. They will be bounded to each
# other in the end of work.
recons = tibble(depth = ages$depth)
