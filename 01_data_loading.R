## This script provides data for the training (modern) and the prediction
## (fossil) sets from NeotomaDB or from .xlsx sheets,
## as well as present data for the palaeoenvironmental reconstructions. Also
## the script does preparation of loaded tables.
## Provided scripts are written to perform reconstructions for multiple
## cores and environmental parameters.

# 1. Dependencies ----
# Packages loading
library('readxl')
library('writexl')
library('flextable')
library('neotoma2')
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
depth = rownames(fossil)
ages = read_excel(paste0('data/fossil/ages_', name, '.xlsx'), col_names = F) |> 
  pull()

# Modern data (training set)
modern = read_excel('data/modern/modern_new.xlsx',
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

# Biome selection
modern = left_join(modern,
                   cover,
                   by = 'points') |>
  select(-km50:-type1) |>
  filter(!str_detect(type2, 'putorana')) |>
  select(-type2)
cover = select(cover, -type1:-type2)

# Column names switch
row.names(modern) = modern$points
row.names(fossil) = fossil$variable
colnames(fossil) = colnames(modern)

parameters_modern = semi_join(climate, modern, by = 'points') |>
  inner_join(cover) |>
  select(-km50, -km10, -km5)

# parameters_modern = semi_join(climate, modern, by = 'points') |>
#   inner_join(cover) |>
#   select(points, km50, km20, km10, km5)

parameters_types = colnames(parameters_modern)[-1]

fossil = select(fossil, -points)
modern = select(modern, -points)

# Data frame creation for final reconstructions. They will be bound to each
# other in the ends of reconstructions.
recons = data.frame(depth)

# 2.2. Loading from NeotomaDB
