# This script provide postprocessing of reconstruction results. They proceed
# several procedures:
#   1. Data handling for plotting
#   2. Stratigraphical plotting
#   3. Correlation analysis between fitted values
#   4. Nice summary table formatting
# Visualisation relies on pre-recorded parameter names, divided into two groups:
# ordinary and woody. If you want to add some more, you need to write their
# full names for plots :)

# 0. Dependencies ----
# Packages loading
library('ggcorrplot')
library('flextable')
library('tidyverse')
library('tidypaleo')
source('R/plotting_functions.R')
source('R/summary_functions.R')

# 1. Data loading ----
## Loading the data frame with reconstructions 
recons = readxl::read_xlsx(paste0('output/reconstructed/', name, '_',
                                  param_type2,
                                  '.xlsx')) |> 
  mutate(depth = as.character(depth))

## If you need to load new ages
# median = read_delim('data/fossil/NizhnyayaTunguska_52_ages.txt') |> 
#   select(depth, median) |> 
#   mutate(depth = as.character(depth))
# median

# recons = recons |> 
#   select(-ages) |>
#   mutate(depth = as.character(depth)) |> 
#   left_join(median, by = 'depth') |> 
#   relocate(median, .after = depth) |> 
#   rename(ages = median)

# 2. Data handling ----
# Converting to longer format
if (param_type2 == 'climate'){
  
  recon_plot = recons |> 
    pivot_longer(contains(c('mat', 'wa', 'wapls', 'rf')), names_to = 'type', 
                 values_to = 'fitted') |> 
    transform(depth = as.numeric(depth)) |> 
    transform(ages = abs(as.numeric(ages))) |> 
    separate(type, into = c('type1', 'type2'), sep = '\\.')
  
  labels = c(
    'T_jan' = 'Tjan, °C',
    'T_jul' = 'Tjuly, °C',
    'T_ann' = 'Tann, °C',
    'P_ann' = 'Pann, mm',
    'km20' = 'Woody cover, 20 km')
  
} else if (param_type2 == 'woody'){
  
  recon_plot = recons |> 
    pivot_longer(contains(c('mat', 'wa', 'wapls', 'rf')), names_to = 'type', 
                 values_to = 'fitted') |> 
    transform(depth = as.numeric(depth)) |> 
    transform(ages = abs(as.numeric(ages))) |> 
    separate(type, into = c('type1', 'type2'), sep = '\\.') |> 
    mutate(type1 = fct_relevel(type1, c('km50', 'km20', 'km10', 'km5')))
  
  labels = c(
    'km50' = '50 km',
    'km20' = '20 km',
    'km10' = '10 km',
    'km5' = '5 km')
  
} else {
  cat('Didnt find parameter type')
}

# SSE
# recon_plot = recons |>
#   select(contains('T_jul.')) |>
#   mutate(ci_lower = T_jul.wapls - 1.96 * T_jul.wapls.sse,
#          ci_upper = T_jul.wapls + 1.96 * T_jul.wapls.sse)
# 
# recon_plot = recons |>
#   select(contains('km5.')) |>
#   mutate(ci_lower = km5.wapls - 1.96 * km5.wapls.sse,
#          ci_upper = km5.wapls + 1.96 * km5.wapls.sse)

age_breaks = c(seq(0, 9000, 250))
depth_breaks = c(seq(0, 200, 25))

adm_data = tibble(depth = as.numeric(recons$depth), age = ages)
adm = age_depth_model(adm_data,
                      depth = depth,
                      age = age)

# 3. Plotting ----
## 3.1. Single plotting with Sample Specific Errors ----

# ggplot(recon_plot, aes(x = T_jul.wapls,
#                        y = ages)) +
#   # Draw the shadowed confidence interval area first
#   geom_errorbar(aes(xmin = T_jul.wapls - T_jul.wapls.sse, 
#                     xmax = T_jul.wapls + T_jul.wapls.sse), 
#                 size = 0.1) +
#   # Draw the main prediction line on top
#   geom_lineh(size = 0.3) +
#   geom_vline(aes(xintercept = xintercept),
#              data = tibble(type1 = 'T_jul', xintercept = 16.1),
#              linetype = 6, size = 0.4, color = 'red') +
#     scale_y_reverse()  +
#     labs(x = 'Reconstructed',
#          y = 'Age cal BP') +
#     scale_color_discrete(name = 'Model',
#                          labels = c('WAPLS')) +
#     theme(legend.position = 'bottom') +
#     theme_paleo() +
#     rotated_axis_labels(45)

## 3.2. Stratigraphical vertical plot ----
vert_plot = ggplot(recon_plot, aes(x = fitted,
                       y = depth,
                       color = type2,
                       group = type2)) +
  geom_lineh(size = 0.3) +
  scale_y_reverse()  +
  facet_geochem_gridh(vars(type1),
                      labeller = labeller(type1 = labels)) +
  labs(x = 'Reconstructed',
       y = 'Depth, cm') +
  scale_color_discrete(name = 'Model',
                       labels = c('MAT', 'RF', 'WA', 'WAPLS')) +
  theme(legend.position = 'bottom') +
  theme_paleo() +
  rotated_axis_labels(45) +
  scale_y_depth_age(adm, age_name = 'cal BP', age_breaks = age_breaks)
vert_plot

if (param_type2 == 'climate'){
  
  vert_plot2 = vert_plot +
    geom_vline(aes(xintercept = xintercept),
               data = tibble(type1 = 'P_ann', xintercept = 415),
               linetype = 6, size = 0.4, color = 'red') +
    geom_vline(aes(xintercept = xintercept),
               data = tibble(type1 = 'T_ann', xintercept = -6),
               linetype = 6, size = 0.4, color = 'red') +
    geom_vline(aes(xintercept = xintercept),
               data = tibble(type1 = 'T_jul', xintercept = 17.7),
               linetype = 6, size = 0.4, color = 'red') +
    geom_vline(aes(xintercept = xintercept),
               data = tibble(type1 = 'T_jan', xintercept = -30.8),
                linetype = 6, size = 0.4, color = 'red')
  
  vert_plot2
  
} else if (param_type2 == 'woody'){
  
  vert_plot2 = vert_plot +
  geom_vline(aes(xintercept = xintercept),
             data = tibble(type1 = 'km50', xintercept = 0.95),
             linetype = 6, size = 0.4, color = 'red') +
  geom_vline(aes(xintercept = xintercept),
             data = tibble(type1 = 'km20', xintercept = 0.95),
             linetype = 6, size = 0.4, color = 'red') +
  geom_vline(aes(xintercept = xintercept),
             data = tibble(type1 = 'km10', xintercept = 0.92),
             linetype = 6, size = 0.4, color = 'red') +
  geom_vline(aes(xintercept = xintercept),
             data = tibble(type1 = 'km5', xintercept = 0.9),
             linetype = 6, size = 0.4, color = 'red')
  
  vert_plot2
  
} else {
  cat('Didnt find parameter type')
}

ggsave(paste0('plots/reconstructions/', paste0(name, '_',
              'reconstructions_',
              param_type2,
              '_depth.pdf')),
       plot = vert_plot2, device = 'pdf', width = 2700, height = 1800,
       units = 'px')


# 4. Correlation analysis between reconstructed values ----
# recons_corr = recons |> 
#   select(-depth, -ages)
# 
# i = 0
# method = 'spearman'
# for (i in 1:length(parameters_types)){
#   param_corr(recons_corr, parameters_types[i], method)
# }

# 5. Summary table formatting and saving ---- 
summary_mat2 = summary_mat |> 
  summary_to_table()

summary_wa2 = summary_wa |> 
  summary_to_table()

summary_wapls2 = summary_wapls |> 
  summary_to_table()

summary_rf2 = summary_rf |> 
  summary_to_table()

summary = bind_rows(summary_mat2, summary_wa2, summary_wapls2, summary_rf2)
models = c('MAT', 'MAT', 'MAT', 'MAT', 'WA', 'WA', 'WA', 'WA',
           'WAPLS', 'WAPLS', 'WAPLS', 'WAPLS', 'RF', 'RF', 
           'RF', 'RF')
summary = cbind(summary, models) |> 
  relocate(models)
rownames(summary) = 1:nrow(summary)

summary = summary |> 
  group_by(models, variable)

table = summary

# Table visualization. Not completed
# flex_table = as_flextable(summary)
# flex_table = bold(flex_table, ~ p.value <= 0.1, ~ p.value, bold = TRUE)
# flex_table

writexl::write_xlsx(summary, paste0('output/summary/', paste0(name, '_'),
                                    'summary_',
                                    param_type2,
                                    '.xlsx'))
