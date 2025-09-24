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
recons = readxl::read_xlsx(paste0('output/reconstructed/', name,
                                  '_woody.xlsx')) |> 
  mutate(depth = as.character(depth))

## If you need to load new ages
median = read_delim('data/fossil/NizhnyayaTunguska_52_ages.txt') |> 
  select(depth, median) |> 
  mutate(depth = as.character(depth))
median

recons = recons |> 
  select(-ages) |>
  mutate(depth = as.character(depth)) |> 
  left_join(median, by = 'depth') |> 
  relocate(median, .after = depth) |> 
  rename(ages = median)

# 2. Data handling ----
# Converting to longer format 
recon_plot = recons |> 
  pivot_longer(contains(c('mat', 'wa', 'wapls', 'rf')), names_to = 'type', 
               values_to = 'fitted') |> 
  transform(depth = as.numeric(depth)) |> 
  transform(ages = abs(as.numeric(ages))) |> 
  separate(type, into = c('type1', 'type2'), sep = '\\.') |> # separate types
  mutate(type1 = fct_relevel(type1, c('km50', 'km20', 'km10', 'km5')))

ordinary = c(
  'T_jan' = 'Tjan, °C',
  'T_jul' = 'Tjuly, °C',
  'T_ann' = 'Tann, °C',
  'P_ann' = 'Pann, mm',
  'km20' = 'Woody cover, 20 km')

woody = c(
  'km50' = '50 km',
  'km20' = '20 km',
  'km10' = '10 km',
  'km5' = '5 km')

age_breaks = c(seq(0, 8000, 500))
depth_breaks = c(seq(0, 220, 20))

adm_data = tibble(depth = as.numeric(recons$depth), age = recons$ages)
adm = age_depth_model(adm_data,
                      depth = depth,
                      age = age)

# 3. Plotting ----
## 3.1. Stratigraphical horizontal plot ----
ggplot(data = recon_plot, aes(x = ages, y = fitted, color = type2,
                              group = type2)) +
  geom_line(size = 0.2) +
  geom_point(size = 0.2) +
  scale_x_reverse() +
  labs(x = 'Reconstructed values',
       y = 'Age (cal yr BP)') +
  scale_color_discrete(name = 'Model',
                       labels = c('MAT', 'RF', 'WA', 'WAPLS')) +
  theme(legend.position = 'bottom') +
  facet_wrap(~type1, scales = 'free', labeller = labeller(type1 = c(ordinary, woody))) +
  theme_classic()

ggsave(paste0('plots/reconstructions/', paste0(name, '_'),
              'reconstructions.png'),
       plot = last_plot(), device = 'png', width = 300, height = 100,
       units = 'mm')

## 3.2. Stratigraphical vertical plot ----
vert_plot = ggplot(recon_plot, aes(x = fitted,
                       y = depth,
                       color = type2,
                       group = type2)) +
  geom_lineh(size = 0.3) +
  geom_vline(aes(xintercept = xintercept),
             data = tibble(type1 = 'km50', xintercept = 0.96),
             linetype = 6, size = 0.4, color = 'red') +
  geom_vline(aes(xintercept = xintercept),
             data = tibble(type1 = 'km20', xintercept = 0.94),
             linetype = 6, size = 0.4, color = 'red') +
  geom_vline(aes(xintercept = xintercept),
             data = tibble(type1 = 'km10', xintercept = 0.95),
             linetype = 6, size = 0.4, color = 'red') +
  geom_vline(aes(xintercept = xintercept),
             data = tibble(type1 = 'km5', xintercept = 0.94),
             linetype = 6, size = 0.4, color = 'red') +
  # geom_vline(aes(xintercept = xintercept),
  #            data = tibble(type1 = 'P_ann', xintercept = 373),
  #            linetype = 6, size = 0.4, color = 'red') +
  # geom_vline(aes(xintercept = xintercept),
  #            data = tibble(type1 = 'T_ann', xintercept = -9.3),
  #            linetype = 6, size = 0.4, color = 'red') +
  # geom_vline(aes(xintercept = xintercept),
  #            data = tibble(type1 = 'T_jul', xintercept = 16.5),
  #            linetype = 6, size = 0.4, color = 'red') +
  # geom_vline(aes(xintercept = xintercept),
  #            data = tibble(type1 = 'T_jan', xintercept = -36.2),
  #             linetype = 6, size = 0.4, color = 'red') +
  scale_y_reverse()  +
  facet_geochem_gridh(vars(type1),
                      labeller = labeller(type1 = woody)) +
  labs(x = 'Reconstructed values',
       y = 'Depth') +
  scale_color_discrete(name = 'Model',
                       labels = c('MAT', 'RF', 'WA', 'WAPLS')) +
  theme(legend.position = 'bottom') +
  theme_paleo() +
  rotated_axis_labels(45) +
  scale_y_depth_age(adm, age_name = 'Age (cal yr BP)', age_breaks = age_breaks)
vert_plot

# age_model = age_depth_model(depth = recon_plot$depth,
#                             age = recon_plot$ages)
# 
# vert_plot +
#   scale_y_age_depth(age_model, depth_name = 'Depth, cm')

ggsave(paste0('plots/reconstructions/', paste0(name, '_'),
              'reconstructions_woody_depth.pdf'),
       plot = vert_plot, device = 'pdf', width = 2700, height = 1800,
       units = 'px')

ggsave(paste0('plots/reconstructions/', paste0(name, '_'),
              'reconstructions_woody_depth.png'),
       plot = vert_plot, device = 'png', width = 2700, height = 1800,
       units = 'px')

ggsave(paste0('plots/reconstructions/', paste0(name, '_'),
              'reconstructions_woody_depth.svg'),
       plot = vert_plot, device = 'svg', width = 2700, height = 1800,
       units = 'px')


# 4. Correlation analysis between reconstructed values ----
recons_corr = recons |> 
  select(-depth, -ages)

i = 0
method = 'spearman'
for (i in 1:length(parameters_types)){
  param_corr(recons_corr, parameters_types[i], method)
}

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
                                    'summary_climate.xlsx'))
