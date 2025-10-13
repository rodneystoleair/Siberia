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
library('tidyverse')
library('tidypaleo')
library('ggcorrplot')
source('R/plotting_functions.R')
source('R/summary_functions.R')


# 1. Data handling ----
# write reconstructions into sheet
#writexl::write_xlsx(recons, paste0('output/reconstructed/', 'Igarka',
#                                    '_woody.xlsx'))
# Converting to longer format 
recon_plot = recons |> 
  pivot_longer(contains(c('mat', 'wa', 'wapls', 'rf')), names_to = 'type', 
               values_to = 'fitted') |>
  separate(type, into = c('type1', 'type2'), sep = '\\.') # separate types

ordinary = c(
  'T_jan' = 'Tjan, °C',
  'T_jul' = 'Tjuly, °C',
  'T_ann' = 'Tann, °C',
  'P_ann' = 'Pann, mm',
  'km20' = 'Woody cover, 20 km')

age_breaks = c(seq(4800, 9800, 200))
depth_breaks = c(seq(0, 170, 10))

adm_data = tibble(depth = as.numeric(ages$depth), age = ages$age)
adm = age_depth_model(adm_data,
                      depth = depth,
                      age = age)

# 3. Plotting ----
## 3.1. Stratigraphical vertical plot ----
vert_plot = ggplot(recon_plot, aes(x = fitted,
                       y = depth,
                       color = type2,
                       group = type2)) +
  geom_lineh(size = 0.3) +
  scale_y_reverse()  +
  facet_geochem_gridh(vars(type1),
                      labeller = labeller(type1 = ordinary)) +
  labs(x = 'Reconstructed values',
       y = 'Depth') +
  scale_color_discrete(name = 'Model',
                       labels = c('MAT', 'RF', 'WA', 'WAPLS')) +
  theme(legend.position = 'bottom') +
  theme_paleo() +
  rotated_axis_labels(45) +
  scale_y_depth_age(adm, age_name = 'Age (cal yr BP)', age_breaks = age_breaks) +
  geom_vline(aes(xintercept = xintercept),
             data = tibble(type1 = 'P_ann', xintercept = 516),
             linetype = 6, size = 0.4, color = 'red') +
  geom_vline(aes(xintercept = xintercept),
             data = tibble(type1 = 'T_ann', xintercept = -8.1),
             linetype = 6, size = 0.4, color = 'red') +
  geom_vline(aes(xintercept = xintercept),
             data = tibble(type1 = 'T_jul', xintercept = 15.3),
             linetype = 6, size = 0.4, color = 'red') +
  geom_vline(aes(xintercept = xintercept),
             data = tibble(type1 = 'T_jan', xintercept = -27.7),
              linetype = 6, size = 0.4, color = 'red')
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

# 4. Summary table formatting and saving ---- 
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
