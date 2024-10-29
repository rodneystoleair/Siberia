# This script provide postprocessing of reconstruction results. They proceed
# several procedures:
#   1. Data handling for plotting
#   2. Stratigraphical plotting
#   3. Correlation analysis between fitted values
#   4. Nice summary table formatting
# Visualisation relies on pre-recorded parameter names, divided into two groups:
# ordinary and woody. If you want to add some more, you need to write their
# full names for plots :)

# 1. Dependencies ----
# Packages loading
library('ggcorrplot')
library('flextable')
library('tidyverse')
library('tidypaleo')

# 2. Data handling ----
# Last 3 samples removal. After reconstruction assessment, we decided that
# reconstruction for them are not reliable due to unrealistic values.
# This topic requires additional research.
recons = recons[-1:-3,] 

# Converting to longer format 
recon_plot = recons |> 
  pivot_longer(contains(c('mat', 'wa', 'wapls', 'rf')), names_to = 'type', 
               values_to = 'fitted') |> 
  transform(depth = as.numeric(depth)) |> 
  transform(ages = as.numeric(ages)) |> 
  separate(type, into = c('type1', 'type2'), sep = '\\.') # separate types
  # mutate(type1 = fct_relevel(type1, c('km50', 'km20', 'km10', 'km5')))

ordinary = c(
  'T_jan' = 'January mean T, С°',
  'T_jul' = 'July mean T, С°',
  'T_ann' = 'Year mean T, С°',
  'P_ann' = 'Year precipitation, mm',
  'km20' = 'Woody cover, 20 km')

woody = c(
  'km50' = '50 km',
  'km20' = '20 km',
  'km10' = '10 km',
  'km5' = '5 km')

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
  facet_wrap(~type1, scales = 'free', labeller = labeller(type1 = ordinary)) +
  theme_classic()

ggsave(paste0('plots/reconstructions/', paste0(name, '_'),
              'reconstructions.png'),
       plot = last_plot(), device = 'png', width = 300, height = 100,
       units = 'mm')

## 3.2. Stratigraphical vertical plot ----
vert_plot = ggplot(recon_plot, aes(x = fitted,
                       y = ages,
                       color = type2,
                       group = type2)) +
  geom_lineh(size = 0.3) +
  # geom_vline(aes(xintercept = xintercept),
  #            data = tibble(type1 = 'km50', xintercept = 0.96),
  #            linetype = 6, size = 0.4, color = 'red') +
  # geom_vline(aes(xintercept = xintercept),
  #            data = tibble(type1 = 'km20', xintercept = 0.94),
  #            linetype = 6, size = 0.4, color = 'red') +
  # geom_vline(aes(xintercept = xintercept),
  #            data = tibble(type1 = 'km10', xintercept = 0.95),
  #            linetype = 6, size = 0.4, color = 'red') +
  # geom_vline(aes(xintercept = xintercept),
  #            data = tibble(type1 = 'km5', xintercept = 0.94),
  #            linetype = 6, size = 0.4, color = 'red') +
  geom_vline(aes(xintercept = xintercept),
             data = tibble(type1 = 'P_ann', xintercept = 373),
             linetype = 6, size = 0.4, color = 'red') +
  geom_vline(aes(xintercept = xintercept),
             data = tibble(type1 = 'T_ann', xintercept = -9.3),
             linetype = 6, size = 0.4, color = 'red') +
  geom_vline(aes(xintercept = xintercept),
             data = tibble(type1 = 'T_jul', xintercept = 16.5),
             linetype = 6, size = 0.4, color = 'red') +
  geom_vline(aes(xintercept = xintercept),
             data = tibble(type1 = 'T_jan', xintercept = -36.2),
             linetype = 6, size = 0.4, color = 'red') +
  scale_y_reverse()  +
  facet_geochem_gridh(vars(type1),
                      labeller = labeller(type1 = ordinary)) +
  labs(x = 'Reconstructed values',
       y = 'Age (cal yr BP)') +
  scale_color_discrete(name = 'Model',
                       labels = c('MAT', 'RF', 'WA', 'WAPLS')) +
  theme(legend.position = 'bottom') +
  theme_paleo() +
  rotated_axis_labels(45)
vert_plot

age_model = age_depth_model(depth = recon_plot$depth,
                            age = recon_plot$ages)

vert_plot +
  scale_y_age_depth(age_model, depth_name = 'Depth, cm')

ggsave(paste0('plots/reconstructions/', paste0(name, '_'),
              'reconstructions_woody.pdf'),
       plot = last_plot(), device = 'pdf', width = 2700, height = 1800,
       units = 'px')

# 4. Correlation analysis between reconstructed values ----
recons_corr = recons |> 
  select(-depth, -ages)

param_corr = function(recons_data, parameter, method) {
  corr_data = recons_data |> 
    select(contains(parameter))
  corr = round(cor(corr_data, method = method), 2)
  pvalue = cor_pmat(corr)
  plot = ggcorrplot(corr, hc.order = T, type = 'lower', p.mat = pvalue,
                    legend.title = 'Spearman rho', insig = 'pch')
  ggsave(paste0(parameter, '_', method,
                '_corr.svg'),
         path = 'plots/corr/',
         width = 1600,
         height = 1200,
         units = 'px')
  return(plot)
}

i = 0
method = 'spearman'
for (i in 1:length(parameters_types)){
  param_corr(recons_corr, parameters_types[i], method)
}

# Summary table formatting
summary_to_table = function(summary){
  table = summary |> 
    lapply(as.data.frame) |> 
    bind_rows() |> 
    mutate(variable = names(summary_rf)) |> 
    relocate(variable)
  return(table)
}

summary_mat2 = summary_mat |> 
  summary_to_table()

summary_wa2 = summary_wa |> 
  summary_to_table()

summary_wapls2 = summary_wapls |> 
  summary_to_table()

summary_rf2 = summary_rf |> 
  summary_to_table()

summary = bind_rows(summary_mat2, summary_wa2, summary_wapls2, summary_rf2)
models = c('MAT', 'MAT', 'MAT', 'MAT', 'MAT', 'WA', 'WA', 'WA', 'WA', 'WA',
           'WAPLS', 'WAPLS', 'WAPLS', 'WAPLS', 'WAPLS', 'RF', 'RF', 'RF', 'RF',
           'RF')
summary = cbind(summary, models) |> 
  relocate(models)
rownames(summary) = 1:nrow(summary)

summary = summary |> 
  group_by(models, variable)

table = summary

flex_table = as_flextable(summary)
flex_table = bold(flex_table, ~ p.value <= 0.1, ~ p.value, bold = TRUE)
flex_table
