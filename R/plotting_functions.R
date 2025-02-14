# Plotting functions ----

# plots ----
# written by : Rodion Andreev
# purpose    : to plot a nice scatter plot with ggplot2 interface.
# All results are cross-validated. Function also saves plots in .png pictures.
# params     : dataset: a data frame, containing fitted, residuals and
# observed values, method_type: one string, just for the plot title,
# param_name: one string with fitted parameter name, for plot subtitle,
# settings: one string with necessary model features, for subtitle too.
# desc       : function uses the tibble after given writing functions

plots = function(dataset, method_type, param_name, settings, is.suppl = F){
  regr_line1 = tibble(
    fitted = fitted(lm(fitted_recon ~ observed_orig,
                       data = dataset)),
    observed = dataset$observed_orig
  )
  
  obs_v_fit = ggplot(data = dataset) +
    geom_line(
      data = regr_line1,
      aes(x = observed,
          y = fitted),
      color = 'black',
      linewidth = 0.2
    ) +
    geom_point(
      mapping = aes(x = observed_orig,
                    y = fitted_recon),
      color = 'salmon',
      alpha = 0.5,
      size = 3,
      shape = 1
    ) +
    ggtitle(method_type,
            subtitle = paste0(param_name, ', ', settings)) +
    xlab('Obsevrved') +
    ylab('Fitted') +
    theme_classic()
  
  if (is.suppl == F) {
    ggsave(paste0(parameters_types[i], '_', method, '_cv1.png'),
           plot = obs_v_fit,
           path = paste0('plots/', method, '/'),
           width = 1600,
           height = 1200,
           units = 'px')
  } else if (is.suppl == T) {
    return(obs_v_fit)
  }
  
  regr_line2 = tibble(
    fitted = fitted(lm(residuals_recon ~ observed_orig,
                       data = dataset)),
    observed = dataset$observed_orig
  )
  
  obs_v_resid = ggplot(data = dataset) +
    geom_line(
      data = regr_line2,
      aes(x = observed,
          y = fitted),
      color = 'black',
      linewidth = 0.2
    ) +
    geom_point(
      mapping = aes(x = observed_orig,
                    y = residuals_recon),
      color = 'salmon',
      alpha = 0.5,
      size = 2,
      shape = 1
    ) +
    ggtitle(method,
            subtitle = paste0(param_name, ', ', settings, ' - residuals')) +
    xlab('Observed') +
    ylab('Residuals') +
    theme_classic()
  
  if (is.suppl == F) {
  ggsave(paste0(parameters_types[i], '_', method,
                '_cv2.png'),
         plot = obs_v_resid,
         path = paste0('plots/', method, '/'),
         width = 1600,
         height = 1200,
         units = 'px')
  }
}

# define_name ----
# written by : Rodion Andreev
# purpose    : defining fitted parameter name for plot subtitles
# params     : x: vector of strings with parameter names, i: cycle increment 
# desc       : -

# In English
define_name = function(x, i){
  if (x[i] == 'T_jan'){
    param_name = 'Mean January temperature, °C'
  } else if (x[i] == 'T_jul'){
    param_name = 'Mean July temperature, °C'
  } else if (x[i] == 'T_ann'){
    param_name = 'Mean annual temperature, °C'
  } else if (x[i] == 'P_ann'){
    param_name = 'Mean precipitation, mm/yr'
  } else if (x[i] == 'km50'){
    param_name = 'Woody cover, 50 km radius'
  } else if (x[i] == 'km20'){
    param_name = 'Woody cover, 20 km radius'
  } else if (x[i] == 'km10'){
    param_name = 'Woody cover, 10 km radius'
  } else if (x[i] == 'km5'){
    param_name = 'Woody cover, 5 km radius'
  }
  return(param_name)
}

# In Russian
# define_name = function(x, i){
#   if (x[i] == 'T_jan'){
#     param_name = 'Средняя температура января, С°'
#   } else if (x[i] == 'T_jul'){
#     param_name = 'Средняя температура июля, С°'
#   } else if (x[i] == 'T_ann'){
#     param_name = 'Среднегодовая температура, С°'
#   } else if (x[i] == 'P_ann'){
#     param_name = 'Осадков в год, мм'
#   } else if (x[i] == 'km50'){
#     param_name = 'Лесистость в радиусе 50 км, доля'
#   } else if (x[i] == 'km20'){
#     param_name = 'Лесистость в радиусе 20 км, доля'
#   } else if (x[i] == 'km10'){
#     param_name = 'Лесистость в радиусе 10 км, доля'
#   } else if (x[i] == 'km5'){
#     param_name = 'Лесистость в радиусе 5 км, доля'
#   }
#   return(param_name)
# }

# write_screeplot ----
# written by : Rodion Andreev
# purpose    : Data frame preparation for screeplotting
# params     : t_function: a mat.cv-type object
# desc       : works with 'analogue' package

write_screeplot = function(t_function){
  results2 = tibble(
    no = c(1:20),
    rmsep_loo = t_function[["model"]][["rmsep"]][1:20],
    rmsep_boot = t_function[["bootstrap"]][["rmsep"]][1:20],
    max.bias_loo = t_function[["model"]][["max.bias"]][1:20],
    max.bias_boot = t_function[["bootstrap"]][["max.bias"]][1:20]
  )
}

# screeplots ----
# written by : Rodion Andreev
# purpose    : Screeplot function for MAT: returns scree plots
# params     : dataset: a data frame with RMSEP and R2, method_name:
# one string, just for the plot title, param_name: one string with fitted
# parameter name, for plot subtitle
# desc       : no. of analogs are plotted against bootstrapped cross-validated
# RMSEP and Rsquared. For 20 nearest analogs.

screeplots = function(dataset, method_name, param_name){
  ggplot(data = dataset, aes(x = no)) +
    geom_line(aes(y = rmsep_loo, color = 'Проверка
с исключением'),
              linewidth = 0.2) +
    geom_line(aes(y = rmsep_boot, color = 'Бутстрэп'),
              linewidth = 0.2) +
    geom_label(aes(y = rmsep_loo,
                   label = no),
               label.padding = unit(0.01, 'lines'),
               label.size = 0,
               size = 2) +
    geom_label(aes(y = rmsep_boot,
                   label = no),
               label.padding = unit(0.01, 'lines'),
               label.size = 0, 
               size = 2) +
    ggtitle(paste0(param_name, ' - ' ,method_name),
            subtitle = 'График собственных значений') +
    xlab('Число аналогов') +
    ylab('RMSEP') +
    labs(colour = '') +
    theme(legend.position = 'top') +
    theme_classic()
  
  ggsave(paste0(parameters_types[i], '_', method,
                '_screeplot1.png'),
         path = paste0('plots/', method, '/'),
         width = 1600,
         height = 1200,
         units = 'px')
  
  ggplot(data = dataset, aes(x = no)) +
    geom_line(aes(y = max.bias_loo, color = 'Проверка
с исключением'),
              linewidth = 0.2) +
    geom_line(aes(y = max.bias_boot, color = 'Бутстрэп'),
              linewidth = 0.2) +
    geom_label(aes(y = max.bias_loo,
                   label = no),
               label.padding = unit(0.01, 'lines'),
               label.size = 0,
               size = 2) +
    geom_label(aes(y = max.bias_boot,
                   label = no),
               label.padding = unit(0.01, 'lines'),
               label.size = 0,
               size = 2) +
    ggtitle(paste0(param_name, ' - ' ,method_name),
            subtitle = 'График собственных значений') +
    xlab('Число аналогов') +
    ylab('Максимальное смещение оценки') +
    labs(colour = '') +
    theme(legend.position = 'top') +
    theme_classic()
  
  ggsave(paste0(parameters_types[i], '_', method,
                '_screeplot2.png'),
         path = paste0('plots/', method, '/'),
         width = 1600,
         height = 1200,
         units = 'px')
}

# params_corr ----
# written by : Rodion Andreev
# purpose    : generate correlation matricies and save them as .svg
# params     : recons_data -- a data frame with reconstructed values
# parameter -- reconstructed parameter name to correlate
# method -- model type (MAT, WA etc)

param_corr = function(recons_data, parameter, method) {
  if (parameter != 'km5') {
    corr_data = recons_data |> 
      select(contains(parameter))
  } else if (parameter == 'km5') {
    corr_data = recons_data |> 
      select(contains('km5')) |> 
      select(-contains('km50'))
  }
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
