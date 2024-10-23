# Writing functions ---- 

# write_results ----
# written by : Rodion Andreev
# purpose    : to write a MAT transfer function result in a tibble
# desc       : for package analogue data type 'mat'

write_results = function(t_function) {
  results = tibble(
    fitted_recon = fitted(t_function)$estimated,
    residuals_recon =
      resid(t_function, k = getK(t_function))$bootstrap[, getK(t_function)],
    observed_orig = t_function$observed
  )
  return(results)
}

# write_summary ----
# written by : Rodion Andreev
# purpose    : to write a MAT transfer function performance in a list
# params     : t_function: a bootstrap.mat object
#              prediction: a predict.mat object

write_summary = function(cv, prediction, p) {
  performance = list(
    parameter = paste0('k = ', getK(cv)),
    r2 = round(cv$bootstrap$r.squared[getK(cv)], 2),
    rmsep = round(RMSEP(cv), 2),
    max.bias = round(cv$bootstrap$max.bias[getK(cv)], 2),
    p.value = p
  )
  return(performance)
}

# write_results_rioja ----
# written by : Rodion Andreev
# purpose    : to write WA&WAPLS transfer function result in a tibble
# desc       : for package rioja data type 

write_results_rioja = function(cv){
  performance = performance(cv)
  type =
    names(
      performance[["crossval"]][,1][which.min(performance[["crossval"]][,1])]
      )
  fitted_all = as.data.frame(fitted(cv))
  residuals_all = as.data.frame(residuals(cv, cv = T))
  results = tibble(
    fitted_recon = fitted_all[, type],
    residuals_recon = residuals_all[, type],
    observed_orig = cv[['x']]
  )
  return(results)
}

# write_summary_rioja ----
# written by : Rodion Andreev
# purpose    : to write a MAT transfer function performance in a list
# params     : cv: a list returned by rioja performance() function
#              p: p-value from palaeoSig object
#              param: model parameters (WA.cla, WA.inv, Comp01-Comp03)

write_summary_rioja = function(cv, p, param){
  performance = list(
    parameter = param,
    r2 = round(cv$crossval[param,][2], 2),
    rmsep = round(cv$crossval[param,][1], 2),
    max.bias = round(cv$crossval[param,][4], 2),
    p.value = p
  )
  return(performance)
}

# write_results_rf ----
# written by : Rodion Andreev
# purpose    : to write RF transfer function result in a tibble
# desc       : works with caret package

write_results_rf = function(t_function){
  results = tibble(
    fitted_recon = t_function$finalModel$predicted,
    residuals_recon = t_function$finalModel$y - t_function$finalModel$predicted,
    observed_orig = t_function$finalModel$y
  )
  return(results)
}

# performance_summary ----
# written by : Rodion Andreev
# purpose    : to write performance summary in a list
# desc       : creates list with cells for each parameter

performance_summary = function(param_names){
  summary = 1:length(param_names) |> 
    as.list() |> 
    setNames(param_names)
  return(summary)
}
