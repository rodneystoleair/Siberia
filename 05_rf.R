## This script does reconstruction using Random Forest method.
## The training set is in variable "modern", which has pollen spectra,
## prediction set is in the variable "fossil", which contains the fossil spectra

# 1. Dependencies ----
# Packages loading
library('randomForest')
library('e1071')
library('caret')

# Custom functions
source('R/writing_functions.R')
source('R/plotting_functions.R')
source('R/rfWrapper_functions.R')

# 2. Data ----
# Removing zero abundance taxa
condition = colSums(modern) != 0
modern_rf = modern[, condition]

# Column (taxa) names renaming for randomForest()
names = c(1:length(modern))
i = 0
for (i in 1:length(names)) {
  names[i] = paste0('T', names[i])
}
modern_rf = modern
colnames(modern_rf) = names
fossil_rf = fossil
colnames(fossil_rf) = names

# Variables meaning
meaning = data.frame(names, colnames(modern))

# 3. Transfer function ----
# Transfer function evaluation using k-fold cross-validation. caret train()
# method is being used. RF iterates three times in order to evaluate maxnodes,
# ntree, and mtry RF parameters. Also performance results are being written
# into text file
i = 0
rf = data.frame(ages)
method = 'RF'
summary_rf = performance_summary(parameters_types) # for summary

# CV parameters
params_cv = trainControl(method = 'cv', number = 5, search = 'grid')

# Cycle repeats for each reconstruction parameter
for (i in 1:length(parameters_types)){
  
  # Data preparation
  parameter = pull(parameters_modern, var = parameters_types[i])
  dataset = cbind(parameter, modern_rf)
  
  # Grid for the first CV iteration
  grid = expand.grid(.mtry = c(1:20))
  
  # start recording performance
  sink(paste0('output/performance/rf/', parameters_types[i], '_rf.txt'))
  
  # First RF iterarion: the best mtry
  cv_rf_mtry = train(
    parameter ~ .,
    data = dataset,
    method = 'rf',
    metric = 'Rsquared',
    tuneGrid = grid,
    trControl = params_cv,
    importance = T,
    nodesize = 14,
    ntree = 300
  )
  best_mtry = as.numeric(cv_rf_mtry$bestTune$mtry)
  print(paste0('Best mtry = ', best_mtry))
  
  # Second iteration: best maxnodes
  # New search grid using new mtry value
  grid = expand.grid(.mtry = best_mtry)
  maxnodes_storage = list()
  for (maxnodes in c(10:30)) {
    set.seed(1234)
    cv_rf_maxnode = train(
      parameter ~ .,
      data = dataset,
      method = 'rf',
      metric = 'Rsquared',
      tuneGrid = grid,
      trControl = params_cv,
      importance = T,
      nodesize = 14,
      maxnodes = maxnodes,
      ntree = 300
    )
    current_iteration = toString(maxnodes)
    maxnodes_storage[[current_iteration]] = cv_rf_maxnode
  }
  
  # Writing the results 
  results_maxnodes = resamples(maxnodes_storage)
  print(summary(results_maxnodes))
  maxnodes_colname = colnames(
    results_maxnodes$values)[apply(
      results_maxnodes$values, 1, which.max)][1]
  best_maxnodes = as.numeric(substr(maxnodes_colname, 1, 2))
  print(paste0('Best maxnodes = ', best_maxnodes))
  
  # Third iterarion: the best ntree
  ntree_storage = list()
  for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)){
    set.seed(1234)
    cv_rf_ntree = train(
      parameter ~ .,
      data = dataset,
      method = 'rf',
      metric = 'Rsquared',
      tuneGrid = grid,
      trControl = params_cv,
      importance = T,
      nodesize = 14,
      maxnodes = best_maxnodes,
      ntree = ntree
    )
    ntree_value = toString(ntree)
    ntree_storage[[ntree_value]] = cv_rf_ntree
  }
  
  # Writing the results 
  results_ntree = resamples(ntree_storage)
  summary(results_ntree)
  values = results_ntree$values
  ntree_colname = colnames(values)[apply(values, 1, which.max)][1]
  best_ntree = as.numeric(substr(ntree_colname, 1, 3))
  print(paste0('Best ntree = ', best_ntree))
  print(summary(results_ntree))
  
  # Returning R2 values
  r2_rf = apply(values, 2, max)[colnames(values)[apply(
    values, 1, which.max)][1]] |> 
    as.numeric() |> 
    round(2)
  print(paste0('R2 = ', r2_rf))
  
  # Returning RMSEP values
  rmsep_flt = str_detect(colnames(values), 'RMSE')
  rmsep_rf = round(apply(values[rmsep_flt], 1, min)[1], 2)
  print(paste0('RMSEP = ', r2_rf))
  
  # Final reconstruction
  func_rf = train(
    parameter ~ .,
    data = dataset,
    method = "rf",
    metric = "Rsquared",
    tuneGrid = grid,
    trControl = params_cv,
    importance = T,
    nodesize = 14,
    ntree = best_ntree,
    maxnodes = best_maxnodes
  )
  # Importance of different variables
  varImp(func_rf)
  sink()
  
  # Reconstruction results 
  recon_rf = predict(func_rf, fossil_rf)
  rf = cbind(rf, recon_rf)
  colnames(rf)[length(rf)] = paste0(parameters_types[i], '.rf')
  parameter_name = define_name(parameters_types, i)
  
  # here goes statistical significance assessment
  sig_rf = randomTF(
    spp = sqrt(modern_rf),
    env = parameter,
    fos = sqrt(fossil_rf),
    fun = rfWrap,
    col = 1,
    n = 99,
    ntree = best_ntree,
    mtry = best_mtry,
    maxnodes = best_maxnodes
  )
  
  # write summary
  summary_rf[i] = list(
    parameter = paste0(
      'ntree = ',
      best_ntree,
      ', mtry = ',
      best_mtry,
      ', maxnodes = ',
      best_maxnodes
    ),
    rmsep = round(rmsep_rf, 2),
    r2 = round(r2_rf, 2),
    max.bias = NA,
    p.value = sig_rf$sig
  )
  
  # Scatter plots
  results = write_results_rf(func_rf)
  plots(
    results,
    method,
    parameter_name,
    paste0(
      'ntree = ',
      best_ntree,
      ', mtry = ',
      best_mtry,
      ', maxnodes = ',
      best_maxnodes
    )
  )
}

# append the final result with others
rf = rf[,-1]
recons = cbind(recons, rf)
