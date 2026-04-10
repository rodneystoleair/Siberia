## This script does reconstruction using 'rioja' package. The method is
## weighting averaging - partial least squares (WAPLS). 
## The training set is in variable "modern", which has pollen spectra,
## prediction set is in the variable "fossil", which contains the fossil spectra
## Model and CV parameters are in transfer function body.

# 1. Dependencies ----
# Packages loading
library('palaeoSig')
library('rioja')

# Custom functions
source('R/writing_functions.R')
source('R/plotting_functions.R')

# 2. Data ----
# Removing zero abundance taxa
condition = colSums(modern) != 0
modern_wapls = modern[, condition]

# 3. WAPLS transfer function ----
# Transfer function & cross-validation. Cycle repeats for every reconstruction
# parameter. Also performance results are being written into text file
i = 0 # increment
wapls = data.frame(ages)
summary_wapls = performance_summary(parameters_types) # for summary

for (i in 1:length(parameters_types)){
  
  # initial parameters
  method = 'WAPLS'
  parameter = pull(parameters_modern, var = parameters_types[i])
  
  # basic transfer function
  func_wapls = WAPLS(
    modern_wapls,
    parameter,
    npls = 3,
    iswapls = T,
    standx = F,
    lean = F,
    check.data = T
  )
  
  # bootstrap cross-validation
  cv_wapls = rioja::crossval(
    func_wapls,
    cv.method = 'bootstrap',
    verbose = T,
    ngroups = 10,
    nboot = 100
  )
  
  # prediction with newdata
  recon_wapls = predict(
    func_wapls,
    newdata = fossil,
    sse = T,
    nboot = 100,
    match.data = T,
    verbose = T
  )
  
  # performance & summary writing 
  parameter_name = define_name(parameters_types, i) # parameter type in text
  performance_cv = rioja::performance(cv_wapls) # write performance 
  
  model_settings = names(
    performance_cv$crossval[,1][which.min(
      performance_cv$crossval[,1])]) # pull model parameters
  
  # pull fitted values: best parameters
  parameter1 = recon_wapls$fit[,model_settings]
  # sse = recon_wapls$SEP.boot[,model_settings]
  
  # bind fitted values for one parameter with others
  wapls = cbind(wapls, parameter1)
  # wapls = cbind(wapls, sse)
  colnames(wapls)[length(wapls)] = paste0(parameters_types[i], '.wapls')
  # colnames(wapls)[length(wapls)] = paste0(parameters_types[i], '.wapls.sse')
  
  # here goes statistical significance assessment
  sig_wapls = randomTF(
    spp = sqrt(modern_wapls),
    env = parameter,
    fos = sqrt(fossil),
    fun = WAPLS,
    n = 999,
    col = model_settings,
    npls = 3,
    iswapls = T,
    standx = F,
    lean = F,
    check.data = T
  )
  
  results = write_results_rioja(cv_wapls) # write results
  summary_wapls[[i]] = write_summary_rioja(performance_cv,
                                           sig_wapls$sig,
                                           model_settings) # summary
  plots(results, method, parameter_name, model_settings) # plot
  
  # print summary
  paste0(parameters_types[i], '--------------------------------------------') |> 
    print()
  print(cv_wapls)
  print(sig_wapls$sig)
}

# append the final result with others
wapls = wapls |> 
  select(-ages)
recons = cbind(recons, wapls)
