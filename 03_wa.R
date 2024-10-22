## This script does reconstruction using 'rioja' package. The method is
## weighting averaging (WA). 
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
modern_wa = modern[, condition]

# 3. WA transfer function ----
# Transfer function & cross-validation. Cycle repeats for every reconstruction
# parameter. Also performance results are being written into text file
i = 0 # increment
wa = data.frame(ages)
summary_wa = performance_summary(parameters_types) # for summary

for (i in 1:length(parameters_types)){
  
  # initial parameters
  method = 'WA'
  parameter = pull(parameters_modern, var = parameters_types[i])
  
  # start recording performance
  sink(paste0('output/performance/wa/', parameters_types[i], '_wa.txt'))
  
  # basic transfer function
  func_wa = WA(
    modern_wa,
    parameter,
    mono = F,
    tolDW = F,
    use.N2 = F,
    tol.cut = .01,
    check.data = T,
    lean = F
  )
  
  # bootstrap cross-validation
  cv_wa = crossval(
    func_wa,
    cv.method = 'bootstrap',
    verbose = T,
    ngroups = 10,
    nboot = 100,
    h.cutoff = 0,
    h.dist = NULL
  )
  
  # prediction with newdata
  recon_wa = predict(
    func_wa,
    newdata = fossil,
    sse = F,
    nboot = 100,
    match.data = T,
    verbose = T
  )
  
  # print model performance 
  print(cv_wa)
  print(sig_wa)
  sink()
  
  # performance & summary writing 
  parameter_name = define_name(parameters_types, i) # parameter type in text
  performance_cv = performance(cv_wa) # write performance using rioja function
  
  model_settings = names(
    performance_cv$crossval[,1][which.min(
      performance_cv$crossval[,1])]) # pull model parameters
  
  # pull fitted values: best model parameters
  parameter1 = recon_wa$fit[,model_settings]
  
  # bind fitted values for one parameter with others
  wa = cbind(wa, parameter1)
  colnames(wa)[length(wa)] = paste0(parameters_types[i], '.wa')
  
  # here goes statistical significance assessment
  sig_wa = randomTF(
    spp = sqrt(modern_wa),
    env = parameter,
    fos = sqrt(fossil),
    fun = WA,
    n = 999,
    col = model_settings,
    mono = F,
    tolDW = F,
    use.N2 = F,
    tol.cut = .01,
    check.data = T,
    lean = F
  )
  
  results = write_results_rioja(cv_wa) # write results
  summary_wa[[i]] = write_summary_rioja(performance_cv,
                                        sig_wa$sig,
                                        model_settings) # summary
  plots(results, method, parameter_name, model_settings) # plot
  }

# append the final result with others
wa = wa |> 
  select(-ages)
recons = cbind(recons, wa)
