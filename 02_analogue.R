## This script does reconstruction using 'analogue' package. The method is
## Modern Analogue Technique (MAT).
## The training set is in variable "modern", which has pollen spectra,
## prediction set is in the variable "fossil", which contains the fossil spectra
## Model and CV parameters are in transfer function body.

# 1. Dependencies ----
# Packages loading
library('analogue')
library('rioja')
library('palaeoSig')

# Custom functions
source('R/writing_functions.R')
source('R/plotting_functions.R')

# 2. Data ----
# Data frames join
dat = join(modern, fossil)

# For MAT data needs to be proportional
modern_mat = dat$modern / 100 
fossil_mat = dat$fossil / 100

# 3. MAT transfer function ----
# Transfer function & cross-validation. Cycle repeats for every reconstruction
# parameter. Also performance results are being written into text file
i = 0 # Increment
mat = data.frame(ages)
summary_mat = performance_summary(parameters_types) # for summary

for (i in 1:length(parameters_types)){
  # initial parameters
  method = 'MAT'
  parameter = pull(parameters_modern, var = parameters_types[i])
  
  # initial transfer function
  func_mat = mat(modern_mat, parameter,
                 method = "SQchord") 
  
  # bootstrap cross-validation
  cv_mat = bootstrap(func_mat, n.boot = 100)
  
  # prediction on newdata
  recon_mat = predict(func_mat, fossil_mat, k = getK(cv_mat))
  
  # here goes statistical significance assessment
  sig_mat = randomTF(
    spp = modern_mat,
    env = parameter,
    fos = fossil_mat,
    fun = MAT,
    n = 999,
    col = 'MAT',
    k = getK(cv_mat),
    lean = T,
    dist.method = 'sq.chord'
  )
  
  # pull the prediction 
  parameter1 =
    recon_mat$predictions$model$predicted[getK(cv_mat),]
  
  # bind fitted values for one parameter with others
  mat = cbind(mat, parameter1)
  colnames(mat)[length(mat)] = paste0(parameters_types[i], '.mat')
  
  # results and performance writing
  results = write_results(cv_mat) # results: residuals, predictions for plots
  parameter_name = define_name(parameters_types, i) # text name for plots
  results2 = write_screeplot(cv_mat) # pull the results for scree plots
  summary_mat[[i]] = write_summary(cv_mat, recon_mat, sig_mat$sig)
  # performance
  model_settings = paste0('n of analogues: ', getK(cv_mat)) # settings for plots
  
  # plots
  plots(results, method, parameter_name, model_settings)
  screeplots(results2, method, parameter_name)
  
  # print summary
  paste0(parameters_types[i], '--------------------------------------------') |> 
    print()
  print(cv_mat)
  print(recon_mat)
  print(sig_mat$sig)
}

# 4. Analogue quality assessment (by R.J. Telford) ----
n_analogs = minDC(recon_mat)$minDC
goodpoorbad = quantile(paldist(modern_mat), prob = c(0.05, 0.1))
plot(depth, n_analogs, ylab = "Squared chord distance", xlab = "Depth, cm")
abline(h = goodpoorbad, col = c("orange", "red"))

# append the final result with others
recons = cbind(recons, mat)
