# Random Forest reconstructions statistical significance wrapper functions ---- 

# rfWrap ----
# written by : Richard J. Telford
# purpose    : to provide RF data for palaeoSig functions

rfWrap = function(spp, ev,...){
  mod = randomForest(env~. ,data = cbind(env = ev, spp))
  res = list(mod = mod)
  class(res) = 'rfWrap'
  res
}

# predict.rfWrap ----
# written by : Richard J. Telford
# purpose    : prediction with rfWrap function

predict.rfWrap = function(object, newdata,...){
  pred = predict(object$mod, newdata = newdata)
  list(fit = data.frame(p = pred))
}
