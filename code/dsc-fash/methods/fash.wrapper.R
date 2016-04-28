# fash

source("../fash.R")
fash.wrapper = function(input,args){
  fit = fash(input$fstat, input$df1, input$df2, oneside=TRUE)
  pve = pve_plugin(input$df2/(input$df1+1)+1, fit)
  return(list(fit=fit, pve=pve))
}

# PVE = sigma^2/s^2
# logalpha = (1+nsamp*sigma^2/s^2)
# So: PVE = (exp(logalpha)-1)/(exp(logalpha)-1+nsamp)
pve_plugin = function(nsamp,fashobj){
  (exp(fashobj$PosteriorMean.logf)-1)/(exp(fashobj$PosteriorMean.logf)-1+nsamp)
}
