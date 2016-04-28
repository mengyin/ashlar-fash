library(limma)

# Moderated t-test (Smyth, 2005)
# Using R package "limma"
# input: Y - data matrix
limma_fash.wrapper = function(input,args){
  lim = lmFit(input$Y, model.matrix(~input$condition))
  lim = eBayes(lim[,-1])
  fit = fash(lim$F, input$df1, lim$df.total[1], oneside=TRUE)
  pve = pve_plugin(input$df2/(input$df1+1)+1, fit)
  return(list(fit=fit, pve=pve))
}