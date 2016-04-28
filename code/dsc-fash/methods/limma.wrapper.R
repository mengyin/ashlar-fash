library(limma)

# Moderated t-test (Smyth, 2005)
# Using R package "limma"
# input: Y - data matrix
limma.wrapper = function(input,args){
  fit = lmFit(input$Y, model.matrix(~input$condition))
  fit = eBayes(fit[,-1])
  pvalue = fit$F.p.value
  
  pve = pve_ftest(input$df2/(input$df1+1)+1, input$MST, fit$s2.post)
  return(list(pvalue=pvalue, pve=pve))
}