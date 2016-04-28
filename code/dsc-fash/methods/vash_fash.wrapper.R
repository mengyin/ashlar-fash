vash_fash.wrapper = function(input,args){
  va = vash(sqrt(input$MSE),df=input$df2,singlecomp=TRUE)
  F.tilde = input$MST/va$sd.post^2
  fit = fash(F.tilde, input$df1, 2*va$PosteriorShape[1], oneside=TRUE)
  pve = pve_plugin(input$df2/(input$df1+1)+1, fit)
  return(list(fit=fit, pve=pve))
}
