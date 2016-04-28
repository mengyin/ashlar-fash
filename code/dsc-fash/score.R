#This file should define your score function
library(AUC)

score = function(data, output){
  if (class(output)=="list"){
    pi0 = data$meta$pi0
    pi0.est = output$pi0
    DP_005 = mean(output$qvalue<=0.05,na.rm=TRUE)
    FDP_005 = sum(data$meta$null==1 & output$qvalue<=0.05,na.rm=TRUE)/sum(output$qvalue<=0.05,na.rm=TRUE)
    AUC = auc(roc(output$qvalue,factor(data$meta$null)))
    rmse.pve = sqrt(mean((data$meta$pve-output$pve)^2))
    
    res = c(pi0, pi0.est, DP_005, FDP_005, AUC, rmse.pve)
    names(res) = c("pi0","pi0.est","DP_005","FDP_005","AUC","rmse.pve") 
    return(res)
  }else{
    res = rep(NA, 6)
    names(res) = c("pi0","pi0.est","DP_005","FDP_005","AUC","rmse.pve") 
    return(res)
  } 
}
