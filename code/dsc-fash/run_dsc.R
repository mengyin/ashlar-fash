library(dscr)

dsc_fash = new.dsc("dsc-fash","dsc-fash-files")
source("scenarios.R")
source("methods.R")
source("score.R")

library(qvalue)
fash2qval_est =function(output){
  if (class(output)=="list"){
    qvalue = output$fit$qvalue
    pi0 = output$fit$fitted.g$pi[1]
    return(list(qvalue=qvalue, pi0=pi0,
                pve=output$pve))
  }else{
    return(list(qvalue=NA, pi0=NA, pve=NA))
  }
} 
pval2qval_est =function(output){
  if (class(output)=="list"){
    qq = qvalue(output$pvalue[!is.na(output$pvalue)])
    qvalue = rep(NA,length(output$pvalue))
    qvalue[!is.na(output$pvalue)] = qq$qval
    return(list(qvalue=qvalue, pi0=qq$pi0,
                pve=output$pve))
  }else{
    return(list(qvalue=NA, pi0=NA, pve=NA))
  }
}
addOutputParser(dsc_fash,"pval2qval",pval2qval_est,"pval_output","qval_output")
addOutputParser(dsc_fash,"fash2qval",fash2qval_est,"fash_output","qval_output")

addScore(dsc_fash,score,name="score",outputtype="qval_output")

res = run_dsc(dsc_fash)

save(res,file="res.Rdata")

# library(ggplot2)
# library(tidyr)
# res = separate(res,scenario,c("scenario","nsamp","ngroup"),",")
# res$nsamp = factor(res$nsamp, levels=c("nsamp=2","nsamp=10","nsamp=50"))
# res$ngroup = factor(res$ngroup, levels=c("ngroup=3","ngroup=5","ngroup=10"))
# 
# ggplot(res, aes(pi0,pi0.est,colour=method))+
#   facet_grid(nsamp+ngroup~scenario) + geom_point(shape=1) +xlim(0,1) +ylim(0,1) + 
#   xlab("true pi0") +ylab("estimated pi0") +geom_abline(slope=1,intercept=0,color=1) + coord_equal()
# 
# ggplot(res, aes(pi0,FDP_005,colour=method)) + 
#   facet_grid(nsamp+ngroup~scenario) + geom_point(shape=1) +xlim(0,1) +ylim(0,0.2) + 
#   xlab("true pi0") +ylab("false discovery proportion when q=0.05") +geom_abline(slope=0,intercept=0.05,color=1)
