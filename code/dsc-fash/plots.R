library(ggplot2)
library(tidyr)
res = separate(res,scenario,c("scenario","nsamp","ngroup"),",")
res$nsamp = factor(res$nsamp, levels=c("nsamp=2","nsamp=10","nsamp=50"))
res$ngroup = factor(res$ngroup, levels=c("ngroup=3","ngroup=5","ngroup=10"))
res$scenario = factor(res$scenario, levels=c("spiky","near_normal","flat_top","big-normal"))

ggplot(res, aes(pi0,pi0.est,colour=method))+
  facet_grid(ngroup+nsamp~scenario) + geom_point(shape=1) +xlim(0,1) +ylim(0,1) + 
  xlab("true pi0") +ylab("estimated pi0") +geom_abline(slope=1,intercept=0,color=1) + coord_equal()

ggplot(res, aes(pi0,FDP_005,colour=method)) + 
  facet_grid(ngroup+nsamp~scenario) + geom_point(shape=1) +xlim(0,1) +ylim(0,0.2) + 
  xlab("true pi0") +ylab("false discovery proportion when q=0.05") +geom_abline(slope=0,intercept=0.05,color=1)

ggplot(res, aes(pi0,DP_005,colour=method)) + 
  facet_grid(ngroup+nsamp~scenario) + geom_point(shape=1) +xlim(0,1) +ylim(0,1) + 
  xlab("true pi0") +ylab("discovery proportion when q=0.05") +geom_abline(slope=-1,intercept=1,color=1)

ggplot(res, aes(pi0,rmse.pve,colour=method)) + 
  facet_grid(ngroup+nsamp~scenario) + geom_point(shape=1) +xlim(0,1)+
  xlab("true pi0") +ylab("rmse.pve")

#####
library(dplyr)
newres = res
newres = newres[newres$method=='ftest',]
newres = newres[c(2:5,12)]
names(newres)[5] = c('rmse.pve_baseline')
test = left_join(res,newres,by=c('seed','scenario','ngroup','nsamp'))
test$rmse.pve_rel = test$rmse.pve/test$rmse.pve_baseline # RRMSE_prec

ggplot(test, aes(pi0,rmse.pve_rel,colour=method)) + 
  facet_grid(ngroup+nsamp~scenario) + geom_point(shape=1) +xlim(0,1)+
  xlab("true pi0") +ylab("relative rmse.pve")
