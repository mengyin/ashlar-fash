sourceDir("datamakers")

# addScenario(dsc_fash,name=paste0("spiky,nsamp=",2,"ngroup=",3), 
#             fn=datamaker,
#             args=list(ngene=5000, nsamp=2, ngroup=3, pi0="random",
#                       logaargs=list(pi=c(.4,.2,.2,.2),mu=c(0,0,0,0),sd=c(.25,.5,1,2))),
#             seed=1:1)

for (nsamp in c(2,10,50)){
  for (ngroup in c(3,5,10)){
    addScenario(dsc_fash,name=paste0("spiky,nsamp=",nsamp,",ngroup=",ngroup), 
                fn=datamaker,
                args=list(ngene=5000, nsamp=nsamp, ngroup=ngroup, pi0="random",
                          logaargs=list(pi=c(.4,.2,.2,.2),mu=c(0,0,0,0),sd=c(.25,.5,1,2))),
                seed=1:30)
    
    addScenario(dsc_fash,name=paste0("near_normal,nsamp=",nsamp,",ngroup=",ngroup), 
                fn=datamaker,
                args=list(ngene=5000, nsamp=nsamp, ngroup=ngroup, pi0="random",
                          logaargs=list(pi=c(2/3,1/3),mu=c(0,0),sd=c(1,2))),
                seed=1:30)
    
    addScenario(dsc_fash,name=paste0("flat_top,nsamp=",nsamp,",ngroup=",ngroup), 
                fn=datamaker,
                args=list(ngene=5000, nsamp=nsamp, ngroup=ngroup, pi0="random",
                          logaargs=list(pi=rep(1/7,7),mu=c(-1.5,-1,-0.5,0,0.5,1,1.5),sd=rep(0.5,7))),
                seed=1:30)
    
    addScenario(dsc_fash,name=paste0("big-normal,nsamp=",nsamp,",ngroup=",ngroup), 
                fn=datamaker,
                args=list(ngene=5000, nsamp=nsamp, ngroup=ngroup, pi0="random",
                          logaargs=list(pi=c(1),mu=c(0),sd=c(4))),
                seed=1:30)
  }
  
}




