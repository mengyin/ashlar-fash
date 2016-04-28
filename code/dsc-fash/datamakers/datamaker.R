# simulate data Y_{gi}=\mu_{gi}+\beta_{g,c(i)}+e_{gi}
# where \beta~N(0,sigma_g^2), e~N(0,s^2)
# and log(alpha):=log(1+nsamp*sigma_g^2/s^2)
# then Fstat~alpha*F(ngroup-1, nsamp*ngroup-ngroup)

# ngene: # of genes
# nsamp: # of samples for each group
# ngroup: # of groups
# mumean, musd: generate true mu's from N(logrmean, logrsd)
datamaker = function(args){  
  #sigma.e = rep(1,args$ngene)
  if(is.null(args$errshape)){
    args$errshape = 5
  }
  if(is.null(args$errrate)){
    args$errrate = 5
  }
  sigma.e = sqrt(rgamma(args$ngene, args$errshape, args$errrate))
  
  # logalpha generated from normal mixture
  loga = gen_normalmix(args$ngene, 
                       args$logaargs$pi, args$logaargs$mu, args$logaargs$sd, 
                       args$pi0)
  logalpha = abs(loga$beta) # must be postive
  null = loga$null
  
  # logalpha:=log((1+nsamp*sigma.c^2/sigma.e^2))
  sigma.c = sqrt((exp(logalpha)-1)/args$nsamp*sigma.e^2)
  
  mu = rep(0,args$ngene)
  beta = matrix(rep(rnorm(args$ngene*args$ngroup, 0, rep(sigma.c,each=args$ngroup)), each=args$nsamp), 
                nrow=args$ngene, byrow=TRUE)
  e = matrix(rnorm(args$ngene*args$ngroup*args$nsamp, 0, rep(sigma.e, each=args$ngroup*args$nsamp)), 
             nrow=args$ngene, byrow=TRUE)
  
  Y = mu+beta+e
  condition = factor(rep(1:args$ngroup,each=args$nsamp))
  
  ftests = apply(Y,1,get_fstat,condition=condition)
  fstat = ftests[1,]
  MST = ftests[2,]
  MSE = ftests[3,]
  
  meta = list(pi0=loga$pi0, logalpha=logalpha, null=null, pve=sigma.c^2/(sigma.c^2+sigma.e^2),
              logaprior=list(pi=c(loga$pi0,args$logaargs$pi/(1-loga$pi0)),
                             mu=c(0,args$loagargs$mu),
                             sd=c(0,args$loagargs$sd)), 
              args=args)
  
  input = list(Y=Y, condition=condition, fstat=fstat, MST=MST, MSE=MSE,
               df1=args$ngroup-1, df2=(args$nsamp-1)*args$ngroup)
  data = list(meta=meta,input=input)
  return(data)
}

get_fstat = function(y,condition){
  fit = lm(y~condition)
  fstat = anova(fit)$F[1]
  MST = anova(fit)$M[1]
  MSE = anova(fit)$M[2]
  return(c(fstat,MST,MSE))
}

# Generate beta from normal mixture prior
gen_normalmix = function(ngene, pi, mu, sd, pi0){
  if (pi0=="random"){
    pi0 = runif(1,0,1) #generate the proportion of true nulls randomly
  }
  k = length(pi) # number of components
  comp = sample(1:k,ngene,pi,replace=TRUE) #randomly draw a component
  isnull = (runif(ngene,0,1) < pi0)
  beta = ifelse(isnull, 0, rnorm(ngene,mu[comp],sd[comp]))
  return(list(beta=beta, pi0=pi0, null=isnull))
}

