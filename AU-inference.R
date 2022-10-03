####################################################
## DATA IMPORT
InflationUnit <- read.csv("https://raw.githubusercontent.com/ProfNascimento/AlphaUnit/main/InflationUnit.csv")
hist(InflationUnit$Inflation,probability=TRUE,xlim=c(0,1))

## CLASSIC INFERENCE
require(maxLik)
log.L=function(x,alpha0) {
  logvero= sum(
    log(dnorm(log(x)/alpha0))+
      log(2/(alpha0*x))+
          2*log(-(log(x)/alpha0)) )
}

set.seed(123456)
est0 = maxLik(logLik=log.L, start=c(alpha0=0.5), 
              x= as.numeric(InflationUnit$Inflation),
              method="BFGSR")
summary(est0)

## ALPHA UNIT DENSITY
dAU<-function(x,alpha){
  dnorm(log(x)/alpha)*(2/(alpha*x))*(log(x)/alpha)^2}

curve(dAU(x,est0$estimate),col="red",add=T)

#######################
## BAYESIAN INFERENCE
require(rstan)

stanmodel <- "
functions{
real AU_log(real x, real alpha0){
    real prob;
      prob = std_normal_lpdf(log(x)/alpha0)+log(2/(alpha0*x))+2*log(-(log(x)/alpha0));
    return prob;
 }
}
data{
int <lower=0> N;
real Y[N];
}
parameters{
real<lower=0> alpha;
}
model{
// prior
alpha ~ gamma(0.0001,0.0001);
// likelihood
for (i in 1:N){
  Y[i] ~ AU(alpha);
  }
}

"
fit=stan(model_code=stanmodel,
         data=list(N=length(InflationUnit$Inflation), 
                   Y=InflationUnit$Inflation),
         iter=1000,chain=1)

summary(fit)

require(shinystan)
launch_shinystan(fit)
