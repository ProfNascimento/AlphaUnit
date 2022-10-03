#########################################
## Generate random numbers for AU model
#########################################
### Normal Bimodal (sqrt(2)) - NB(sqrt(2))
sol<-c()
n<-1000
for (i in 1:n) {
  y<-rchisq(1,3)
  u<-runif(1)
  if (u<1/2){j<-sqrt(y)}else{j<--sqrt(y)}
  sol[i]<-j
}
sol
hist(sol,freq=F,ylim=c(0,0.3))

dens<-function(x){(x^2)*dnorm(x)}
curve(dens(x),-6,6,add=T,col="red")
abline(v=sqrt(2),col="blue")
abline(v=-sqrt(2),col="blue")

### Half Normal Bimodal - HNB(0.8)
sol<-c()
n<-1000
for (i in 1:n) {
  y<-rchisq(1,3)
  u<-runif(1)
  if (u<1/2){j<-sqrt(y)}else{j<--sqrt(y)}
  sol[i]<-j
}
newdend<-0.8*abs(sol)
densdhalfalpha<-function(x,alpha){dnorm(x/alpha)*(2/alpha)*(x/alpha)^2}
hist(newdend,freq=F,breaks = 20)
curve(densdhalfalpha(x,0.8),0,10,col="red",add=T)

### Unit Half Bimodal (ALPHA UNIT)
sol<-c()
n<-1000
for (i in 1:n) {
  y<-rchisq(1,3)
  u<-runif(1)
  if (u<1/2){j<-sqrt(y)}else{j<--sqrt(y)}
  sol[i]<-j
}
new<-exp(-0.5*abs(sol))   #datos generados
hist(new,freq=F)
dAU<-function(x,alpha){
  dnorm(log(x)/alpha)*(2/(alpha*x))*(log(x)/alpha)^2}
curve(densdunitedNB(x,0.5),0.001,1,col="red",add=T)
