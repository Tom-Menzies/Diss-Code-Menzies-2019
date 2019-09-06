library(plot3D)
library(bindata)
memory.limit(1000000000000)
loss_br <- function(p, dir, w=0.5) {
  return( (1/p[which(dir==1)])^w + (1/(1-p[which(dir==0)]))^(1-w))
}
loss_br2 <- function(p, dir, w=0.5){
  return((1/p[which(dir==1)])^w * (1/(1-p[which(dir==0)]))^(1-w))
}
loss_br3 <- function(p, dir, w=0.5){
  return((1/p[which(dir==1)])^(2*w/3) + (1/(1-p[which(dir==0)]))^(2*(1-w)/3) + (1/(p[which(dir==1)]*(1-p[which(dir==0)])))^((w/3)+((1-w)/3)) )
}
us_br <- function(p, dir, w=0.5) {
  return(sum(w*p*dir + (1-w)*(1-p)*(1-dir)))
}
us_br2 <- function(p, dir, w=0.5) {
  return(sum((p[which(dir==1)])^(w) * ((1-p[which(dir==0)]))^(1-w)))
}
us_br3 <- function(p, dir, w=0.5) {
  return(sum((2*w/3)*p[which(dir==1)] + (2*(1-w)/3)*(1-p[which(dir==0)]) + ((w/3)+((1-w)/3))*(p[which(dir==1)])*(1-p[which(dir==0)]) ))
}
us_br3(c(0.5,0.5),c(1,0))
# Function to generate n_sim Beta distributed variables according to a nb of success and a nb total of patients
rbeta_fnct <- function(n_success, n_total, n_sim) {
  return(rbeta(n_sim, n_success, (n_total - n_success)))
}
# Function to generate the simulations
simul_br <- function(prob, weight_l=0.5, weight_u=0.5) {
  success = sapply(prob, rbinom, n=nsim1, size=n)
  
  marg.prob =  apply(success, c(1,2), rbeta_fnct, n_sim=nsim2, n_total = n) 
  
  loss = apply(marg.prob, c(2, 1), loss_br, dir=dir.t, w=weight_l)
  loss2 = apply(marg.prob, c(2, 1), loss_br2, dir=dir.t, w=weight_l)
  loss3 = apply(marg.prob, c(2, 1), loss_br3, dir=dir.t, w=weight_l)
  us   = apply(marg.prob, c(2, 1), us_br, dir=dir.t, w=weight_u)
  us2 = apply(marg.prob, c(2, 1), us_br2, dir=dir.t, w=weight_u)
  us3 = apply(marg.prob, c(2, 1), us_br3, dir=dir.t, w=weight_u)
  return(list("loss"=loss, "loss2"=loss2, "loss3"=loss3, "utility"=us, "utility2"=us2, "utility3"=us3))
}
# Function to map the weights
der_mid<-function(w){
  y<-(w/(1-w)) * 2 ^(2*w-1)
  return(y)
}
search<-function(target){
  y<-der_mid(w=seq(0.0001,1,0.0001))
  z<-which.min(abs(y-target))
  we<-seq(0.0001,1,0.0001)[z]
  return(we)
}
 


set.seed(2019)
nsim1=2500 # nb of clinical trials
nsim2=2000 # nb of simulations to obtain the posterior distributions
dir.t = c(1, 0)
n=100 # nb of patients per arm for each simulated trial
scen1 = matrix(c( 0.5, 0.5, 
                  0.3, 0.7,
                  0.7, 0.3, 
                  0.1, 0.1, 
                  0.9, 0.9,
                  0.3, 0.3, 
                  0.7, 0.7,
                  0.1, 0.9,
                  0.9, 0.1
), nrow=9, byrow=T) # fixed scenarios with the marginal proba of event, trt1
b2=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
r2=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
scen2 =   expand.grid(b2, r2) # scenarios with the marginal proba of event, trt2
nscen1=nrow(scen1) #  nb of scenarios
nscen2=nrow(scen2) #  nb of scenarios
############################################################################
# 1. No correlations, equal weights
############################################################################

trt1 = apply(scen1, 1, simul_br)
trt2 = apply(scen2, 1, simul_br)
res_1_u1=Res_fnct_u1(trt1, trt2)
res_1_u2=Res_fnct_u2(trt1, trt2)
res_1_u3=Res_fnct_u3(trt1, trt2)
res_1_l1=Res_fnct_l1(trt1, trt2)
res_1_l2=Res_fnct_l2(trt1, trt2)
res_1_l3=Res_fnct_l3(trt1, trt2)

# Contour plots for each measure + diff

Saveprob_fnct_u1(res_1_u1, "u1_test")
Saveprob_fnct_u2(res_1_u2, "u2_test")
Saveprob_fnct_u3(res_1_u3, "u3_test")
Saveprob_fnct_l1(res_1_l1, "l1_test")
Saveprob_fnct_l2(res_1_l2, "l2_test")
Saveprob_fnct_l3(res_1_l3, "l3_test")

