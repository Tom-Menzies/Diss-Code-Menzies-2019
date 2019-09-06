memory.limit(10000000000000)

library(plot3D)
library(bindata)
library(mvtnorm)
# Functions
SLoS <- function(p, dir, w=0.5) {
  return( (1/p[which(dir==1)])^w + (1/(1-p[which(dir==0)]))^(1-w))
}

PSLoS <- function(p, dir, w=0.5){
  return((1/p[which(dir==1)])^w * (1/(1-p[which(dir==0)]))^(1-w))
}

MLSLoS <- function(p, dir, w=0.5){
  return((1/p[which(dir==1)])^(2*w/3) + (1/(1-p[which(dir==0)]))^(2*(1-w)/3) + (1/(p[which(dir==1)]*(1-p[which(dir==0)])))^((w/3)+((1-w)/3)) )
}

Linear <- function(p, dir, w=0.5) {
  return(sum(w*p*dir + (1-w)*(1-p)*(1-dir)))
}

Product <- function(p, dir, w=0.5) {
  return(sum((p[which(dir==1)])^(w) * ((1-p[which(dir==0)]))^(1-w)))
}

ML <- function(p, dir, w=0.5) {
  return(sum((2*w/3)*p[which(dir==1)] + (2*(1-w)/3)*(1-p[which(dir==0)]) + ((w/3)+((1-w)/3))*(p[which(dir==1)])*(1-p[which(dir==0)]) ))
}

# Function to generate n_sim Beta distributed variables according to a nb of success and a nb total of patients
rbeta_fnct <- function(n_success, n_total, n_sim) {
  return(rbeta(n_sim, n_success, (n_total - n_success)))
}
# Correlated MultiVariate Binomial
rmvbin_fnct <- function(n_sim, prob, rho) {
  return(apply(rmvbin(n_sim, margprob=prob, sigma=cbind(c(1, rho),c(rho, 1))), 2, sum))
}



# binomial (correlated)
simul_br_corr <- function(prob, rho, weight_l=0.5, weight_u=0.5) {
  success = t(sapply(rep(n, nsim1), rmvbin_fnct,  prob=prob, rho=rho))
  
  marg.prob =  apply(success, c(1,2), rbeta_fnct, n_sim=nsim2, n_total = n) 
  
  l1= apply(marg.prob, c(2, 1), SLoS, dir=dir.t, w=weight_l)
  l2= apply(marg.prob, c(2, 1), PSLoS, dir=dir.t, w=weight_u)
  l3= apply(marg.prob, c(2, 1), MLSLoS, dir=dir.t, w=weight_l)
  u1= apply(marg.prob, c(2, 1), Linear, dir=dir.t, w=weight_u)
  u2= apply(marg.prob, c(2, 1), Product, dir=dir.t, w=weight_l)
  u3= apply(marg.prob, c(2, 1), ML, dir=dir.t, w=weight_u)
  
  return(list("l1"=l1, "l2"=l2, "l3"=l3, "u1"=u1, "u2"=u2, "u3"=u3))
}

# Map weights from normal to SLoS
mapping_func<-function(w){
  y<-(w/(1-w)) * 2 ^(2*w-1)
  return(y)
}

search<-function(target){
  y<-der_mid(w=seq(0.0001,1,0.0001))
  z<-which.min(abs(y-target))
  we<-seq(0.0001,1,0.0001)[z]
  return(we)
}

#run functions_correlations file
#data
set.seed(2019)

nsim1=2500 # trials
nsim2=2000 # sims
dir.t = c(1, 0)
n=100 # patients per trial (binomial/MVTNorm)
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


# p=0.8
corr=0.8
t1_pc = apply(scen1, 1, simul_br_corr, rho=corr)
t2_pc = apply(scen2, 1, simul_br_corr, rho=corr)
p_corr_results_u1_2=Results_fnct_pcorr_u1(t1_pc, t2_pc)
p_corr_results_u2_2=Results_fnct_pcorr_u2(t1_pc, t2_pc)
p_corr_results_u3_2=Results_fnct_pcorr_u3(t1_pc, t2_pc)
p_corr_results_l1_2=Results_fnct_pcorr_l1(t1_pc, t2_pc)
p_corr_results_l2_2=Results_fnct_pcorr_l2(t1_pc, t2_pc)
p_corr_results_l3_2=Results_fnct_pcorr_l3(t1_pc, t2_pc)


Saveprob_fnct_u1_corr_p(p_corr_results_u1_2, "_corrpos80_u1cp_5")
Saveprob_fnct_u2_corr_p(p_corr_results_u2_2, "_corrpos80_u2cp_5")
Saveprob_fnct_u3_corr_p(p_corr_results_u3_2, "_corrpos80_u3cp_5")
Saveprob_fnct_l1_corr_p(p_corr_results_l2_2, "_corrpos80_l1cp_5")
Saveprob_fnct_l2_corr_p(p_corr_results_l2_2, "_corrpos80_l2cp_5")
Saveprob_fnct_l3_corr_p(p_corr_results_l3_2, "_corrpos80_l3cp_5")

