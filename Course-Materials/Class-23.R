## ----echo=FALSE, out.width="100%"----------------------------------------
library(knitr)
include_graphics("media/banner-10.jpg")

## ------------------------------------------------------------------------
# install.packages("MDPtoolbox")
# install.packages("msm")
# install.packages("fitdistrplus")
# install.packages("Hmisc")

## ----echo=FALSE, out.width="100%"----------------------------------------
library(knitr)
include_graphics("media/class-22/loop-learning.png")

## ----echo=FALSE, out.width="50%"-----------------------------------------
library(knitr)
include_graphics("media/class-23/conflict.png")

## ----echo=FALSE, out.width="75%"-----------------------------------------
knitr::include_graphics("media/class-23/ducks-01.png")

## ----echo=FALSE, out.width="100%"----------------------------------------
knitr::include_graphics("media/class-23/ducks-02-policies.png")

## ------------------------------------------------------------------------
## Stochastic dynamic programming requires this library
library(MDPtoolbox)

## ------------------------------------------------------------------------
decisions<- c(0.1,0.2,0.3)
abundance<- c(5,10,15)
# With a decision specific transition matrices matrix
P <- array(0, c(3,3,3))

## ------------------------------------------------------------------------
#### weight of model 1
mod1.wt<- 0.5

## ------------------------------------------------------------------------
## decision 1 harvest = 0.1
Model1.1 <- matrix(c(0.2, 0.5, 0.3, 0.2,.3,0.5,0.1,0.3,0.6), 3, 3, byrow=TRUE)
Model2.1 <- matrix(c(0.3, 0.5, 0.2, 0.3,0.3,0.4,0.2,0.3,0.5), 3, 3, byrow=TRUE)
## model averaged transition matrix
P[,,1] <- Model1.1*mod1.wt + Model2.1*(1-mod1.wt)

## ------------------------------------------------------------------------
## decision 1 harvest = 0.2
Model1.2<- matrix(c(0.5,0.3,0.2,0.2,0.5,0.3,0.2,0.4,0.4), 3, 3, byrow=TRUE)
Model2.2<- matrix(c(0.7,0.3,0.0,0.4,0.5,0.1,0.5,0.4,0.1), 3, 3, byrow=TRUE)
P[,,2] <-  Model1.2*mod1.wt + Model2.2*(1-mod1.wt)

## ------------------------------------------------------------------------
## decision 1 harvest = 0.3
Model1.3<- matrix(c(0.7,0.3,0.0,0.6,0.3,0.1,0.3,0.5,0.2), 3, 3, byrow=TRUE)
Model2.3<- matrix(c(0.9,0.1,0.0,0.7,0.3,0.0,0.4,0.6,0.0), 3, 3, byrow=TRUE)
P[,,3] <-  Model1.3*mod1.wt + Model2.3*(1-mod1.wt)

## ------------------------------------------------------------------------
##Reward matrix
R <- matrix(c(0.5,1.0,1.5,
    1,2,3,
    1.5,3.0,4.5), 
    nrow=3, 
    ncol=3, 
    byrow=TRUE)

## ------------------------------------------------------------------------
## heres the better way it automatically  iterates and stops when the policy is stable
out<-mdp_policy_iteration(P=P, R=R, discount=.99999)

## ------------------------------------------------------------------------
policyTable<- data.frame(Abundance= abundance,
    HarvestRate=decisions[out$policy])

## ------------------------------------------------------------------------
N_t<- c(1:15)
H_t<- seq(0,0.4,by = 0.1)
combo<-expand.grid(
    N_t=N_t,
    P_t=2,
    H_t=H_t,
    rep=1:5000)

## ------------------------------------------------------------------------
#  young production to be added to fall population (equation 2 (Anderson 1975))
combo$Y_t  = 1/((1/(12.48*combo$P_t^0.851))+
    (0.519/combo$N_t)) 
# add uncertainty
library(msm) 
set.seed(8483)#for reproducability
combo$Y_t <- rtnorm(length(combo$Y_t),
    combo$Y_t,
    combo$Y_t*0.3,
    lower = 1)

## ------------------------------------------------------------------------
# Fall population at time t (equation 5 (Anderson 1975))
combo$F_t	= (0.92*combo$N_t) + combo$Y_t 

## ------------------------------------------------------------------------
# Harvest at time t (need to keep track of this)
combo$harvest<-ifelse(combo$F_t < combo$H_t*combo$F_t,
    combo$F_t, 
    combo$H_t*combo$F_t)

## ------------------------------------------------------------------------
# AMH: ADDITIVE MORTALITY
combo$survival_adult_amh<- (1-0.27*exp(2.08*combo$H_t))
combo$survival_young_amh<- (1-0.40*exp(0.67*combo$H_t))
# Pop size after spring migration
# AMH: ADDITIVE MORTALITY
combo$N.t1.amh<- combo$N_t*combo$survival_adult_amh + 
    combo$Y_t*combo$survival_young_amh

## ------------------------------------------------------------------------
# CMH: COMPENSATORY MORTALITY
combo$survival_adult_cmh<- ifelse(combo$H_t<0.25,
    0.57,
    (0.57-1.2*(combo$H_t-0.25)))
combo$survival_young_cmh<-  ifelse(combo$H_t<0.25,
    0.5,
    (0.5-1*(combo$H_t-0.25)))
# Pop size after spring migration
# CMH: COMPENSATORY MORTALITY
combo$N.t1.cmh<- combo$N_t*combo$survival_adult_cmh + 
    combo$Y_t*combo$survival_young_cmh

## ------------------------------------------------------------------------
# keep new states from arising
combo$N.t1.amh  <- ifelse(combo$N.t1.amh>15,
    15,
    combo$N.t1.amh)
combo$N.t1.amh  <- floor(ifelse(combo$N.t1.amh<1,
    1,
    combo$N.t1.amh))
combo$N.t1.cmh  <- ifelse(combo$N.t1.cmh>15,
    15,
    combo$N.t1.cmh)
combo$N.t1.cmh  <- floor(ifelse(combo$N.t1.cmh<1,
    1,
    combo$N.t1.cmh))

## ------------------------------------------------------------------------
## create a table of transition frequencies that will be turned into 
## state transition probabilities one for each decision alternative
TM_amh<- table(combo$N_t,combo$N.t1.amh,combo$H_t)
TM_cmh<- table(combo$N_t,combo$N.t1.cmh,combo$H_t)

## ------------------------------------------------------------------------
# These are now transition matrices one for each  
# harvest decision alternative
TM_amh<- prop.table(TM_amh,
    margin=c(1,3))
TM_cmh<- prop.table(TM_cmh,
    margin=c(1,3))

## ------------------------------------------------------------------------
## calculate the average (expected) return for each population
## state / decision alternative combination
harvest<-tapply(X=combo$harvest,
    INDEX=list(combo$N_t,combo$H_t), 
    FUN=mean)

## ------------------------------------------------------------------------
## Weight and add model specific population size estimates
prior_amh<- 0.5
prior_cmh<- (1-prior_amh)
TM<- TM_amh*prior_amh + prior_cmh*TM_cmh

## ------------------------------------------------------------------------
### now find optimal state dependent harvest 
out<- mdp_policy_iteration(P=TM, 
    R=harvest, 
    discount=.999,
    policy0=rep(4,15),
    max_iter=100)

## ------------------------------------------------------------------------
policyTable<- data.frame(Abundance= N_t,
    HarvestRate1=H_t[out$policy])
policyTable

## ------------------------------------------------------------------------
## Weight and add model specific population size estimates
prior_amh<- 0.25
prior_cmh<- (1-prior_amh)
TM<- TM_amh*prior_amh + prior_cmh*TM_cmh

## ------------------------------------------------------------------------
### now find optimal state dependent harvest 
out<- mdp_policy_iteration(P=TM, 
    R=harvest, 
    discount=.999,
    policy0=rep(4,15),
    max_iter=100)

## ------------------------------------------------------------------------
policyTable$HarvestRate2<- H_t[out$policy]
policyTable

## ----echo=FALSE, out.width="100%"----------------------------------------
knitr::include_graphics("media/class-23/ducks-03-monitoring-map.png")

## ------------------------------------------------------------------------
Surv<- c(0.5, 0.6, 0.45, 0.65, 0.45, 0.7) 
s.mean<-mean(Surv) 
s.var<- var(Surv)

## ------------------------------------------------------------------------
### beta method of moments 
beta.mom<-function(mean,v)
    { 
    x<-mean 
    a<-x*(x*(1-x)/v-1) 
    b<-(1-x)*(x*(1-x)/v-1) 
    c(a,b) 
    } 
out<-beta.mom(s.mean,s.var) 
out

## ------------------------------------------------------------------------
library(fitdistrplus) 
fit<- fitdist(Surv,"beta",
    start=c(shape1=out[1],
    shape2=out[2]))
fit

## ------------------------------------------------------------------------
library(Hmisc)
# weights as whole numbers 
wt1<-c(1,1,2,1,2,4) 
# weights as proportions 
wt2<- wt1/sum(wt1) 
#means 
wtd.mean(Surv,wt1) 
wtd.mean(Surv,wt2) 
#variance 
wtd.var(Surv,wt1) 
wtd.var(Surv,wt2) 

out_w<-beta.mom(mean=wtd.mean(Surv,wt1),
    v=wtd.var(Surv,wt2)) 
out_w

## ------------------------------------------------------------------------
S<- seq(0,1,0.01)
unweighted<-dbeta(S, 
    shape1=out[1], 
    shape2=out[2])
plot(S,unweighted,
    col="black",
    type='l', 
    ylab="Density", 
    lwd=2, 
    ylim=c(0,6),
    las=1)
weighted<-dbeta(S, 
    shape1=out_w[1], 
    shape2= out_w[2])
points(S,weighted,
    col="red",
    type='l', 
    lwd=2)
legend("topleft", 
    legend=c("unweighted", "weighted"), 
    lty=1, 
    col=c("black","red"))

## ------------------------------------------------------------------------
# Absent
Many.Open<-c(0.36,0.37,0.36) 
Many.Closed<-c(0.15,0.17,0.17) 
Few.Open<-c(0.67,0.69,0.66) 
Few.Closed<-c(0.44,0.51,0.56) 

#means 
mean(Many.Open) 
mean(Many.Closed) 
mean(Few.Open) 
mean(Few.Closed) 

## ------------------------------------------------------------------------
#expert weights 
wt<-c(5,10,100) 
#weighted means
wtd.mean(Many.Open,wt) 
wtd.mean(Many.Closed,wt) 
wtd.mean(Few.Open,wt) 
wtd.mean(Few.Closed,wt) 



