## ----echo=FALSE, out.width="100%"----------------------------------------
library(knitr)
include_graphics("media/banner-07.jpg")
rm(list=objects())

## ------------------------------------------------------------------------
#install.packages("msm")
#install.packages("MDPtoolbox")

## ----echo=FALSE, out.width="100%"----------------------------------------
library(knitr)
include_graphics("media/class-21/Picture1.png")

## ------------------------------------------------------------------------
lambda <- 10 
brks<-c(0,8,11,20,1000)
outcomes<-rpois(50000,lambda)
states<- cut(x=outcomes,
    breaks= brks,
    labels=c("0-8","8-11","11-20","20+"),
    include.lowest=TRUE)
counts<- table(states)
probs<- prop.table(counts)
probs

## ------------------------------------------------------------------------
values<-c(10,15,21,23)
weightedValues<- values*probs
# EXPECTED VALUE WITH PERFECT INFORMATION
ev<- sum(weightedValues) 

## ------------------------------------------------------------------------
# prior probability of presence 
priorPresent<- 0.5 
#prior probability of absence 
priorAbsent<- 1- priorPresent 

## ------------------------------------------------------------------------
#probability of detection given presence 
detectionProbability<-0.8 
#probability not detection given absence  (no false positives)
nonDetecionProbability <-1 

## ------------------------------------------------------------------------
# PROBABILITY OF DETECTION GIVEN PRESENT
detections<- rbinom(500000,1,detectionProbability)
counts<- table(detections)
probs<- prop.table(counts)
probs[1] # Present and not detected
probs[2] # Present and detected

## ------------------------------------------------------------------------
post<-probs[1]*priorPresent/
    (probs[1]*priorPresent + nonDetecionProbability*priorAbsent) 
post 

## ------------------------------------------------------------------------
post<-nonDetecionProbability*priorAbsent/
    (probs[1]*priorPresent + nonDetecionProbability*priorAbsent) 
post 

## ------------------------------------------------------------------------
Z<-matrix(c(0.8,0.2,0,1),
    ncol=2, 
    byrow=TRUE,
    dimnames=list(c("Present","Not present"),
     c("Detected","Not detected")))
Z

## ------------------------------------------------------------------------
p<-matrix(c(0.5,0.5),ncol=2,nrow=2, byrow=TRUE,
    dimnames=list(c("Present","Not present")))

## ------------------------------------------------------------------------
xx<-t(Z) *p
xx/rowSums(xx)

## ----echo=FALSE, out.width="100%"----------------------------------------
library(knitr)
include_graphics("media/class-21/evii.png")

## ------------------------------------------------------------------------
pDetection<-0.23
sites<-c(1,2,3,5,10,20,30,50,75,100)
prob<- c()
for(i in 1:length(sites))
    {
    prob<- c(prob, 
    # proportion of reps with at least 1 site detected
    mean(rbinom(5000,sites[i],pDetection)>=1))
    }
probs<- cbind(sites,prob,1-prob)  

## ------------------------------------------------------------------------
evii<- c(0.48, 0.87, 1.16, 1.58, 2.04, 2.19, 2.20, 2.20, 2.20, 2.20) 

## ------------------------------------------------------------------------
plot(x=sites,y=evii,
    xlab="Number of sites to sample",
    ylab="Value of information",
    pch=19)
abline(h=2.2,lty=2)# Value of perfect information

## ----echo=FALSE, out.width="100%"----------------------------------------
library(knitr)
include_graphics("media/class-21/amphib-evi.png")

## ----echo=FALSE, out.width="100%"----------------------------------------
library(knitr)
include_graphics("media/class-21/amphib-evii.png")

## ------------------------------------------------------------------------
est.flow.mod_decrease.power <- 30
est.predation.mod_decrease.power <- 15
est.flow.mod_control.flatheads <- 15
est.predation.mod_control.flatheads <- 25

## ------------------------------------------------------------------------
Flow <- 0.5
Predation <- 0.5

## ------------------------------------------------------------------------

Decrease.power.generation<- Flow*est.flow.mod_decrease.power + Predation*est.predation.mod_decrease.power
Control.flatheads <- Flow*est.flow.mod_control.flatheads + Predation*est.predation.mod_control.flatheads

Decrease.power.generation
Control.flatheads

## ------------------------------------------------------------------------
## observed abundance after management action Decrease.power.generation
obs<- 21

## conditional Likelihoods for Decrease.power.generation action
flow.like<-dpois(obs,est.flow.mod_decrease.power)
predation.like<-dpois(obs,est.predation.mod_decrease.power)

## ------------------------------------------------------------------------
## used Bayes rule to calculate posterior probabilities
Flow.post<-flow.like*Flow/(flow.like*Flow + predation.like*Predation)
Predation.post<- predation.like*Predation/(flow.like*Flow + predation.like*Predation)

# New model weights 
Flow.post
Predation.post

## ------------------------------------------------------------------------
## These posteriors become the weights for the next time step
Flow <- Flow.post
Predation <- Predation.post

## expected value
Decrease.power.generation<- Flow*est.flow.mod_decrease.power + Predation*est.predation.mod_decrease.power
Control.flatheads <- Flow*est.flow.mod_control.flatheads + Predation*est.predation.mod_control.flatheads

Decrease.power.generation
Control.flatheads

## observed abundance after management action Control.flatheads
obs<- 18

## conditional Likelihoods for Control.flatheads
flow.like<-dpois(obs,est.flow.mod_control.flatheads)
predation.like<-dpois(obs,est.predation.mod_control.flatheads)


## used Bayes rule to calculate posterior probabilities
Flow.post<-flow.like*Flow/(flow.like*Flow + predation.like*Predation)
Predation.post<- predation.like*Predation/(flow.like*Flow + predation.like*Predation)

# New model weights, these will become the prior for the next time step
Flow.post
Predation.post

## ----message=FALSE,warning=FALSE-----------------------------------------
### Library needed for truncated normal
library(msm)
### library needed for SDP
library(MDPtoolbox)

rm(list=ls())

## ------------------------------------------------------------------------

## we will create transition matrices by simulating 
## harvest and population dynamics note that the way it is set up
## indicates that the harvest decision was based on the spring population
## before reproduction Also note that we are not keeping track of the number of 
## ponds to aid in decision making

combo<-merge(c(1:15),c(0:4*0.1))
colnames(combo)<-c("N_t","H_t")

attach(combo)

H_t<- rep(H_t,500)
N_t<- rep(N_t,500)
## choose harvest mortality model
har_type<-'AMH'
R_t<- 16
P_t <- 2
P_t_N=max(0.0001,-2.76+0.391*P_t+0.233*R_t)# KEEP FROM GOING NEGATIVE (equation 10 (Anderson 1975))  
#  young production to be added to fall population (equation 2 (Anderson 1975))
Y_t  = 1/((1/(12.48*P_t^0.851))+(0.519/N_t)) 

Y_t <- rtnorm(length(Y_t),Y_t,Y_t*0.3,lower = 1)

  # Fall population at time t (equation 5 (Anderson 1975))
  F_t	= (0.92*N_t) + Y_t 
  harvest<-ifelse(F_t < H_t*F_t,F_t, H_t*F_t)# harvest at time t (need to keep track of this)
  
  # AMH: ADDITIVE MORTALITY
  survival_adult_amh<- (1-0.27*exp(2.08*H_t))
  survival_young_amh<- (1-0.40*exp(0.67*H_t))
  
  # CMH: COMPENSATORY MORTALITY
  survival_adult_cmh<- ifelse(H_t<0.25,0.57,(0.57-1.2*(H_t-0.25)))
  survival_young_cmh<-  ifelse(H_t<0.25,0.5,(0.5-1*(H_t-0.25)))
  
  # Pop size after spring migration
  # AMH: ADDITIVE MORTALITY
  N.t1.amh<- N_t*survival_adult_amh + Y_t*survival_young_amh
  
  # CMH: COMPENSATORY MORTALITY
  N.t1.cmh<- N_t*survival_adult_cmh + Y_t*survival_young_cmh
  
  ## Weight and add model specific population size estimates
  harvest_type<- ifelse(har_type=='AMH', 1, 0)   
  N_t_N<- N.t1.amh*harvest_type + N.t1.cmh*(1-harvest_type)
  
  
# Discretize population sizes into 3 states
# Could do more classes this is just to simplify
Initial_N<-floor(N_t/5.5)
### prevents new pop states from arising
N_t_N <- ifelse(N_t_N < 1, 1, N_t_N)
End_N <-ifelse(floor(N_t_N/5.5)> 2, 2,floor(N_t_N/5.5)) 

## create a table of transition frequencies that will be turned into 
## state transition probabilities one for each decision alternative
TM<- table(Initial_N,End_N,H_t)

### These are now transition matrices one for each  harvest decision alternative
TM_1 <- prop.table(TM[,,1],1)
TM_2 <- prop.table(TM[,,2],1)
TM_3 <- prop.table(TM[,,3],1)
TM_4 <- prop.table(TM[,,4],1)
TM_5 <- prop.table(TM[,,5],1)


## calculate the average (expected) return for each population
## state / decision alternative combination
Return<-tapply(harvest,list(Initial_N,H_t), mean)


# Set up arrays for solving 
P <- array(0, c(3,3,5))
P[,,1] <- TM_1
P[,,2] <- TM_2
P[,,3] <- TM_3
P[,,4] <- TM_4
P[,,5] <- TM_5
R <- Return


### now find optimal state dependent harvest 
mdp_policy_iteration(P, R, discount=.99999)


  

