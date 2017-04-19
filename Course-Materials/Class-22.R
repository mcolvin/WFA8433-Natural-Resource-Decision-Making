## ----echo=FALSE, out.width="100%"----------------------------------------
library(knitr)
include_graphics("media/banner-11.jpg")

## ------------------------------------------------------------------------
deck<- expand.grid(
    suit = c("Diamond", "Club", "Heart", "Spade"),
    card = c("Ace", "Deuce", "Three", "Four","Five", 
             "Six", "Seven", "Eight", "Nine", "Ten", 
             "Jack", "Queen", "King"))
deck$id<-1:nrow(deck) # for sampling later
ncards<-nrow(deck)
prX<- nrow(deck[deck$suit=="Heart",])/ncards #p(x=Heart)
prY<-  nrow(deck[deck$suit=="Spade",])/ncards #p(x=Spade) 

## ------------------------------------------------------------------------
prX
prY

## ------------------------------------------------------------------------
reps<-50000
indx<-sample(nrow(deck),reps,replace=TRUE) # index for card selected
out<-data.frame(firstCardSuit=deck[indx,]$suit)
prop.table(table(out)) # all close to 0.25

## ------------------------------------------------------------------------
out$secondCardSuite<-NA
# SIMULATE THE PROCESS
for(i in 1:reps)
    {
    # SAMPLE ANOTHER CARD AND GET THE SUITE
    id<- sample(deck$id[-indx[i]],1)
    out$secondCardSuit[i]<- as.character(deck$suit[id])
    }

## ------------------------------------------------------------------------
out$tmp<-1
outcomes<-aggregate(tmp~firstCardSuit+secondCardSuit,out,FUN=sum)
outcomes$p<- outcomes$tmp/reps

## ------------------------------------------------------------------------
outcomes

## ------------------------------------------------------------------------
nrow(out[out$firstCardSuit=="Heart" & out$secondCardSuit=="Spade",])/reps
nrow(out[out$firstCardSuit=="Spade" & out$secondCardSuit=="Heart",])/reps

## ------------------------------------------------------------------------
dnorm(140,125,10) #p(x|H_i), 3rd model

## ------------------------------------------------------------------------
dnorm(140,c(100,150,125,130),100)

## ------------------------------------------------------------------------
priors<-rep(.25,4)# prior weights
observed<-140
predicted<-c(100,150,125,135)
sd<-10
like<-dnorm(observed,predicted,sd)  # p(x|Hi)
post<-like*priors/sum(like*priors) # P(Hi): priors, p(x): sum(like*post)
summ<-cbind(priors,predicted, like,post)
models<-data.frame(priors=priors, pred=predicted,like=like,post=post)

## ------------------------------------------------------------------------
models

## ------------------------------------------------------------------------
models$sd<- 10

## ------------------------------------------------------------------------
sd<-25
like<-dnorm(observed,predicted,sd)
post<-like*priors/sum(like*priors)
summ<-cbind(priors,predicted, like,post)
app<-data.frame(priors=priors, pred=predicted,like=like,post=post,sd=sd)
models<-rbind(models,app)

## ------------------------------------------------------------------------
sd<-100
like<-dnorm(observed,predicted,sd)
post<-like*priors/sum(like*priors)
summ<-cbind(priors,predicted, like,post)
app<-data.frame(priors=priors, pred=predicted,like=like,post=post,sd=sd)
models<-rbind(models,app)

## ------------------------------------------------------------------------
models

## ------------------------------------------------------------------------
weights<- matrix(models$post,
    ncol=4,nrow=3,
    dimnames=list(c("10","25","100"),c("H1","H2","H3","H4")),
    byrow=TRUE)

## ------------------------------------------------------------------------
barplot(weights,beside=TRUE,ylim=c(0,0.5),
    las=1,
    xlab="Hypothesis",
    ylab="Posterior probability",
    col=c("grey10","grey40","grey80"))
legend("topleft",
    legend=c("SD=10","SD=25","SD=100"),
    fill=c("grey10","grey40","grey80"))
abline(h=0.25, col="red",lty=2)
text(x=0.75,y=0.265,
    labels="Prior probability",
    pos=4)
box()

## ------------------------------------------------------------------------
models<-data.frame()
observed<- 140
sd<-100
like<-dnorm(observed,predicted,sd)
priors<-rep(.25,4)# prior weights
post<-like*priors/sum(like*priors)
summ<-cbind(priors,predicted, like,post)
app<-data.frame(hypothesis=c(1:4),priors=priors, 
    pred=predicted,like=like,post=post,year=1)
models<-rbind(models,app)

## ------------------------------------------------------------------------
models

## ------------------------------------------------------------------------
observed<- 139
sd<-100
like<-dnorm(observed,predicted,sd)
priors<-post # make posterior for year 1 as priors for year 2
post<-like*priors/sum(like*priors)
summ<-cbind(priors,predicted, like,post)
app<-data.frame(hypothesis=c(1:4),priors=priors, 
    pred=predicted,like=like,post=post,year=2)
models<-rbind(models,app)

## ------------------------------------------------------------------------
observed<- 143
sd<-100
like<-dnorm(observed,predicted,sd)
priors<-post # make posterior for year 2 as priors for year 3
post<-like*priors/sum(like*priors)
summ<-cbind(priors,predicted, like,post)
app<-data.frame(hypothesis=c(1:4),priors=priors, 
    pred=predicted,like=like,post=post,year=3)
models<-rbind(models,app)

## ------------------------------------------------------------------------
observed<- 125
sd<-100
like<-dnorm(observed,predicted,sd)
priors<-post # make posterior for year 3 as priors for year 4
post<-like*priors/sum(like*priors)
summ<-cbind(priors,predicted, like,post)
app<-data.frame(hypothesis=c(1:4),priors=priors, 
    pred=predicted,like=like,post=post,year=4)
models<-rbind(models,app)

## ------------------------------------------------------------------------
observed<- 138
sd<-100
like<-dnorm(observed,predicted,sd)
priors<-post # make posterior for year 4 as priors for year 5
post<-like*priors/sum(like*priors)
summ<-cbind(priors,predicted, like,post)
app<-data.frame(hypothesis=c(1:4),priors=priors, 
    pred=predicted,like=like,post=post,year=5)
models<-rbind(models,app)

## ------------------------------------------------------------------------
models

## ------------------------------------------------------------------------
plot(post~year,
    data=models,
    xlab="Year",
    ylab="Posterior probability",
    type='n')
points(post~year,
    data=models,
    subset=hypothesis==1,
    type='b',
    col="black")
 points(post~year,
    data=models,
    subset=hypothesis==2,
    type='b',
    col="red")   
points(post~year,
    data=models,
    subset=hypothesis==3,
    type='b',
    col="green")    
points(post~year,
    data=models,
    subset=hypothesis==4,
    type='b',
    col="blue")   
legend("bottomleft",
    legend=c("H1","H2","H3","H4"),
    lty=1,
    pch=1,
    col=c("black","red","green","blue"))

## ------------------------------------------------------------------------
est_flow_decreasePower <- 30
est_flow_controlFlatheads <- 15

## ------------------------------------------------------------------------
est_predation_decreasePower <- 15
est_predation_controlFlatheads <- 25

## ------------------------------------------------------------------------
Flow <- 0.5 # PRIOR PROBABILITY FOR FLOW MODEL
Predation <- 0.5 # PRIOR PROBABILITY FOR PREDATION MODEL

## ------------------------------------------------------------------------
decreasePowerGeneration<- Flow*est_flow_decreasePower + 
    Predation*est_predation_decreasePower

## ------------------------------------------------------------------------
controlFlatheads <- Flow*est_flow_controlFlatheads + 
    Predation*est_predation_controlFlatheads

## ------------------------------------------------------------------------
decreasePowerGeneration
controlFlatheads

## ------------------------------------------------------------------------
decision<-"Decrease Power Generation"

## ------------------------------------------------------------------------
obs<- 21

## ------------------------------------------------------------------------
## conditional Likelihoods for Decrease power generation action
flow_like<-dpois(obs,est_flow_decreasePower)
predation_like<-dpois(obs,est_predation_decreasePower)

## ------------------------------------------------------------------------
flow_like
predation_like

## ------------------------------------------------------------------------
flow_post<-flow_like*Flow/(flow_like*Flow + predation_like*Predation)
predation_post<- predation_like*Predation/(flow_like*Flow + predation_like*Predation)
flow_wghts<-flow_post
predation_weights<- predation_post

## ------------------------------------------------------------------------
flow_post
predation_post

## ------------------------------------------------------------------------
Flow <- flow_post
Predation <- predation_post

## ------------------------------------------------------------------------
decreasePowerGeneration<- Flow*est_flow_decreasePower + 
    Predation*est_predation_decreasePower

## ------------------------------------------------------------------------
controlFlatheads <- Flow*est_flow_controlFlatheads + 
    Predation*est_predation_controlFlatheads

## ------------------------------------------------------------------------
decreasePowerGeneration
controlFlatheads

## ------------------------------------------------------------------------
decision<-c(decision , "Control Flathead Catfish")

## ------------------------------------------------------------------------
obs<- 18

## ------------------------------------------------------------------------
flow_like<-dpois(obs,est_flow_controlFlatheads)
predation_like<-dpois(obs,est_predation_controlFlatheads)

## ------------------------------------------------------------------------
flow_post<-flow_like*Flow/(flow_like*Flow + predation_like*Predation)
predation_post<- predation_like*Predation/(flow_like*Flow + predation_like*Predation)
flow_wghts<-c(flow_wghts,flow_post) # keep track of to look at later
predation_weights<- c(predation_weights,predation_post) # keep track of to look at later

## ------------------------------------------------------------------------
# New model weights 
flow_post
predation_post

## ------------------------------------------------------------------------
Flow <- flow_post
Predation <- predation_post

## ------------------------------------------------------------------------
decreasePowerGeneration<- Flow*est_flow_decreasePower + 
    Predation*est_predation_decreasePower
controlFlatheads <- Flow*est_flow_controlFlatheads + 
    Predation*est_predation_controlFlatheads
decreasePowerGeneration
controlFlatheads
decision<- c(decision, "Decrease Power Generation")

## ------------------------------------------------------------------------
obs<- 22
flow_like<-dpois(obs,est_flow_decreasePower)
predation_like<-dpois(obs,est_predation_decreasePower)
flow_post<-flow_like*Flow/(flow_like*Flow + predation_like*Predation)
predation_post<- predation_like*Predation/(flow_like*Flow + predation_like*Predation)

## ------------------------------------------------------------------------
flow_wghts<-c(flow_wghts,flow_post)
predation_weights<- c(predation_weights,predation_post)

## ------------------------------------------------------------------------
Flow <- flow_post
Predation <- predation_post

## ------------------------------------------------------------------------
decreasePowerGeneration<- Flow*est_flow_decreasePower + 
    Predation*est_predation_decreasePower
controlFlatheads <- Flow*est_flow_controlFlatheads + 
    Predation*est_predation_controlFlatheads
decreasePowerGeneration
controlFlatheads
decision<- c(decision, "Decrease Power Generation")

## ------------------------------------------------------------------------
obs<- 17
## conditional Likelihoods for Decrease.power.generation action
flow_like<-dpois(obs,est_flow_decreasePower)
predation_like<-dpois(obs,est_predation_decreasePower)
flow_post<-flow_like*Flow/(flow_like*Flow + predation_like*Predation)
predation_post<- predation_like*Predation/(flow_like*Flow + predation_like*Predation)

## ------------------------------------------------------------------------
flow_wghts<-c(flow_wghts,flow_post)
predation_weights<- c(predation_weights,predation_post)

## ------------------------------------------------------------------------
Flow <- flow_post
Predation <- predation_post

## ------------------------------------------------------------------------
decreasePowerGeneration<- Flow*est_flow_decreasePower + 
    Predation*est_predation_decreasePower
controlFlatheads <- Flow*est_flow_controlFlatheads + 
    Predation*est_predation_controlFlatheads
decreasePowerGeneration
controlFlatheads
decision<- c(decision, "Control Flathead Catfish")

## ------------------------------------------------------------------------
obs<- 15
## conditional Likelihoods
flow_like<-dpois(obs,est_flow_controlFlatheads)
predation_like<-dpois(obs,est_predation_controlFlatheads)
flow_post<-flow_like*Flow/(flow_like*Flow + predation_like*Predation)
predation_post<- predation_like*Predation/(flow_like*Flow + predation_like*Predation)

## ------------------------------------------------------------------------
flow_wghts<-c(flow_wghts,flow_post)
predation_weights<- c(predation_weights,predation_post)

## ------------------------------------------------------------------------
Flow <- flow_post
Predation <- predation_post

## ------------------------------------------------------------------------
decreasePowerGeneration<- Flow*est_flow_decreasePower + 
    Predation*est_predation_decreasePower
controlFlatheads <- Flow*est_flow_controlFlatheads + 
    Predation*est_predation_controlFlatheads
decreasePowerGeneration
controlFlatheads
decision<- c(decision, "Control Flathead Catfish")

## ------------------------------------------------------------------------
obs<- 13
flow_like<-dpois(obs,est_flow_controlFlatheads)
predation_like<-dpois(obs,est_predation_controlFlatheads)
flow_post<-flow_like*Flow/(flow_like*Flow + predation_like*Predation)
predation_post<- predation_like*Predation/(flow_like*Flow + predation_like*Predation)

## ------------------------------------------------------------------------
flow_wghts<-c(flow_wghts,flow_post)
predation_weights<- c(predation_weights,predation_post)

## ------------------------------------------------------------------------
decision
flow_wghts
predation_weights

## ------------------------------------------------------------------------
plot(flow_wghts,
    xlab="Year",
    ylab="Probability",
    ylim=c(0,1),
    type='b',
    col='blue',
    las=1)
points(predation_weights,
    type='b',
    col='green')
legend("topleft", 
    legend=c("Flow variability","Predation"),
    lty=1,
    col=c("blue","green"),
    lwd=2)

## ----echo=FALSE, out.width="100%"----------------------------------------
library(knitr)
include_graphics("media/class-22/loop-learning.png")

