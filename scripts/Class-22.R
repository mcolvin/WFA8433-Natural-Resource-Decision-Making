## ----unnamed-chunk-1---- ##
deck<- expand.grid(
    suit = c("Diamond", "Club", "Heart", "Spade"),
    card = c("Ace", "Deuce", "Three", "Four","Five", 
             "Six", "Seven", "Eight", "Nine", "Ten", 
             "Jack", "Queen", "King"))
deck$id<-1:nrow(deck) # for sampling later
ncards<-nrow(deck)
prX<- nrow(deck[deck$suit=="Heart",])/ncards #p(x=Heart)
prY<-  nrow(deck[deck$suit=="Spade",])/ncards #p(x=Spade) 
 
 
 
## ----unnamed-chunk-2---- ##
prX
prY
 
 
 
## ----unnamed-chunk-3---- ##
reps<-50000
indx<-sample(nrow(deck),reps,replace=TRUE) # index for card selected
out<-data.frame(firstCardSuit=deck[indx,]$suit)
prop.table(table(out)) # all close to 0.25
 
 
 
## ----unnamed-chunk-4---- ##
out$secondCardSuite<-NA
# SIMULATE THE PROCESS
for(i in 1:reps)
    {
    # SAMPLE ANOTHER CARD AND GET THE SUITE
    id<- sample(deck$id[-indx[i]],1)
    out$secondCardSuit[i]<- as.character(deck$suit[id])
    }
 
 
 
## ----unnamed-chunk-5---- ##
out$tmp<-1
outcomes<-aggregate(tmp~firstCardSuit+secondCardSuit,out,FUN=sum)
outcomes$p<- outcomes$tmp/reps
 
 
 
## ----unnamed-chunk-6---- ##
outcomes
 
 
 
## ----unnamed-chunk-7---- ##
nrow(out[out$firstCardSuit=="Heart" & out$secondCardSuit=="Spade",])/reps
nrow(out[out$firstCardSuit=="Spade" & out$secondCardSuit=="Heart",])/reps
 
 
 
## ----unnamed-chunk-8---- ##
dnorm(140,125,10) #p(x|H_i), 3rd model
 
 
 
## ----unnamed-chunk-9---- ##
dnorm(140,c(100,150,125,130),100)
 
 
 
## ----unnamed-chunk-10---- ##
priors<-rep(.25,4)# prior weights
observed<-140
predicted<-c(100,150,125,135)
sd<-10
like<-dnorm(observed,predicted,sd)  # p(x|Hi)
post<-like*priors/sum(like*priors) # P(Hi): priors, p(x): sum(like*post)
summ<-cbind(priors,predicted, like,post)
models<-data.frame(priors=priors, pred=predicted,like=like,post=post)
 
 
 
## ----unnamed-chunk-11---- ##
models
 
 
 
## ----unnamed-chunk-12---- ##
models$sd<- 10
 
 
 
## ----unnamed-chunk-13---- ##
sd<-25
like<-dnorm(observed,predicted,sd)
post<-like*priors/sum(like*priors)
summ<-cbind(priors,predicted, like,post)
app<-data.frame(priors=priors, pred=predicted,like=like,post=post,sd=sd)
models<-rbind(models,app)
 
 
 
## ----unnamed-chunk-14---- ##
sd<-100
like<-dnorm(observed,predicted,sd)
post<-like*priors/sum(like*priors)
summ<-cbind(priors,predicted, like,post)
app<-data.frame(priors=priors, pred=predicted,like=like,post=post,sd=sd)
models<-rbind(models,app)
 
 
 
## ----unnamed-chunk-15---- ##
models
 
 
 
## ----unnamed-chunk-16---- ##
weights<- matrix(models$post,
    ncol=4,nrow=3,
    dimnames=list(c("10","25","100"),c("H1","H2","H3","H4")),
    byrow=TRUE)
 
 
 
## ----unnamed-chunk-17---- ##
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
 
 
 
## ----unnamed-chunk-18---- ##
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
 
 
 
## ----unnamed-chunk-19---- ##
models
 
 
 
## ----unnamed-chunk-20---- ##
observed<- 139
sd<-100
like<-dnorm(observed,predicted,sd)
priors<-post # make posterior for year 1 as priors for year 2
post<-like*priors/sum(like*priors)
summ<-cbind(priors,predicted, like,post)
app<-data.frame(hypothesis=c(1:4),priors=priors, 
    pred=predicted,like=like,post=post,year=2)
models<-rbind(models,app)
 
 
 
## ----unnamed-chunk-21---- ##
observed<- 143
sd<-100
like<-dnorm(observed,predicted,sd)
priors<-post # make posterior for year 2 as priors for year 3
post<-like*priors/sum(like*priors)
summ<-cbind(priors,predicted, like,post)
app<-data.frame(hypothesis=c(1:4),priors=priors, 
    pred=predicted,like=like,post=post,year=3)
models<-rbind(models,app)
 
 
 
## ----unnamed-chunk-22---- ##
observed<- 125
sd<-100
like<-dnorm(observed,predicted,sd)
priors<-post # make posterior for year 3 as priors for year 4
post<-like*priors/sum(like*priors)
summ<-cbind(priors,predicted, like,post)
app<-data.frame(hypothesis=c(1:4),priors=priors, 
    pred=predicted,like=like,post=post,year=4)
models<-rbind(models,app)
 
 
 
## ----unnamed-chunk-23---- ##
observed<- 138
sd<-100
like<-dnorm(observed,predicted,sd)
priors<-post # make posterior for year 4 as priors for year 5
post<-like*priors/sum(like*priors)
summ<-cbind(priors,predicted, like,post)
app<-data.frame(hypothesis=c(1:4),priors=priors, 
    pred=predicted,like=like,post=post,year=5)
models<-rbind(models,app)
 
 
 
## ----unnamed-chunk-24---- ##
models
 
 
 
## ----unnamed-chunk-25---- ##
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
 
 
 
## ----unnamed-chunk-26---- ##
est_flow_decreasePower <- 30
est_flow_controlFlatheads <- 15
 
 
 
## ----unnamed-chunk-27---- ##
est_predation_decreasePower <- 15
est_predation_controlFlatheads <- 25
 
 
 
## ----unnamed-chunk-28---- ##
Flow <- 0.5 # PRIOR PROBABILITY FOR FLOW MODEL
Predation <- 0.5 # PRIOR PROBABILITY FOR PREDATION MODEL
 
 
 
## ----unnamed-chunk-29---- ##
decreasePowerGeneration<- Flow*est_flow_decreasePower + 
    Predation*est_predation_decreasePower
 
 
 
## ----unnamed-chunk-30---- ##
controlFlatheads <- Flow*est_flow_controlFlatheads + 
    Predation*est_predation_controlFlatheads
 
 
 
## ----unnamed-chunk-31---- ##
decreasePowerGeneration
controlFlatheads
 
 
 
## ----unnamed-chunk-32---- ##
decision<-"Decrease Power Generation"
 
 
 
## ----unnamed-chunk-33---- ##
obs<- 21
 
 
 
## ----unnamed-chunk-34---- ##
## conditional Likelihoods for Decrease power generation action
flow_like<-dpois(obs,est_flow_decreasePower)
predation_like<-dpois(obs,est_predation_decreasePower)
 
 
 
## ----unnamed-chunk-35---- ##
flow_like
predation_like
 
 
 
## ----unnamed-chunk-36---- ##
flow_post<-flow_like*Flow/(flow_like*Flow + predation_like*Predation)
predation_post<- predation_like*Predation/(flow_like*Flow + predation_like*Predation)
flow_wghts<-flow_post
predation_weights<- predation_post
 
 
 
## ----unnamed-chunk-37---- ##
flow_post
predation_post
 
 
 
## ----unnamed-chunk-38---- ##
Flow <- flow_post
Predation <- predation_post
 
 
 
## ----unnamed-chunk-39---- ##
decreasePowerGeneration<- Flow*est_flow_decreasePower + 
    Predation*est_predation_decreasePower
 
 
 
## ----unnamed-chunk-40---- ##
controlFlatheads <- Flow*est_flow_controlFlatheads + 
    Predation*est_predation_controlFlatheads
 
 
 
## ----unnamed-chunk-41---- ##
decreasePowerGeneration
controlFlatheads
 
 
 
## ----unnamed-chunk-42---- ##
decision<-c(decision , "Control Flathead Catfish")
 
 
 
## ----unnamed-chunk-43---- ##
obs<- 18
 
 
 
## ----unnamed-chunk-44---- ##
flow_like<-dpois(obs,est_flow_controlFlatheads)
predation_like<-dpois(obs,est_predation_controlFlatheads)
 
 
 
## ----unnamed-chunk-45---- ##
flow_post<-flow_like*Flow/(flow_like*Flow + predation_like*Predation)
predation_post<- predation_like*Predation/(flow_like*Flow + predation_like*Predation)
flow_wghts<-c(flow_wghts,flow_post) # keep track of to look at later
predation_weights<- c(predation_weights,predation_post) # keep track of to look at later
 
 
 
## ----unnamed-chunk-46---- ##
# New model weights 
flow_post
predation_post
 
 
 
## ----unnamed-chunk-47---- ##
Flow <- flow_post
Predation <- predation_post
 
 
 
## ----unnamed-chunk-48---- ##
decreasePowerGeneration<- Flow*est_flow_decreasePower + 
    Predation*est_predation_decreasePower
controlFlatheads <- Flow*est_flow_controlFlatheads + 
    Predation*est_predation_controlFlatheads
decreasePowerGeneration
controlFlatheads
decision<- c(decision, "Decrease Power Generation")
 
 
 
## ----unnamed-chunk-49---- ##
obs<- 22
flow_like<-dpois(obs,est_flow_decreasePower)
predation_like<-dpois(obs,est_predation_decreasePower)
flow_post<-flow_like*Flow/(flow_like*Flow + predation_like*Predation)
predation_post<- predation_like*Predation/(flow_like*Flow + predation_like*Predation)
 
 
 
## ----unnamed-chunk-50---- ##
flow_wghts<-c(flow_wghts,flow_post)
predation_weights<- c(predation_weights,predation_post)
 
 
 
## ----unnamed-chunk-51---- ##
Flow <- flow_post
Predation <- predation_post
 
 
 
## ----unnamed-chunk-52---- ##
decreasePowerGeneration<- Flow*est_flow_decreasePower + 
    Predation*est_predation_decreasePower
controlFlatheads <- Flow*est_flow_controlFlatheads + 
    Predation*est_predation_controlFlatheads
decreasePowerGeneration
controlFlatheads
decision<- c(decision, "Decrease Power Generation")
 
 
 
## ----unnamed-chunk-53---- ##
obs<- 17
## conditional Likelihoods for Decrease.power.generation action
flow_like<-dpois(obs,est_flow_decreasePower)
predation_like<-dpois(obs,est_predation_decreasePower)
flow_post<-flow_like*Flow/(flow_like*Flow + predation_like*Predation)
predation_post<- predation_like*Predation/(flow_like*Flow + predation_like*Predation)
 
 
 
## ----unnamed-chunk-54---- ##
flow_wghts<-c(flow_wghts,flow_post)
predation_weights<- c(predation_weights,predation_post)
 
 
 
## ----unnamed-chunk-55---- ##
Flow <- flow_post
Predation <- predation_post
 
 
 
## ----unnamed-chunk-56---- ##
decreasePowerGeneration<- Flow*est_flow_decreasePower + 
    Predation*est_predation_decreasePower
controlFlatheads <- Flow*est_flow_controlFlatheads + 
    Predation*est_predation_controlFlatheads
decreasePowerGeneration
controlFlatheads
decision<- c(decision, "Control Flathead Catfish")
 
 
 
## ----unnamed-chunk-57---- ##
obs<- 15
## conditional Likelihoods
flow_like<-dpois(obs,est_flow_controlFlatheads)
predation_like<-dpois(obs,est_predation_controlFlatheads)
flow_post<-flow_like*Flow/(flow_like*Flow + predation_like*Predation)
predation_post<- predation_like*Predation/(flow_like*Flow + predation_like*Predation)
 
 
 
## ----unnamed-chunk-58---- ##
flow_wghts<-c(flow_wghts,flow_post)
predation_weights<- c(predation_weights,predation_post)
 
 
 
## ----unnamed-chunk-59---- ##
Flow <- flow_post
Predation <- predation_post
 
 
 
## ----unnamed-chunk-60---- ##
decreasePowerGeneration<- Flow*est_flow_decreasePower + 
    Predation*est_predation_decreasePower
controlFlatheads <- Flow*est_flow_controlFlatheads + 
    Predation*est_predation_controlFlatheads
decreasePowerGeneration
controlFlatheads
decision<- c(decision, "Control Flathead Catfish")
 
 
 
## ----unnamed-chunk-61---- ##
obs<- 13
flow_like<-dpois(obs,est_flow_controlFlatheads)
predation_like<-dpois(obs,est_predation_controlFlatheads)
flow_post<-flow_like*Flow/(flow_like*Flow + predation_like*Predation)
predation_post<- predation_like*Predation/(flow_like*Flow + predation_like*Predation)
 
 
 
## ----unnamed-chunk-62---- ##
flow_wghts<-c(flow_wghts,flow_post)
predation_weights<- c(predation_weights,predation_post)
 
 
 
## ----unnamed-chunk-63---- ##
decision
flow_wghts
predation_weights
 
 
 
## ----unnamed-chunk-64---- ##
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
 
 
 
