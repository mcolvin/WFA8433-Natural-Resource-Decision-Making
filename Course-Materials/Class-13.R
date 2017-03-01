## ----echo=FALSE, out.width="95%"-----------------------------------------
include_graphics("media/banner-05.jpg")
rm(list=objects())

## ------------------------------------------------------------------------
S<-seq(from=0.1,to=1,by=0.05) # SET SURVIVAL
N<- c(10,50,100,500,1000)

## ------------------------------------------------------------------------
survivors_pois<-rpois(50000,S*N[1])
survivors_bin<-rbinom(50000,N[1],S)

## ------------------------------------------------------------------------
hist(survivors_pois)
hist(survivors_bin)

## ------------------------------------------------------------------------
par(mfrow=c(2,1))
hist(survivors_pois,
    xlim=c(0,10))
hist(survivors_bin,
    xlim=c(0,10))

## ------------------------------------------------------------------------
sims<- expand.grid(N=N,S=S,reps=c(1:500))
sims$surv_pois<-rpois(n=nrow(sims),lambda=sims$S*sims$N)
sims$surv_bin<- rbinom(n=nrow(sims),size=sims$N,prob=sims$S)

## ------------------------------------------------------------------------
boxplot(surv_pois~N,data=sims)
boxplot(surv_bin~N,data=sims,add=TRUE,col='grey')

## ------------------------------------------------------------------------
boxplot(surv_pois~S,data=sims,
    at=1:length(S)-0.25)
boxplot(surv_bin~S,data=sims,add=TRUE,col='grey',
    at=1:length(S)+0.25)

## ------------------------------------------------------------------------
boxplot(surv_pois~S,data=sims,
    at=1:1:length(S)-0.25,
    subset=N==10,
    boxwex=0.25, # make boxplot narrower
    xaxt='n') # suppress x-axis label plotting
boxplot(surv_bin~S,data=sims,add=TRUE,col='grey',
    at=1:1:length(S)+0.25,
    subset=N==10,
    boxwex=0.25,
    xaxt='n')
axis(side=1,at=1:length(S),labels=S)
legend("topleft",legend=c("Poisson","Binomial"),
    fill=c("white","grey"))

## ------------------------------------------------------------------------
boxplot(surv_pois~S,data=sims,
    at=1:1:length(S)-0.25,
    subset=N==1000,
    boxwex=0.25, # make boxplot narrower
    xaxt='n') # suppress x-axis label plotting
boxplot(surv_bin~S,data=sims,add=TRUE,col='grey',
    at=1:1:length(S)+0.25,
    subset=N==1000,
    boxwex=0.25,
    xaxt='n')
axis(side=1,at=1:length(S),labels=S)

## ------------------------------------------------------------------------
sims$lambda<- sims$N*sims$S
vars<- aggregate(surv_pois~lambda,sims,var)
plot(surv_pois~lambda,data=vars,   
    xlab="Lambda",
    ylab="Variance")
abline(0,1)

## ----echo=FALSE----------------------------------------------------------
set.seed(8433)
betas<- c(-4.5,2.3,2.6,1.75)
n=87
fishCounts<- data.frame(
    width= round(runif(n,0.5,1.3),1),
    habitat=sample(c("forest","urban","ag"),n,replace=TRUE))
fishCounts$streamLength<- round(fishCounts$width*50,0)
mm<- model.matrix(as.formula("~width+habitat"),fishCounts) 
lambda<- exp(mm %*% betas+log(fishCounts$streamLength))
fishCounts$counts<-  rpois(n,lambda) 
write.csv(fishCounts,"fishCounts.csv",row.names=FALSE)

## AN OVERDISPERSED POISSON
betas<- c(4.5,-1,-0.5,
    2,1,0.5,0.0015)
n=267
dat_od<- data.frame(
    treatment=sample(c("no burn","partial burn","full burn"),n,replace=TRUE),
    habitat=sample(c("field","riparian","mixed hardwood","pine"),
        n,replace=TRUE),
    elevation= round(runif(n,5,300),0))
mm<- model.matrix(as.formula("~treatment+habitat+elevation"),dat_od) 
lambda<- exp(mm %*% betas)

# http://stats.stackexchange.com/questions/35/modelling-a-poisson-distribution-with-overdispersion
dispersion <- 4
dat_od$counts<-  rnbinom(n, size=(lambda/(dispersion-1)), 
    mu=lambda)#rpois(n,lambda) 
#plot(counts~elevation,dat_od)
stemCounts<- dat_od
write.csv(stemCounts,"stemCounts.csv",row.names=FALSE)


## ------------------------------------------------------------------------
fishCounts<- read.csv("fishCounts.csv")
fishCounts

## ------------------------------------------------------------------------
plot(counts~width,
    data=fishCounts,
    xlab="Width (m)",
    ylab="Catch",
    type='n',
    las=1)
points(counts~width, data=fishCounts,
    subset=habitat=="ag",col="red",pch=1)
points(counts~width, data=fishCounts,
    subset=habitat=="forest",col="blue",pch=2)
points(counts~width, data=fishCounts,
    subset=habitat=="urban",col="black",pch=3)
legend("topleft",legend=c("Agriculture","Forested","Urban"),
    col=c("red","blue","black"),pch=c(1,2,3))

## ------------------------------------------------------------------------
fit<- glm(counts~width+habitat,
    data=fishCounts,
    offset=log(fishCounts$streamLength),# recall it is log 
    family="poisson")

## ------------------------------------------------------------------------
summary(fit)

## ------------------------------------------------------------------------
betas<- coef(fit)
confint(fit)

## ------------------------------------------------------------------------
fishCounts$pred<- fitted(fit) 
fishCounts$res<- resid(fit) 
plot(counts~pred,data=fishCounts)
abline(0,1) 

## ------------------------------------------------------------------------
plot(res~pred,data=fishCounts)
abline(h=0) 

## ------------------------------------------------------------------------
# do 4 streams with varying depths and land cover

preddat<-data.frame(
    width=c(1.3,1.05,0.65,0.7,0.65,0.95),
    habitat=c("forest","forest","urban","urban","ag","ag"),
    streamLength=c(123,324,75,84,68,98))

# preddat$lambda<- predict(fit,newdata=preddat,
  #   type="response")  # does not work 

## ------------------------------------------------------------------------
preddat$lambda<- NA # prep vector to fill

preddat[preddat$habitat=="ag",]$lambda<- exp(betas[1]+
    preddat[preddat$habitat=="ag",]$width*betas[2]+
    log(preddat[preddat$habitat=="forest",]$streamLength))
preddat[preddat$habitat=="forest",]$lambda<- exp(betas[1]+
    preddat[preddat$habitat=="forest",]$width*betas[2]+
    betas[3]+
    log(preddat[preddat$habitat=="forest",]$streamLength))   
preddat[preddat$habitat=="urban",]$lambda<- exp(betas[1]+
    preddat[preddat$habitat=="urban",]$width*betas[2]+
    betas[4]+
    log(preddat[preddat$habitat=="urban",]$streamLength))   
preddat   

## ------------------------------------------------------------------------
xx<-rpois(10000,10)
xx<- sort(xx)
xx[250]
xx[9750]

## ------------------------------------------------------------------------
vals<-quantile(x=xx,probs=c(0.025,0.975))
vals

## ------------------------------------------------------------------------
vals2<- qpois(c(0.025,0.975),lambda=10)
vals2

## ------------------------------------------------------------------------
preddat$uci<-qpois(0.975, preddat$lambda)
preddat$lci<-qpois(0.025, preddat$lambda)

## ------------------------------------------------------------------------
# MAKE A MATRIX TO STORE OUR PRECIOUS PROBABILITIES
outcomes<- matrix(0,nrow=1001,ncol=nrow(preddat))

## ------------------------------------------------------------------------
dpois(x=3,lambda=10) # should be pretty low
dpois(x=10,lambda=10) # about mid of the road
dpois(x=17,lambda=10) # close to 1

## ------------------------------------------------------------------------
for(i in 1:nrow(preddat))# loop over each row of predat
    {
    outcomes[,i]<-dpois(c(0:1000),lambda=preddat$lambda[i])
    }

## ------------------------------------------------------------------------
colSums(outcomes)

## ------------------------------------------------------------------------
matplot(y=outcomes,type='l',lwd=3)

## ------------------------------------------------------------------------
preddat<-expand.grid( # MAKE DATASET
    width=c(1.3,1.05,0.65,0.7,0.65,0.95),
    habitat=c("forest","urban","ag"),
    streamLength=c(25:65))

preddat$lambda<- NA # prep vector to fill

# PREDICT LAMBDA
preddat[preddat$habitat=="ag",]$lambda<- exp(betas[1]+
    preddat[preddat$habitat=="ag",]$width*betas[2]+
    log(preddat[preddat$habitat=="forest",]$streamLength))
preddat[preddat$habitat=="forest",]$lambda<- exp(betas[1]+
    preddat[preddat$habitat=="forest",]$width*betas[2]+
    betas[3]+
    log(preddat[preddat$habitat=="forest",]$streamLength))   
preddat[preddat$habitat=="urban",]$lambda<- exp(betas[1]+
    preddat[preddat$habitat=="urban",]$width*betas[2]+
    betas[4]+
    log(preddat[preddat$habitat=="urban",]$streamLength))   

## ------------------------------------------------------------------------
preddat$uci<-qpois(0.975, preddat$lambda)
preddat$lci<-qpois(0.025, preddat$lambda)

## ------------------------------------------------------------------------
plot(uci~width,data=preddat,
    xlab="Stream width",
    ylab="Fish count",
    subset=streamLength==25,
    type='n')
points(lambda~width,data=preddat,
    subset=streamLength==25 & habitat=="forest", 
    lty=1,type='l')
 

## ------------------------------------------------------------------------
preddat<- preddat[order(preddat$streamLength,preddat$width),]

## ------------------------------------------------------------------------
plot(uci~width,data=preddat,
    xlab="Stream width",
    ylab="Fish count",
    subset=streamLength==25,
    type='n',
    las=1)
points(lambda~width,data=preddat,
    subset=streamLength==25 & habitat=="forest", 
    lty=1,type='l')
points(lci~width,data=preddat,
    subset=streamLength==25 & habitat=="forest", 
    lty=2,type='l')
points(uci~width,data=preddat,
    subset=streamLength==25 & habitat=="forest", 
    lty=2,type='l')

## ------------------------------------------------------------------------
stemCounts<- read.csv("stemCounts.csv")
head(stemCounts)

## ------------------------------------------------------------------------
plot(counts~elevation,data=stemCounts,
    xlab="Elevation",
    ylab="Stem counts",
    las=1,
    type='n')
points(counts~elevation,data=stemCounts,
    subset=treatment=="no burn" & habitat=="field",
    pch=1,col="red")
points(counts~elevation,data=stemCounts,
    subset=treatment=="no burn" & habitat=="riparian",
    pch=2,col="red")   
points(counts~elevation,data=stemCounts,
    subset=treatment=="no burn" & habitat=="mixed hardwood",
    pch=3,col="red")     
points(counts~elevation,data=stemCounts,
    subset=treatment=="no burn" & habitat=="pine",
    pch=4,col="red")   
    
points(counts~elevation,data=stemCounts,
    subset=treatment=="partial burn" & habitat=="field",
    pch=1,col="blue")
points(counts~elevation,data=stemCounts,
    subset=treatment=="partial burn" & habitat=="riparian",
    pch=2,col="blue")   
points(counts~elevation,data=stemCounts,
    subset=treatment=="partial burn" & habitat=="mixed hardwood",
    pch=3,col="blue")     
points(counts~elevation,data=stemCounts,
    subset=treatment=="partial burn" & habitat=="pine",
    pch=4,col="blue") 

points(counts~elevation,data=stemCounts,
    subset=treatment=="full burn" & habitat=="field",
    pch=1,col="green")
points(counts~elevation,data=stemCounts,
    subset=treatment=="full burn" & habitat=="riparian",
    pch=2,col="green")   
points(counts~elevation,data=stemCounts,
    subset=treatment=="full burn" & habitat=="mixed hardwood",
    pch=3,col="green")     
points(counts~elevation,data=stemCounts,
    subset=treatment=="full burn" & habitat=="pine",
    pch=4,col="green")  
legend("topleft",legend=c("No burn", "Partial burn", "Full burn",
    "Field","Riparian","Mixed hardwood", "Pines"),
    pch=c(1,1,1,1,1,2,3,4),col=c("red","blue","green","black","black",
        "black","black"),ncol=2)

## ------------------------------------------------------------------------
fit<- glm(counts~treatment+habitat+elevation,
    data=stemCounts,
    family="poisson")
summary(fit)

## ------------------------------------------------------------------------
betas<- coef(fit)
confint(fit)

## ------------------------------------------------------------------------
stemCounts$fitted<-fitted(fit)
plot(counts~fitted, data= stemCounts,
    xlab="Predicted values",
    ylab="Counts",
    las=1)
abline(0,1)

## ------------------------------------------------------------------------
preddat<- expand.grid(
    treatment=levels(stemCounts$treatment),
    habitat=levels(stemCounts$habitat),
    elevation=c(min(stemCounts$elevation):max(stemCounts$elevation)))
 

## ------------------------------------------------------------------------
preddat$lambda<- predict(fit, newdata=preddat,
    type='response')

## ------------------------------------------------------------------------
preddat$lci<-qpois(0.025,preddat$lambda)
preddat$uci<-qpois(0.975,preddat$lambda)

## ------------------------------------------------------------------------
plot(counts~elevation,data=stemCounts,
    xlab="Elevation",
    ylab="Stem counts",
    las=1,
    type='n',subset=treatment=="no burn")
points(counts~elevation,data=stemCounts,
    subset=treatment=="no burn" & habitat=="field",
    pch=1,col="black")
## PLOT 95% PREDICTION INTERVALS    
points(lci~elevation,data=preddat,
    subset=treatment=="no burn" & habitat=="field",
    type='l',col="black",lty=2)
points(uci~elevation,data=preddat,
    subset=treatment=="no burn" & habitat=="field",
    type='l',col="black",lty=2)     
 
points(counts~elevation,data=stemCounts,
    subset=treatment=="no burn" & habitat=="riparian",
    pch=2,col="green")  
points(lci~elevation,data=preddat,
    subset=treatment=="no burn" & habitat=="riparian",
    type='l',col="green",lty=2)
points(uci~elevation,data=preddat,
    subset=treatment=="no burn" & habitat=="riparian",
    type='l',col="green",lty=2) 
    
points(counts~elevation,data=stemCounts,
    subset=treatment=="no burn" & habitat=="mixed hardwood",
    pch=3,col="red")    
points(lci~elevation,data=preddat,
    subset=treatment=="no burn" & habitat=="mixed hardwood",
    type='l',col="red",lty=2)
points(uci~elevation,data=preddat,
    subset=treatment=="no burn" & habitat=="mixed hardwood",
    type='l',col="red",lty=2) 
   
points(counts~elevation,data=stemCounts,
    subset=treatment=="no burn" & habitat=="pine",
    pch=4,col="blue") 
points(lci~elevation,data=preddat,
    subset=treatment=="no burn" & habitat=="pine",
    type='l',col="blue",lty=2)
points(uci~elevation,data=preddat,
    subset=treatment=="no burn" & habitat=="pine",
    type='l',col="blue",lty=2) 
    

## ------------------------------------------------------------------------
fit<- glm(counts~treatment+habitat+elevation,
    data=stemCounts,
    family="quasipoisson")
summary(fit)

## ------------------------------------------------------------------------
preddat<- expand.grid(
    treatment=levels(stemCounts$treatment),
    habitat=levels(stemCounts$habitat),
    elevation=c(min(stemCounts$elevation):max(stemCounts$elevation)))
preddat$lambda<- predict(fit, newdata=preddat,
    type='response')

## ------------------------------------------------------------------------
dispersion<- summary(fit)$dispersion # GET DISPERSION

preddat$lci<-qnbinom(0.025, size=(preddat$lambda/(dispersion-1)),    
    mu=preddat$lambda)
preddat$uci<-qnbinom(0.975, size=(preddat$lambda/(dispersion-1)),    
    mu=preddat$lambda)

## ------------------------------------------------------------------------
plot(counts~elevation,data=stemCounts,
    xlab="Elevation",
    ylab="Stem counts",
    las=1,
    type='n',subset=treatment=="no burn")
points(counts~elevation,data=stemCounts,
    subset=treatment=="no burn" & habitat=="field",
    pch=1,col="black")
## PLOT 95% PREDICTION INTERVALS    
points(lci~elevation,data=preddat,
    subset=treatment=="no burn" & habitat=="field",
    type='l',col="black",lty=2)
points(uci~elevation,data=preddat,
    subset=treatment=="no burn" & habitat=="field",
    type='l',col="black",lty=2)     
 
points(counts~elevation,data=stemCounts,
    subset=treatment=="no burn" & habitat=="riparian",
    pch=2,col="green")  
points(lci~elevation,data=preddat,
    subset=treatment=="no burn" & habitat=="riparian",
    type='l',col="green",lty=2)
points(uci~elevation,data=preddat,
    subset=treatment=="no burn" & habitat=="riparian",
    type='l',col="green",lty=2) 
    
points(counts~elevation,data=stemCounts,
    subset=treatment=="no burn" & habitat=="mixed hardwood",
    pch=3,col="red")    
points(lci~elevation,data=preddat,
    subset=treatment=="no burn" & habitat=="mixed hardwood",
    type='l',col="red",lty=2)
points(uci~elevation,data=preddat,
    subset=treatment=="no burn" & habitat=="mixed hardwood",
    type='l',col="red",lty=2) 
   
points(counts~elevation,data=stemCounts,
    subset=treatment=="no burn" & habitat=="pine",
    pch=4,col="blue") 
points(lci~elevation,data=preddat,
    subset=treatment=="no burn" & habitat=="pine",
    type='l',col="blue",lty=2)
points(uci~elevation,data=preddat,
    subset=treatment=="no burn" & habitat=="pine",
    type='l',col="blue",lty=2) 
    

