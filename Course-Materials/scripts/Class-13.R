## ----unnamed-chunk-1---- ##
S<-seq(from=0.1,to=1,by=0.05) # SET SURVIVAL
N<- c(10,50,100,500,1000)
 
 
 
## ----unnamed-chunk-2---- ##
survivors_pois<-rpois(50000,S*N[1])
survivors_bin<-rbinom(50000,N[1],S)
 
 
 
## ----unnamed-chunk-3---- ##
hist(survivors_pois)
hist(survivors_bin)
 
 
 
## ----unnamed-chunk-4---- ##
par(mfrow=c(2,1))
hist(survivors_pois,
    xlim=c(0,10))
hist(survivors_bin,
    xlim=c(0,10))
 
 
 
## ----unnamed-chunk-5---- ##
sims<- expand.grid(N=N,S=S,reps=c(1:500))
sims$surv_pois<-rpois(n=nrow(sims),lambda=sims$S*sims$N)
sims$surv_bin<- rbinom(n=nrow(sims),size=sims$N,prob=sims$S)
 
 
 
## ----unnamed-chunk-6---- ##
boxplot(surv_pois~N,data=sims)
boxplot(surv_bin~N,data=sims,add=TRUE,col='grey')
 
 
 
## ----unnamed-chunk-7---- ##
boxplot(surv_pois~S,data=sims,
    at=1:length(S)-0.25)
boxplot(surv_bin~S,data=sims,add=TRUE,col='grey',
    at=1:length(S)+0.25)
 
 
 
## ----unnamed-chunk-8---- ##
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
 
 
 
## ----unnamed-chunk-9---- ##
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
 
 
 
## ----unnamed-chunk-10---- ##
sims$lambda<- sims$N*sims$S
vars<- aggregate(surv_pois~lambda,sims,var)
plot(surv_pois~lambda,data=vars,   
    xlab="Lambda",
    ylab="Variance")
abline(0,1)
 
 
 
## ----unnamed-chunk-11---- ##
fishCounts<- read.csv("fishCounts.csv")
fishCounts
 
 
 
## ----unnamed-chunk-12---- ##
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
 
 
 
## ----unnamed-chunk-13---- ##
fit<- glm(counts~width+habitat,
    data=fishCounts,
    offset=log(fishCounts$streamLength),# recall it is log 
    family="poisson")
 
 
 
## ----unnamed-chunk-14---- ##
summary(fit)
 
 
 
## ----unnamed-chunk-15---- ##
betas<- coef(fit)
confint(fit)
 
 
 
## ----unnamed-chunk-16---- ##
fishCounts$pred<- fitted(fit) 
fishCounts$res<- resid(fit) 
plot(counts~pred,data=fishCounts)
abline(0,1) 
 
 
 
## ----unnamed-chunk-17---- ##
plot(res~pred,data=fishCounts)
abline(h=0) 
 
 
 
## ----unnamed-chunk-18---- ##
# do 4 streams with varying depths and land cover

preddat<-data.frame(
    width=c(1.3,1.05,0.65,0.7,0.65,0.95),
    habitat=c("forest","forest","urban","urban","ag","ag"),
    streamLength=c(123,324,75,84,68,98))

# preddat$lambda<- predict(fit,newdata=preddat,
  #   type="response")  # does not work 
 
 
 
## ----unnamed-chunk-19---- ##
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
 
 
 
## ----unnamed-chunk-20---- ##
xx<-rpois(10000,10)
xx<- sort(xx)
xx[250]
xx[9750]
 
 
 
## ----unnamed-chunk-21---- ##
vals<-quantile(x=xx,probs=c(0.025,0.975))
vals
 
 
 
## ----unnamed-chunk-22---- ##
vals2<- qpois(c(0.025,0.975),lambda=10)
vals2
 
 
 
## ----unnamed-chunk-23---- ##
preddat$uci<-qpois(0.975, preddat$lambda)
preddat$lci<-qpois(0.025, preddat$lambda)
 
 
 
## ----unnamed-chunk-24---- ##
# MAKE A MATRIX TO STORE OUR PRECIOUS PROBABILITIES
outcomes<- matrix(0,nrow=1001,ncol=nrow(preddat))
 
 
 
## ----unnamed-chunk-25---- ##
dpois(x=3,lambda=10) # should be pretty low
dpois(x=10,lambda=10) # about mid of the road
dpois(x=17,lambda=10) # close to 1
 
 
 
## ----unnamed-chunk-26---- ##
for(i in 1:nrow(preddat))# loop over each row of predat
    {
    outcomes[,i]<-dpois(c(0:1000),lambda=preddat$lambda[i])
    }
 
 
 
## ----unnamed-chunk-27---- ##
colSums(outcomes)
 
 
 
## ----unnamed-chunk-28---- ##
matplot(y=outcomes,type='l',lwd=3)
 
 
 
## ----unnamed-chunk-29---- ##
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
 
 
 
## ----unnamed-chunk-30---- ##
preddat$uci<-qpois(0.975, preddat$lambda)
preddat$lci<-qpois(0.025, preddat$lambda)
 
 
 
## ----unnamed-chunk-31---- ##
plot(uci~width,data=preddat,
    xlab="Stream width",
    ylab="Fish count",
    subset=streamLength==25,
    type='n')
points(lambda~width,data=preddat,
    subset=streamLength==25 & habitat=="forest", 
    lty=1,type='l')
 
 
 
## ----unnamed-chunk-32---- ##
preddat<- preddat[order(preddat$streamLength,preddat$width),]
 
 
 
## ----unnamed-chunk-33---- ##
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
 
 
 
## ----unnamed-chunk-34---- ##
stemCounts<- read.csv("stemCounts.csv")
head(stemCounts)
 
 
 
## ----unnamed-chunk-35---- ##
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
 
 
 
## ----unnamed-chunk-36---- ##
fit<- glm(counts~treatment+habitat+elevation,
    data=stemCounts,
    family="poisson")
summary(fit)
 
 
 
## ----unnamed-chunk-37---- ##
betas<- coef(fit)
confint(fit)
 
 
 
## ----unnamed-chunk-38---- ##
stemCounts$fitted<-fitted(fit)
plot(counts~fitted, data= stemCounts,
    xlab="Predicted values",
    ylab="Counts",
    las=1)
abline(0,1)
 
 
 
## ----unnamed-chunk-39---- ##
preddat<- expand.grid(
    treatment=levels(stemCounts$treatment),
    habitat=levels(stemCounts$habitat),
    elevation=c(min(stemCounts$elevation):max(stemCounts$elevation)))
 
 
 
## ----unnamed-chunk-40---- ##
preddat$lambda<- predict(fit, newdata=preddat,
    type='response')
 
 
 
## ----unnamed-chunk-41---- ##
preddat$lci<-qpois(0.025,preddat$lambda)
preddat$uci<-qpois(0.975,preddat$lambda)
 
 
 
## ----unnamed-chunk-42---- ##
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
 
 
 
## ----unnamed-chunk-43---- ##
fit<- glm(counts~treatment+habitat+elevation,
    data=stemCounts,
    family="quasipoisson")
summary(fit)
 
 
 
## ----unnamed-chunk-44---- ##
preddat<- expand.grid(
    treatment=levels(stemCounts$treatment),
    habitat=levels(stemCounts$habitat),
    elevation=c(min(stemCounts$elevation):max(stemCounts$elevation)))
preddat$lambda<- predict(fit, newdata=preddat,
    type='response')
 
 
 
## ----unnamed-chunk-45---- ##
dispersion<- summary(fit)$dispersion # GET DISPERSION

preddat$lci<-qnbinom(0.025, size=(preddat$lambda/(dispersion-1)),    
    mu=preddat$lambda)
preddat$uci<-qnbinom(0.975, size=(preddat$lambda/(dispersion-1)),    
    mu=preddat$lambda)
 
 
 
## ----unnamed-chunk-46---- ##
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
 
 
 
