## ----unnamed-chunk-1---- ##
dat<- read.csv("transport-mortality-data.csv")
 
 
 
## ----unnamed-chunk-2---- ##
head(dat)
 
 
 
## ----unnamed-chunk-3---- ##
dat$n_mortalities<- dat$n_translocated-dat$n_survived
 
 
 
## ----unnamed-chunk-4---- ##
fit<-glm(cbind(n_survived,n_mortalities)~temperature+truck+temperature:truck,
    data=dat, family="binomial")
 
 
 
## ----unnamed-chunk-5---- ##
combos<- expand.grid(
    temperature=c(8,10,12,14),
    ntranslocate=c(100,250,500,1000),
    truck=c("truck 1","truck 2"))
head(combos) # look at the combos
 
 
 
## ----unnamed-chunk-6---- ##
combos$S<- predict(fit, # predict survival for each input combination
    newdata=combos,
    type="response")
 
 
 
## ----unnamed-chunk-7---- ##
nsims<- 10000
outcomes<- matrix(0, # fill with 0s
    nrow=nrow(combos), # number of rows
    ncol=nsims) # number of columns
 
 
 
## ----unnamed-chunk-8---- ##
for(i in 1:nrow(combos))
    {
    S<- combos$S[i]
    Nt<- combos$ntranslocate[i]
    outcomes[i,]<-rbinom(nsims,Nt, S)
    }
 
 
 
## ----unnamed-chunk-9---- ##
hist(outcomes[32,])  
 
 
 
## ----unnamed-chunk-10---- ##
table(outcomes[32,])/nsims
 
 
 
## ----unnamed-chunk-11---- ##
success<- rbinom(n=100,size=1,prob=0.3)
 
 
 
## ----unnamed-chunk-12---- ##
table(success)
 
 
 
## ----unnamed-chunk-13---- ##
dat<- read.csv("translocation-success-data.csv")
 
 
 
## ----unnamed-chunk-14---- ##
head(dat)
str(dat)
 
 
 
## ----unnamed-chunk-15---- ##
plot(success~n_survived,data=dat,
    xlab="Number survived",
    ylab="Translocation success")
 
 
 
## ----unnamed-chunk-16---- ##
plot(success~n_survived,data=dat,
    xlab="Number survived",
    ylab="Translocation success",
    subset=habitat=="forest",
    main="Forest")
 
 
 
## ----unnamed-chunk-17---- ##
plot(success~n_survived,data=dat,
    xlab="Number survived",
    ylab="Translocation success",
    subset=habitat=="field",
    main="Field")
 
 
 
## ----unnamed-chunk-18---- ##
plot(success~n_survived,data=dat,
    xlab="Number survived",
    ylab="Translocation success",
    subset=habitat=="swamp",
    main="Swamp")
 
 
 
## ----unnamed-chunk-19---- ##
plot(success~n_survived,data=dat,
    xlab="Number survived",
    ylab="Translocation success",
    type="n")
points(success~n_survived,data=dat,
      subset=habitat=="forest",
      col="blue")
points(success~n_survived,data=dat,
      subset=habitat=="field",
      col="red")
points(success~n_survived,data=dat,
      subset=habitat=="swamp",
      col="green")
 
 
 
## ----unnamed-chunk-20---- ##
par(mfrow=c(3,1))
plot(success~n_survived,data=dat,
    xlab="Number survived",
    ylab="Translocation success",
    subset=habitat=="forest")
plot(success~n_survived,data=dat,
    xlab="Number survived",
    ylab="Translocation success",
    subset=habitat=="field")
 plot(success~n_survived,data=dat,
    xlab="Number survived",
    ylab="Translocation success",
    subset=habitat=="swamp")  
 
 
 
## ----unnamed-chunk-21---- ##
par(mfrow=c(3,1),mar=c(1,1,1,1))
plot(success~n_survived,data=dat,
    xlab="Number survived",
    ylab="Translocation success",
    subset=habitat=="forest")
plot(success~n_survived,data=dat,
    xlab="Number survived",
    ylab="Translocation success",
    subset=habitat=="field")
 plot(success~n_survived,data=dat,
    xlab="Number survived",
    ylab="Translocation success",
    subset=habitat=="swamp")  
 
 
 
## ----unnamed-chunk-22---- ##
par(mfrow=c(3,1),mar=c(6,6,1,1))
plot(success~n_survived,data=dat,
    xlab="Number survived",
    ylab="Translocation success",
    subset=habitat=="forest")
plot(success~n_survived,data=dat,
    xlab="Number survived",
    ylab="Translocation success",
    subset=habitat=="field")
 plot(success~n_survived,data=dat,
    xlab="Number survived",
    ylab="Translocation success",
    subset=habitat=="swamp")  
 
 
 
## ----unnamed-chunk-23---- ##
par(mfrow=c(3,1),mar=c(1,1,1,1),oma=c(3,3,1,1))
plot(success~n_survived,data=dat,
    xlab="",
    ylab="",
    subset=habitat=="forest")
plot(success~n_survived,data=dat,
    xlab="",
    ylab="",
    subset=habitat=="field")
plot(success~n_survived,data=dat,
    xlab="",
    ylab="",
    subset=habitat=="swamp")  
mtext(text="Number survived",
    side=1,# plot on the bottom outer margin
    line=1,# what line to plot on 
    outer=TRUE) # plot in the outer margin
mtext(text="Translocation success",
    side=2, # plot on the left outer margin
    line=1, # what line to plot on 
    outer=TRUE) # plot in the outer margin
 
 
 
## ----unnamed-chunk-24---- ##
fit<- glm(success~n_survived+truck+habitat,
    data=dat,
    family="binomial")
 
 
 
## ----unnamed-chunk-25---- ##
summary(fit)
 
 
 
## ----unnamed-chunk-26---- ##
betas<- coef(fit)
 
 
 
## ----unnamed-chunk-27---- ##
confint(fit)
 
 
 
## ----unnamed-chunk-28---- ##
dat$residuals<- resid(fit)
dat$fitted<- fitted(fit)
 
 
 
## ----unnamed-chunk-29---- ##
plot(residuals~fitted,
    data=dat)
 
 
 
## ----unnamed-chunk-30---- ##
plot(success~fitted,
    data=dat)
 
 
 
## ----unnamed-chunk-31---- ##
preddat<- expand.grid(
    truck=c("truck 1", "truck 2"),
    n_survived=c(min(dat$n_survived):max(dat$n_survived)),
    habitat=c("forest","field","swamp"))
head(preddat)
 
 
 
## ----unnamed-chunk-32---- ##
preddat$pred<- predict(fit, newdata=preddat,
    type="response")# get probabilities, not log odds
 
 
 
## ----unnamed-chunk-33---- ##
plot(pred~n_survived,preddat,type="n",
    xlab="Number surviving",
    ylab="Probability of success",
    las=1)
points(pred~n_survived,preddat,
    subset=truck=="truck 1" & habitat =="forest",
    lty=1,type="l")
points(pred~n_survived,preddat,
    subset=truck=="truck 2" & habitat =="forest",
    lty=2,type="l")    
points(pred~n_survived,preddat,
    subset=truck=="truck 1" & habitat =="field",
    lty=1,col="darkgrey",type="l")
points(pred~n_survived,preddat,
    subset=truck=="truck 2" & habitat =="field",
    lty=2,col="darkgrey",type="l")      
 points(pred~n_survived,preddat,
    subset=truck=="truck 1" & habitat =="swamp",
    lty=1,col="lightgrey",type="l")
points(pred~n_survived,preddat,
    subset=truck=="truck 2" & habitat =="swamp",
    lty=2,col="lightgrey",type="l")    
# PUT A LEGEND ON THIS AND CALL IT A NIGHT!    
legend("topleft",c("Forest Truck 1","Forest Truck 2",
    "Field Truck 1","Field Truck 2",
    "Swamp Truck 1","Swamp Truck 2"),
    col=c("black","black",
        "darkgrey","darkgrey",
        "lightgrey","lightgrey"),lty=c(1,2))
 
 
 
## ----unnamed-chunk-34---- ##
preddat[preddat$n_survived==200,]
 
 
 
