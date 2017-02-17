## ----echo=FALSE, out.width="95%"-----------------------------------------
include_graphics("media/banner-05.jpg")
rm(list=objects())

## ------------------------------------------------------------------------
dat<- read.csv("transport-mortality-data.csv")

## ------------------------------------------------------------------------
head(dat)

## ------------------------------------------------------------------------
dat$n_mortalities<- dat$n_translocated-dat$n_survived

## ------------------------------------------------------------------------
fit<-glm(cbind(n_survived,n_mortalities)~temperature+truck+temperature:truck,
    data=dat, family="binomial")

## ------------------------------------------------------------------------
combos<- expand.grid(
    temperature=c(8,10,12,14),
    ntranslocate=c(100,250,500,1000),
    truck=c("truck 1","truck 2"))
head(combos) # look at the combos

## ------------------------------------------------------------------------
combos$S<- predict(fit, # predict survival for each input combination
    newdata=combos,
    type="response")

## ------------------------------------------------------------------------
nsims<- 10000
outcomes<- matrix(0, # fill with 0s
    nrow=nrow(combos), # number of rows
    ncol=nsims) # number of columns

## ------------------------------------------------------------------------
for(i in 1:nrow(combos))
    {
    S<- combos$S[i]
    Nt<- combos$ntranslocate[i]
    outcomes[i,]<-rbinom(nsims,Nt, S)
    }

## ------------------------------------------------------------------------
hist(outcomes[32,])  

## ------------------------------------------------------------------------
table(outcomes[32,])/nsims

## ----echo=FALSE, out.width="95%"-----------------------------------------
include_graphics("media/cl12-translocation-network.png")

## ----echo=FALSE, out.width="95%"-----------------------------------------
include_graphics("media/cl12-translocation-network-02.png")

## ----echo=FALSE, out.width="45%"-----------------------------------------
include_graphics("media/cl12-translocation-network-02a.png")

## ----echo=FALSE, out.width="95%"-----------------------------------------
include_graphics("media/cl12-translocation-network-03.png")

## ----echo=FALSE, out.width="45%"-----------------------------------------
include_graphics("media/cl12-translocation-network-03a.png")

## ------------------------------------------------------------------------
success<- rbinom(n=100,size=1,prob=0.3)

## ------------------------------------------------------------------------
table(success)

## ----echo=FALSE----------------------------------------------------------
b<- c(1,-0.02,0.002,-0.0008)
set.seed(8433)
dat<- data.frame(temperature=round(runif(200,3,13),1),
    truck=sample(c("truck 1","truck 2"),200,replace=TRUE))
dat$n_translocated<- floor(runif(200,200,1000) )   

mm<- model.matrix(as.formula("~temperature+truck+temperature:truck"),
    data=dat)
dat$y<- mm %*% b
dat$S<-plogis(dat$y)

dat$n_survived<- dat$n_translocated-rbinom(200,dat$n_translocated,plogis(dat$y))
#dat$p<- dat$n_survived/dat$n_translocated
dat<-dat [,-c(4,5)]
dat$habitat<- as.factor(sample(c("forest","field","swamp"),nrow(dat),replace=TRUE))

mm<- model.matrix(as.formula("~habitat+n_survived + truck "),dat)
betas<- c(-5,-1.8,-0.4,0.03,-1.5)
y<- plogis(mm %*% betas)

dat$success<- rbinom(nrow(dat),1,y)
write.csv(dat,"translocation-success-data.csv",row.names=FALSE)

## ------------------------------------------------------------------------
dat<- read.csv("translocation-success-data.csv")

## ------------------------------------------------------------------------
head(dat)
str(dat)

## ------------------------------------------------------------------------
plot(success~n_survived,data=dat,
    xlab="Number survived",
    ylab="Translocation success")

## ------------------------------------------------------------------------
plot(success~n_survived,data=dat,
    xlab="Number survived",
    ylab="Translocation success",
    subset=habitat=="forest",
    main="Forest")

## ------------------------------------------------------------------------
plot(success~n_survived,data=dat,
    xlab="Number survived",
    ylab="Translocation success",
    subset=habitat=="field",
    main="Field")

## ------------------------------------------------------------------------
plot(success~n_survived,data=dat,
    xlab="Number survived",
    ylab="Translocation success",
    subset=habitat=="swamp",
    main="Swamp")

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
fit<- glm(success~n_survived+truck+habitat,
    data=dat,
    family="binomial")

## ------------------------------------------------------------------------
summary(fit)

## ------------------------------------------------------------------------
betas<- coef(fit)

## ------------------------------------------------------------------------
confint(fit)

## ------------------------------------------------------------------------
dat$residuals<- resid(fit)
dat$fitted<- fitted(fit)

## ------------------------------------------------------------------------
plot(residuals~fitted,
    data=dat)

## ------------------------------------------------------------------------
plot(success~fitted,
    data=dat)

## ------------------------------------------------------------------------
preddat<- expand.grid(
    truck=c("truck 1", "truck 2"),
    n_survived=c(min(dat$n_survived):max(dat$n_survived)),
    habitat=c("forest","field","swamp"))
head(preddat)

## ------------------------------------------------------------------------
preddat$pred<- predict(fit, newdata=preddat,
    type="response")# get probabilities, not log odds

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
preddat[preddat$n_survived==200,]

## ----echo=FALSE, out.width="95%"-----------------------------------------
include_graphics("media/cl12-translocation-network-04.png")

