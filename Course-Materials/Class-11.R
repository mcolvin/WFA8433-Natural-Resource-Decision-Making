## ----echo=FALSE, out.width="95%"-----------------------------------------
include_graphics("media/banner-11.jpg")
rm(list=objects())

## ------------------------------------------------------------------------
myfunction<- function()
    {
    out<- 2+6
    return(out)
    }

## ------------------------------------------------------------------------
myfunction()

## ------------------------------------------------------------------------
myfunction100<- function()
    {
    out<- 1:100
    return(out)
    }

## ------------------------------------------------------------------------
myfunction100()

## ------------------------------------------------------------------------
myfunctionCustom<- function(x1,x2)
    {
    out<- x1:x2
    return(out)
    }

## ------------------------------------------------------------------------
myfunctionCustom(x1=3,x2=55)

## ----eval=FALSE----------------------------------------------------------
## myfunctionCustom(x1=3)

## ------------------------------------------------------------------------
myfunctionCustom(3,55)

## ------------------------------------------------------------------------
myfunctionCustom(55,3)

## ------------------------------------------------------------------------
myfunctionCustom

## ------------------------------------------------------------------------
myfunctionCustom(x2=55,x1=3)

## ------------------------------------------------------------------------
myfunctionCustom<- function(x1=1,x2=100)
    {
    out<- x1:x2
    return(out)
    }

## ------------------------------------------------------------------------
myfunctionCustom()

## ------------------------------------------------------------------------
myfunctionCustom(x1=50)

## ------------------------------------------------------------------------
myfunctionCustom(x2=66)

## ------------------------------------------------------------------------
# THIS WILL READ IN THE CSV FILE FROM YOUR WORKING
# DIRECTORY 
damage_dat<- read.csv("damage-data.csv") 
head(damage_dat)

## ------------------------------------------------------------------------
# scale group size
damage_dat$group_size_scl<- (damage_dat$group_size-0)/(max(damage_dat$group_size)-0)

## ------------------------------------------------------------------------
plot(area_damaged~habitat_area, damage_dat,
    xlab="Habitat area (ha)",
    ylab="Area damaged (ha)",
    las=1,
    cex=damage_dat$group_size_scl*2)# double the scale

## ------------------------------------------------------------------------
fit<- lm(area_damaged~habitat_area+group_size+habitat_area:group_size,
    data=damage_dat)

## ------------------------------------------------------------------------
preddat<- expand.grid(
    habitat_area=c(min(damage_dat$habitat_area):max(damage_dat$habitat_area)),
    group_size= c(min(damage_dat$group_size):max(damage_dat$group_size)))

## ------------------------------------------------------------------------
predicted<- predict(fit, preddat, 
    interval="prediction", 
    level=0.95)
    

## ------------------------------------------------------------------------
class(predicted)

## ------------------------------------------------------------------------
predicted<- as.data.frame(predicted)

## ------------------------------------------------------------------------
preddat$pred<-predicted$fit
preddat$lci_exact<-predicted$lwr
preddat$uci_exact<-predicted$upr

## ------------------------------------------------------------------------
plot(pred~habitat_area, preddat, 
    subset=group_size==3,type='l',ylim=c(-20,50))

# exact prediction intervals
points( lci_exact~habitat_area, preddat,
    subset=group_size==3,type='l',lty=2)
points( uci_exact~habitat_area, preddat, 
    subset=group_size==3,type='l',lty=2)

## ------------------------------------------------------------------------
sigma<- summary(fit)$sigma
preddat$lci_approximate<- preddat$pred-2*sigma
preddat$uci_approximate<-preddat$pred+2*sigma

## ------------------------------------------------------------------------
plot(pred~habitat_area, preddat, 
   subset=group_size==3,type='l',ylim=c(-20,50))

# exact prediction intervals
points( lci_exact~habitat_area, preddat, 
   subset=group_size==3,type='l',lty=2)
points( uci_exact~habitat_area, preddat, 
   subset=group_size==3,type='l',lty=2)     

# add approximate prediction intervals
points(lci_approximate~habitat_area, preddat, 
   subset=group_size==3,type='l',lty=2,col='red')
points(uci_approximate~habitat_area, preddat, 
   subset=group_size==3,type='l',lty=2,col='red')

## ------------------------------------------------------------------------
vcov(fit) # var-cov matrix of betas

## ------------------------------------------------------------------------

bootpredata<- data.frame(habitat_area=100, group_size=3)

bootSimFun <- function(preddata,obsdata) 
    {
    bootdat <- obsdata[sample(seq(nrow(obsdata)),# sample your data
        size=nrow(obsdata),# how many samples?
        replace=TRUE),] # sample with replacement
    # fit the model    
    bootfit <- lm(area_damaged~habitat_area+
            group_size+
            habitat_area:group_size,
            data=bootdat)
    # Make predictions
    bootpred <- predict(bootfit,newdata=preddata)
    sigma<- summary(bootfit)$sigma
    # Add uncertainty to prediction 
    out<- rnorm(length(bootpred),bootpred,sigma)
    return(out)
    }

## ------------------------------------------------------------------------
bootSimFun(preddat=bootpredata, obsdata=damage_dat)

## ------------------------------------------------------------------------
nsims<- 50
pred<- vector(length=nsims)
for(i in 1:nsims)
    {
    pred[i]<- bootSimFun(preddata=bootpredata, obsdata=damage_dat)
    }


## ------------------------------------------------------------------------
hist(pred)
abline(v=quantile(pred,c(0.025,0.975)),col="red",lwd=5)

## ------------------------------------------------------------------------
# the exact solution
preddat[preddat$group_size==3 & preddat$habitat_area==100,]

## ------------------------------------------------------------------------
quantile(pred,c(0.025,0.975))

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
write.csv(dat,"transport-mortality-data.csv",row.names=FALSE)


## ------------------------------------------------------------------------
dat<- read.csv("transport-mortality-data.csv")

## ------------------------------------------------------------------------
head(dat)

## ------------------------------------------------------------------------
fit<-glm(cbind(n_survived,n_translocated)~temperature+truck+temperature:truck,
    data=dat, 
    family="binomial")

## ------------------------------------------------------------------------
dat$pred<- fitted(fit) # get the fitted values

## ------------------------------------------------------------------------
dat$p<- dat$n_survived/dat$n_translocated

## ------------------------------------------------------------------------
dat$resids<- resid(fit)

## ------------------------------------------------------------------------
plot(resids~pred,dat,
    xlab="Predicted survival",
    ylab="Residual",
    las=1)

## ------------------------------------------------------------------------
plot(p~pred,data=dat,
    xlab="Predicted survival",
    ylab="Observed survival",
    las=1)
abline(0,1)

## ------------------------------------------------------------------------
dat$n_mortalities<- dat$n_translocated-dat$n_survived
fit<-glm(cbind(n_survived,n_mortalities)~temperature+truck+temperature:truck,
    data=dat, family="binomial")
dat$pred<- fitted(fit)
dat$p<- dat$n_survived/dat$n_translocated
dat$resids<- resid(fit)

## ------------------------------------------------------------------------
plot(resids~pred,dat,
    xlab="Predicted survival",
    ylab="Residual",
    las=1)

## ------------------------------------------------------------------------
plot(p~pred,data=dat,
    xlab="Predicted survival",
    ylab="Observed survival",
    las=1)
abline(0,1)

## ------------------------------------------------------------------------
pdat<- data.frame(temperature=10, 
    truck=levels(dat$truck))

## ------------------------------------------------------------------------
plink<-predict(fit,pdat,type="link", interval="prediction")
presponse<-predict(fit,pdat,type="response", interval="prediction")

## ------------------------------------------------------------------------
head(plink)
head(presponse)

## ------------------------------------------------------------------------
betas<- coef(fit)
temp<- 10
tr1_est<- betas[1]+ betas[2]*temp + betas[3]*0 + betas[4]*0*temp
tr2_est<- betas[1]+ betas[2]*temp + betas[3]*1 + betas[4]*1*temp

## ------------------------------------------------------------------------
tr1_est
tr2_est

## ------------------------------------------------------------------------
tr1_S<- exp(tr1_est)/(1+exp(tr1_est))# expit function
tr2_S<- exp(tr2_est)/(1+exp(tr2_est))# expit function
tr1_S
tr2_S

## ------------------------------------------------------------------------
try1<- log(tr1_S/(1-tr1_S))
tr1_est
try1 # should be close, within rounding error

## ------------------------------------------------------------------------
try2<- log(tr2_S/(1-tr2_S))
tr2_est
try2 # should be close, within rounding error

## ------------------------------------------------------------------------
translocate<- 100 # num. critters to translocate

## ------------------------------------------------------------------------
outcomes<- rbinom(100000,translocate, tr1_S)

## ------------------------------------------------------------------------
length(outcomes)

## ------------------------------------------------------------------------
N_survive<-table(outcomes)
N_survive

## ------------------------------------------------------------------------
hist(outcomes)

## ------------------------------------------------------------------------
N_survive_p<-N_survive/sum(N_survive)

## ------------------------------------------------------------------------
sum(N_survive_p)# check to see if it sums to 1

## ------------------------------------------------------------------------
grt35<- length(outcomes[outcomes>35])

## ------------------------------------------------------------------------
grt35/length(outcomes)

## ------------------------------------------------------------------------
combos<- expand.grid(
    temperature=c(8,10,12,14),
    ntranslocate=c(100,250,500,1000),
    truck=c("truck 1","truck 2"))

## ------------------------------------------------------------------------
combos$S<- predict(fit,combos,type="response")
head(combos)

## ------------------------------------------------------------------------
nrow(combos)

