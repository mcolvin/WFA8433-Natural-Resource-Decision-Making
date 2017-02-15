## ----unnamed-chunk-1---- ##
myfunction<- function()
    {
    out<- 2+6
    return(out)
    }
 
 
 
## ----unnamed-chunk-2---- ##
myfunction()
 
 
 
## ----unnamed-chunk-3---- ##
myfunction100<- function()
    {
    out<- 1:100
    return(out)
    }
 
 
 
## ----unnamed-chunk-4---- ##
myfunction100()
 
 
 
## ----unnamed-chunk-5---- ##
myfunctionCustom<- function(x1,x2)
    {
    out<- x1:x2
    return(out)
    }
 
 
 
## ----unnamed-chunk-6---- ##
myfunctionCustom(x1=3,x2=55)
 
 
 
## ----unnamed-chunk-7---- ##
myfunctionCustom(3,55)
 
 
 
## ----unnamed-chunk-8---- ##
myfunctionCustom(55,3)
 
 
 
## ----unnamed-chunk-9---- ##
myfunctionCustom
 
 
 
## ----unnamed-chunk-10---- ##
myfunctionCustom(x2=55,x1=3)
 
 
 
## ----unnamed-chunk-11---- ##
myfunctionCustom<- function(x1=1,x2=100)
    {
    out<- x1:x2
    return(out)
    }
 
 
 
## ----unnamed-chunk-12---- ##
myfunctionCustom()
 
 
 
## ----unnamed-chunk-13---- ##
myfunctionCustom(x1=50)
 
 
 
## ----unnamed-chunk-14---- ##
myfunctionCustom(x2=66)
 
 
 
## ----unnamed-chunk-15---- ##
# THIS WILL READ IN THE CSV FILE FROM YOUR WORKING
# DIRECTORY 
damage_dat<- read.csv("damage-data.csv") 
head(damage_dat)
 
 
 
## ----unnamed-chunk-16---- ##
# scale group size
damage_dat$group_size_scl<- (damage_dat$group_size-0)/(max(damage_dat$group_size)-0)
 
 
 
## ----unnamed-chunk-17---- ##
plot(area_damaged~habitat_area, damage_dat,
    xlab="Habitat area (ha)",
    ylab="Area damaged (ha)",
    las=1,
    cex=damage_dat$group_size_scl*2)# double the scale
 
 
 
## ----unnamed-chunk-18---- ##
fit<- lm(area_damaged~habitat_area+group_size+habitat_area:group_size,
    data=damage_dat)
 
 
 
## ----unnamed-chunk-19---- ##
preddat<- expand.grid(
    habitat_area=c(min(damage_dat$habitat_area):max(damage_dat$habitat_area)),
    group_size= c(min(damage_dat$group_size):max(damage_dat$group_size)))
 
 
 
## ----unnamed-chunk-20---- ##
predicted<- predict(fit, preddat, 
    interval="prediction", 
    level=0.95)
 
 
 
## ----unnamed-chunk-21---- ##
class(predicted)
 
 
 
## ----unnamed-chunk-22---- ##
predicted<- as.data.frame(predicted)
 
 
 
## ----unnamed-chunk-23---- ##
preddat$pred<-predicted$fit
preddat$lci_exact<-predicted$lwr
preddat$uci_exact<-predicted$upr
 
 
 
## ----unnamed-chunk-24---- ##
plot(pred~habitat_area, preddat, 
    subset=group_size==3,type='l',ylim=c(-20,50))

# exact prediction intervals
points( lci_exact~habitat_area, preddat,
    subset=group_size==3,type='l',lty=2)
points( uci_exact~habitat_area, preddat, 
    subset=group_size==3,type='l',lty=2)
 
 
 
## ----unnamed-chunk-25---- ##
sigma<- summary(fit)$sigma
preddat$lci_approximate<- preddat$pred-2*sigma
preddat$uci_approximate<-preddat$pred+2*sigma
 
 
 
## ----unnamed-chunk-26---- ##
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
 
 
 
## ----unnamed-chunk-27---- ##
vcov(fit) # var-cov matrix of betas
 
 
 
## ----unnamed-chunk-28---- ##
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
 
 
 
## ----unnamed-chunk-29---- ##
bootSimFun(preddat=bootpredata, obsdata=damage_dat)
 
 
 
## ----unnamed-chunk-30---- ##
nsims<- 50
pred<- vector(length=nsims)
for(i in 1:nsims)
    {
    pred[i]<- bootSimFun(preddata=bootpredata, obsdata=damage_dat)
    }
 
 
 
## ----unnamed-chunk-31---- ##
hist(pred)
abline(v=quantile(pred,c(0.025,0.975)),col="red",lwd=5)
 
 
 
## ----unnamed-chunk-32---- ##
# the exact solution
preddat[preddat$group_size==3 & preddat$habitat_area==100,]
 
 
 
## ----unnamed-chunk-33---- ##
quantile(pred,c(0.025,0.975))
 
 
 
## ----unnamed-chunk-34---- ##
dat<- read.csv("transport-mortality-data.csv")
 
 
 
## ----unnamed-chunk-35---- ##
head(dat)
 
 
 
## ----unnamed-chunk-36---- ##
fit<-glm(cbind(n_survived,n_translocated)~temperature+truck+temperature:truck,
    data=dat, 
    family="binomial")
 
 
 
## ----unnamed-chunk-37---- ##
dat$pred<- fitted(fit) # get the fitted values
 
 
 
## ----unnamed-chunk-38---- ##
dat$p<- dat$n_survived/dat$n_translocated
 
 
 
## ----unnamed-chunk-39---- ##
dat$resids<- resid(fit)
 
 
 
## ----unnamed-chunk-40---- ##
plot(resids~pred,dat,
    xlab="Predicted survival",
    ylab="Residual",
    las=1)
 
 
 
## ----unnamed-chunk-41---- ##
plot(p~pred,data=dat,
    xlab="Predicted survival",
    ylab="Observed survival",
    las=1)
abline(0,1)
 
 
 
## ----unnamed-chunk-42---- ##
dat$n_mortalities<- dat$n_translocated-dat$n_survived
fit<-glm(cbind(n_survived,n_mortalities)~temperature+truck+temperature:truck,
    data=dat, family="binomial")
dat$pred<- fitted(fit)
dat$p<- dat$n_survived/dat$n_translocated
dat$resids<- resid(fit)
 
 
 
## ----unnamed-chunk-43---- ##
plot(resids~pred,dat,
    xlab="Predicted survival",
    ylab="Residual",
    las=1)
 
 
 
## ----unnamed-chunk-44---- ##
plot(p~pred,data=dat,
    xlab="Predicted survival",
    ylab="Observed survival",
    las=1)
abline(0,1)
 
 
 
## ----unnamed-chunk-45---- ##
pdat<- data.frame(temperature=10, 
    truck=levels(dat$truck))
 
 
 
## ----unnamed-chunk-46---- ##
plink<-predict(fit,pdat,type="link", interval="prediction")
presponse<-predict(fit,pdat,type="response", interval="prediction")
 
 
 
## ----unnamed-chunk-47---- ##
head(plink)
head(presponse)
 
 
 
## ----unnamed-chunk-48---- ##
betas<- coef(fit)
temp<- 10
tr1_est<- betas[1]+ betas[2]*temp + betas[3]*0 + betas[4]*0*temp
tr2_est<- betas[1]+ betas[2]*temp + betas[3]*1 + betas[4]*1*temp
 
 
 
## ----unnamed-chunk-49---- ##
tr1_est
tr2_est
 
 
 
## ----unnamed-chunk-50---- ##
tr1_S<- exp(tr1_est)/(1+exp(tr1_est))# expit function
tr2_S<- exp(tr2_est)/(1+exp(tr2_est))# expit function
tr1_S
tr2_S
 
 
 
## ----unnamed-chunk-51---- ##
try1<- log(tr1_S/(1-tr1_S))
tr1_est
try1 # should be close, within rounding error
 
 
 
## ----unnamed-chunk-52---- ##
try2<- log(tr2_S/(1-tr2_S))
tr2_est
try2 # should be close, within rounding error
 
 
 
## ----unnamed-chunk-53---- ##
translocate<- 100 # num. critters to translocate
 
 
 
## ----unnamed-chunk-54---- ##
outcomes<- rbinom(100000,translocate, tr1_S)
 
 
 
## ----unnamed-chunk-55---- ##
length(outcomes)
 
 
 
## ----unnamed-chunk-56---- ##
N_survive<-table(outcomes)
N_survive
 
 
 
## ----unnamed-chunk-57---- ##
hist(outcomes)
 
 
 
## ----unnamed-chunk-58---- ##
N_survive_p<-N_survive/sum(N_survive)
 
 
 
## ----unnamed-chunk-59---- ##
sum(N_survive_p)# check to see if it sums to 1
 
 
 
## ----unnamed-chunk-60---- ##
grt35<- length(outcomes[outcomes>35])
 
 
 
## ----unnamed-chunk-61---- ##
grt35/length(outcomes)
 
 
 
## ----unnamed-chunk-62---- ##
combos<- expand.grid(
    temperature=c(8,10,12,14),
    ntranslocate=c(100,250,500,1000),
    truck=c("truck 1","truck 2"))
 
 
 
## ----unnamed-chunk-63---- ##
combos$S<- predict(fit,combos,type="response")
head(combos)
 
 
 
## ----unnamed-chunk-64---- ##
nrow(combos)
 
 
 
