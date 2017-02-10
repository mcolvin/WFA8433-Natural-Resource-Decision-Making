## ----unnamed-chunk-1---- ##
dat<-read.csv("homerange-data.csv")
 
 
 
## ----unnamed-chunk-2---- ##
head(dat)
 
 
 
## ----unnamed-chunk-3---- ##
str(dat)
 
 
 
## ----unnamed-chunk-4---- ##
boxplot(homerange~sex,data=dat)

hist(dat$homerange)
 
 
 
## ----unnamed-chunk-5---- ##
levels(dat$sex)
 
 
 
## ----unnamed-chunk-6---- ##
fit<-lm(homerange~sex,data=dat) # fit the model
summary(fit) # model summary
coef(fit) # extract estimated coefficients
betas<-coef(fit) # save betas
 
 
 
## ----unnamed-chunk-7---- ##
dat$sex<- factor(dat$sex,levels=c("male","female")) # reorder factor levels
 
 
 
## ----unnamed-chunk-8---- ##
fit<- lm(homerange~sex,data = dat)
summary(fit) # model summary
coef(fit) # estimated coefficients
 
 
 
## ----unnamed-chunk-9---- ##
fit<-lm(homerange~sex+weight,data=dat)
summary(fit)
coef(fit)
 
 
 
## ----unnamed-chunk-10---- ##
newdat<- data.frame(weight=seq(min(dat$weight), max(dat$weight),1))
head(newdat)
 
 
 
## ----unnamed-chunk-11---- ##
newdat$sex<-"male"
preddat<- newdat
newdat$sex<-"female"
preddat<- rbind(preddat,newdat)
 
 
 
## ----unnamed-chunk-12---- ##
str(preddat)
 
 
 
## ----unnamed-chunk-13---- ##
preddat$sex<- factor(preddat$sex,levels=c("male","female"))
levels(preddat$sex)
 
 
 
## ----unnamed-chunk-14---- ##
preddat$pred<- predict(fit,preddat)
head(preddat)
 
 
 
## ----unnamed-chunk-15---- ##
plot(pred~weight,preddat,type='n',xlab="Weight",ylab="Home range (km)")
points(pred~weight,preddat,subset=sex=="male",type='l',lty=1)
points(pred~weight,preddat,subset=sex=="female",type='l',lty=2)
legend('topleft',legend=c("Males","Female"),pch=c(1,2))
 
 
 
## ----unnamed-chunk-16---- ##
plot(homerange~weight,dat,type='n',xlab="Weight",ylab="Home range (km)",ylim=c(-1,10))
points(homerange~weight,dat,subset=sex=="male",pch=1)
points(homerange~weight,dat,subset=sex=="female",pch=2)

points(pred~weight,preddat,subset=sex=="male",type='l',lty=1)
points(pred~weight,preddat,subset=sex=="female",type='l',lty=2)
legend('topleft',legend=c("Males","Female"),pch=c(1,2))
 
 
 
## ----unnamed-chunk-17---- ##
dat$resids<-resid(fit)
dat$preds<-fitted(fit)

# plot of residuals versus predicted
# should be normal and centered around 0
plot(resids~preds,dat)
abline(h=0)
 
 
 
## ----unnamed-chunk-18---- ##
# plot of observed versus predicted
# should fall on a 1:1 line
plot(homerange~preds,dat)
abline(0,1)
 
 
 
## ----unnamed-chunk-19---- ##
fit<- glm(homerange~sex+weight,dat, family="poisson")
 
 
 
## ----unnamed-chunk-20---- ##
summary(fit)
 
 
 
## ----unnamed-chunk-21---- ##
plot(resid(fit)~fitted(fit))
 
 
 
## ----unnamed-chunk-22---- ##
dat$pred<- predict(fit,dat)
plot(homerange~pred,data = dat)
abline(0,1)
 
 
 
## ----unnamed-chunk-23---- ##
dat$pred<- predict(fit,dat,type='response')
plot(homerange~pred,dat)
abline(0,1)
 
 
 
## ----unnamed-chunk-24---- ##
head(dat)
 
 
 
## ----unnamed-chunk-25---- ##
dat$hr_int<- as.integer(round(dat$homerange,0))
 
 
 
## ----unnamed-chunk-26---- ##
fit<- glm(hr_int~sex+weight,dat, family="poisson")
 
 
 
## ----unnamed-chunk-27---- ##
dat$resid<-resid(fit)
dat$pred<-exp(fitted(fit))# response link
plot(hr_int~pred,dat)
abline(0,1)
 
 
 
## ----unnamed-chunk-28---- ##
plot(resid~pred,dat)
 
 
 
## ----unnamed-chunk-29---- ##
head(dat,10)
 
 
 
## ----unnamed-chunk-30---- ##
dat$hr_mm<-dat$homerange*1000000
fit<-glm(hr_mm~sex+weight,dat,family="poisson")
 
 
 
## ----unnamed-chunk-31---- ##
exp(-5)
exp(2)
 
 
 
## ----unnamed-chunk-32---- ##
dat$lhomerange<-log(dat$homerange)

# plot our transformed data
plot(lhomerange~weight, data= dat,
    xlab="Weight",
    ylab="ln(Homerange)",
    type='n')
points(lhomerange~weight,data=dat,subset=sex=="male",pch=1)
points(lhomerange~weight,data=dat,subset=sex=="female",pch=2)
legend('topleft',legend=c("Males","Female"),pch=c(1,2))
 
 
 
## ----unnamed-chunk-33---- ##
fit<- lm(lhomerange~weight+sex+weight:sex,data=dat)
dat$pred<- fitted(fit) # add predictions to dataset
dat$resid<- resid(fit) # add residuals to dataset
 
 
 
## ----unnamed-chunk-34---- ##
plot(resid~pred,data=dat)
 
 
 
## ----unnamed-chunk-35---- ##
plot(lhomerange~pred,data=dat)
abline(0,1)
 
 
 
## ----unnamed-chunk-36---- ##
summary(fit)
 
 
 
## ----unnamed-chunk-37---- ##
betas<- coef(fit)
betas
sigma<- summary(fit)$sigma
sigma
 
 
 
## ----unnamed-chunk-38---- ##
preddat$pred<- predict(fit, preddat)
 
 
 
## ----unnamed-chunk-39---- ##
head(preddat)
 
 
 
## ----unnamed-chunk-40---- ##
preddat$pred<- exp(preddat$pred)
head(preddat)
 
 
 
## ----unnamed-chunk-41---- ##
plot(homerange~weight, data= dat,
    xlab="Weight",
    ylab="Homerange(km)",
    type='n')
points(homerange~weight,data=dat,subset=sex=="male",pch=1)
points(homerange~weight,data=dat,subset=sex=="female",pch=2)

points(pred~weight,data=preddat,subset=sex=="male",type='l',lty=1)
points(pred~weight,data=preddat,subset=sex=="female",type='l',lty=2)

legend('topleft',legend=c("Males","Female"),pch=c(1,2))
 
 
 
## ----unnamed-chunk-42---- ##
betas
sigma
 
 
 
