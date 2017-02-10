## ----echo=FALSE, out.width="95%"-----------------------------------------
include_graphics("media/banner-09.jpg")
rm(list=objects())

## ----echo=FALSE, out.width="35%"-----------------------------------------
include_graphics("media/best-bears.jpg")

## ------------------------------------------------------------------------
dat<-read.csv("homerange-data.csv")

## ------------------------------------------------------------------------
head(dat)

## ------------------------------------------------------------------------
str(dat)

## ------------------------------------------------------------------------
boxplot(homerange~sex,data=dat)

hist(dat$homerange)

## ------------------------------------------------------------------------
levels(dat$sex)

## ------------------------------------------------------------------------
fit<-lm(homerange~sex,data=dat) # fit the model
summary(fit) # model summary
coef(fit) # extract estimated coefficients
betas<-coef(fit) # save betas

## ------------------------------------------------------------------------
dat$sex<- factor(dat$sex,levels=c("male","female")) # reorder factor levels

## ------------------------------------------------------------------------
fit<- lm(homerange~sex,data = dat)
summary(fit) # model summary
coef(fit) # estimated coefficients

## ------------------------------------------------------------------------
fit<-lm(homerange~sex+weight,data=dat)
summary(fit)
coef(fit)

## ------------------------------------------------------------------------
newdat<- data.frame(weight=seq(min(dat$weight), max(dat$weight),1))
head(newdat)

## ------------------------------------------------------------------------
newdat$sex<-"male"
preddat<- newdat
newdat$sex<-"female"
preddat<- rbind(preddat,newdat)

## ------------------------------------------------------------------------
str(preddat)

## ------------------------------------------------------------------------
preddat$sex<- factor(preddat$sex,levels=c("male","female"))
levels(preddat$sex)

## ------------------------------------------------------------------------
preddat$pred<- predict(fit,preddat)
head(preddat)

## ------------------------------------------------------------------------
plot(pred~weight,preddat,type='n',xlab="Weight",ylab="Home range (km)")
points(pred~weight,preddat,subset=sex=="male",type='l',lty=1)
points(pred~weight,preddat,subset=sex=="female",type='l',lty=2)
legend('topleft',legend=c("Males","Female"),pch=c(1,2))

## ------------------------------------------------------------------------
plot(homerange~weight,dat,type='n',xlab="Weight",ylab="Home range (km)",ylim=c(-1,10))
points(homerange~weight,dat,subset=sex=="male",pch=1)
points(homerange~weight,dat,subset=sex=="female",pch=2)

points(pred~weight,preddat,subset=sex=="male",type='l',lty=1)
points(pred~weight,preddat,subset=sex=="female",type='l',lty=2)
legend('topleft',legend=c("Males","Female"),pch=c(1,2))

## ------------------------------------------------------------------------
dat$resids<-resid(fit)
dat$preds<-fitted(fit)

# plot of residuals versus predicted
# should be normal and centered around 0
plot(resids~preds,dat)
abline(h=0)

## ------------------------------------------------------------------------
# plot of observed versus predicted
# should fall on a 1:1 line
plot(homerange~preds,dat)
abline(0,1)

## ------------------------------------------------------------------------
fit<- glm(homerange~sex+weight,dat, family="poisson")

## ------------------------------------------------------------------------
summary(fit)

## ------------------------------------------------------------------------
plot(resid(fit)~fitted(fit))

## ------------------------------------------------------------------------
dat$pred<- predict(fit,dat)
plot(homerange~pred,data = dat)
abline(0,1)

## ------------------------------------------------------------------------
dat$pred<- predict(fit,dat,type='response')
plot(homerange~pred,dat)
abline(0,1)

## ------------------------------------------------------------------------
head(dat)

## ------------------------------------------------------------------------
dat$hr_int<- as.integer(round(dat$homerange,0))

## ------------------------------------------------------------------------
fit<- glm(hr_int~sex+weight,dat, family="poisson")

## ----echo=FALSE, out.width="25%"-----------------------------------------
include_graphics("media/awesome-sauce.jpg")

## ------------------------------------------------------------------------
dat$resid<-resid(fit)
dat$pred<-exp(fitted(fit))# response link
plot(hr_int~pred,dat)
abline(0,1)

## ------------------------------------------------------------------------
plot(resid~pred,dat)

## ------------------------------------------------------------------------
head(dat,10)

## ------------------------------------------------------------------------
dat$hr_mm<-dat$homerange*1000000
fit<-glm(hr_mm~sex+weight,dat,family="poisson")

## ------------------------------------------------------------------------
exp(-5)
exp(2)

## ------------------------------------------------------------------------
dat$lhomerange<-log(dat$homerange)

# plot our transformed data
plot(lhomerange~weight, data= dat,
    xlab="Weight",
    ylab="ln(Homerange)",
    type='n')
points(lhomerange~weight,data=dat,subset=sex=="male",pch=1)
points(lhomerange~weight,data=dat,subset=sex=="female",pch=2)
legend('topleft',legend=c("Males","Female"),pch=c(1,2))

## ------------------------------------------------------------------------
fit<- lm(lhomerange~weight+sex+weight:sex,data=dat)
dat$pred<- fitted(fit) # add predictions to dataset
dat$resid<- resid(fit) # add residuals to dataset

## ------------------------------------------------------------------------
plot(resid~pred,data=dat)

## ------------------------------------------------------------------------
plot(lhomerange~pred,data=dat)
abline(0,1)

## ------------------------------------------------------------------------
summary(fit)

## ------------------------------------------------------------------------
betas<- coef(fit)
betas
sigma<- summary(fit)$sigma
sigma

## ------------------------------------------------------------------------
preddat$pred<- predict(fit, preddat)

## ------------------------------------------------------------------------
head(preddat)

## ------------------------------------------------------------------------
preddat$pred<- exp(preddat$pred)
head(preddat)

## ------------------------------------------------------------------------

plot(homerange~weight, data= dat,
    xlab="Weight",
    ylab="Homerange(km)",
    type='n')
points(homerange~weight,data=dat,subset=sex=="male",pch=1)
points(homerange~weight,data=dat,subset=sex=="female",pch=2)

points(pred~weight,data=preddat,subset=sex=="male",type='l',lty=1)
points(pred~weight,data=preddat,subset=sex=="female",type='l',lty=2)

legend('topleft',legend=c("Males","Female"),pch=c(1,2))

## ------------------------------------------------------------------------
betas
sigma

