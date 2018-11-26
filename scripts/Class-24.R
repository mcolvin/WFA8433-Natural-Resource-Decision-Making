## ----unnamed-chunk-1---- ##
# install.packages("fitdistrplus")
# install.packages("Hmisc")
 
 
 
## ----unnamed-chunk-2---- ##
temp_elic<- data.frame(
    id=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18),
    low=c(68,80,70,79,78,80,76,82,87,50,80,79,77,78,79,85,84,79),
    high=c(81,84,90,84,93,86,88,88,78,100,90,92,85,85,85,88,91,89),
    likely=c(77,82,85,83,86,83,83,86,83,85,85,86,82,81,81,87,87,85),
    confidence=c(75,79,50,40,95,75,80,90,90,25,70,80,80,80,80,5,80,60))
temp_elic
 
 
 
## ----unnamed-chunk-3---- ##
hist(temp_elic$likely,
    main="Elicted likely max. temperatures",
    xlab="Temperature")
abline(v=mean(temp_elic$likely),col='green',lwd=8)
 
 
 
## ----unnamed-chunk-4---- ##
surv<- c(0.5, 0.6, 0.45, 0.65, 0.45, 0.7)
surv_mean<-mean(surv)
surv_var<- var(surv)
 
 
 
## ----unnamed-chunk-5---- ##
### beta method of moments
beta.mom<-function(mean,v)
    {
    x<-mean
    a<-x*(x*(1-x)/v-1)
    b<-(1-x)*(x*(1-x)/v-1)
    c(a,b)
    }
 
 
 
## ----unnamed-chunk-6---- ##
out<-beta.mom(surv_mean,surv_var)
out
 
 
 
## ----unnamed-chunk-7---- ##
library(fitdistrplus)
fit<- fitdist(data=surv,
    distr="beta",
    start=c(shape1=out[1],
        shape2=out[2]))
 
 
 
## ----unnamed-chunk-8---- ##
fit
 
 
 
## ----unnamed-chunk-9---- ##
x<- seq(0,1,0.01) # survivals to plot
 
 
 
## ----unnamed-chunk-10---- ##
dmom<- dbeta(x=x,
    shape1=out[1],
    shape2=out[2])
 
 
 
## ----unnamed-chunk-11---- ##
dmll<- dbeta(x=x,
    shape1=fit$estimate[1],
    shape2=fit$estimate[2] )
 
 
 
## ----unnamed-chunk-12---- ##
plot(x=x,y=dmom,
    xlab="Survival",
    ylab="Likelihood",type='l',
    ylim=c(0,4),
    las=1)
points(x=x,y=dmll, type='l', lty=2)
legend("topleft",
    legend=c("Method of moments","Max. Likelihood"),
    lty=c(1,2))
 
 
 
## ----unnamed-chunk-13---- ##
library(Hmisc)
# weights as whole numbers
wt1<-c(1,1,2,1,2,4)
# weights as proportions
wt2<- wt1/sum(wt1)
 
 
 
## ----unnamed-chunk-14---- ##
#means
wtd.mean(surv,wt1)
wtd.mean(surv,wt2)
#variance
wtd.var(surv,wt1)
#wtd.var(surv,wt2) will not work, needs integer values!
 
 
 
## ----unnamed-chunk-15---- ##
out_w<-beta.mom(mean=wtd.mean(surv,wt1),
v=wtd.var(surv,wt1))
out_w
 
 
 
## ----unnamed-chunk-16---- ##
S<- seq(0,1,0.01)
unweighted<-dbeta(S,
    shape1=out[1],
    shape2=out[2])

weighted<-dbeta(S,
    shape1=out_w[1], 
    shape2= out_w[2])
 
 
 
## ----unnamed-chunk-17---- ##
plot(S,unweighted,
    col="black",
    type='l',
    ylab="Density",
    lwd=2,
    ylim=c(0,4),
    las=1)
points(S,weighted,
    col="red",
    type='l',
    lwd=2)
legend("topleft",
    legend=c("unweighted", "weighted"),
    lty=1,
    col=c("black","red"))
 
 
 
## ----unnamed-chunk-18---- ##
# Absent
Many.Open<-c(0.36,0.37,0.36)
Many.Closed<-c(0.15,0.17,0.17)
Few.Open<-c(0.67,0.69,0.66)
Few.Closed<-c(0.44,0.51,0.56)
 
 
 
## ----unnamed-chunk-19---- ##
#means
mean(Many.Open)
mean(Many.Closed)
mean(Few.Open)
mean(Few.Closed)
 
 
 
## ----unnamed-chunk-20---- ##
#expert weights
wt<-c(5,10,100)
 
 
 
## ----unnamed-chunk-21---- ##
#weighted means
wtd.mean(Many.Open,wt)
wtd.mean(Many.Closed,wt)
wtd.mean(Few.Open,wt)
wtd.mean(Few.Closed,wt)
 
 
 
## ----unnamed-chunk-22---- ##
number_surv<-c(50, 30, 25, 40)
#remember 100 individuals
probability_surv<-number_surv/100
 
 
 
## ----unnamed-chunk-23---- ##
mean(probability_surv)
var(probability_surv)
 
 
 
## ----unnamed-chunk-24---- ##
number_surv<-c(50, 30, 25, 40)
number_die<- 100-number_surv
#let's see what that gets us in terms of means and variance for probability
p_s<-rbeta(1000,number_surv,number_die)
mean(p_s)
var(p_s)
 
 
 
## ----unnamed-chunk-25---- ##
number_surv<-c(25, 15, 25, 40)
number_die<-c(25, 35, 75, 60)
totals<-number_surv+number_die
totals ## notice that the sum of survive and die for expert 1 and 2 is 50

p_s<-rbeta(1000,number_surv,number_die)
mean(p_s)
var(p_s)
 
 
 
## ----unnamed-chunk-26---- ##
p_s<-rbeta(1000,1,1)
mean(p_s)
var(p_s)
#expected value the same, variance different
p_s<-rbeta(1000,100,100)
mean(p_s)
var(p_s)
 
 
 
## ----unnamed-chunk-27---- ##
temp<-c(30, 31, 35, 37, 32, 34.5)
mean(temp)
sd(temp)
 
 
 
## ----unnamed-chunk-28---- ##
pnorm(34,mean(temp),sd(temp))
 
 
 
## ----unnamed-chunk-29---- ##
expertDat<- data.frame(
    ave_summer_temp = c(25,26,27,28,
        29,30,31,32,33,34,35,36,
        37,38,39,40),
    est_survival = c(0.85,0.825,
        0.8,0.75,0.725,0.7,0.65,0.6,
        0.55,0.5,0.45,
        0.4,0.35,0.3,
        0.275,0.225))
expertDat
 
 
 
## ----unnamed-chunk-30---- ##
surv<-round(expertDat$est_survival*100)
died<-100-surv
response<-cbind(surv,died)
glm(response ~ expertDat$ave_summer_temp, 
    fam=binomial)
 
 
 
## ----unnamed-chunk-31---- ##
musselDat<- data.frame(ave_summer_temp=c(14,15,16,17,18,19,
        20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35),
    mussel_growth=c(23.314,24.115,24.716,25.317,25.718,
        25.919,26.120,26.121,26.122,25.923,25.524,25.125,24.526,
        23.927,23.128,22.129,21.130,19.931,18.732,17.333,15.734,
        14.135))

plot(mussel_growth~ave_summer_temp,
    data=musselDat,
    xlab= "Average summer temperature",
    ylab="Mussel growth",
    las=1,
    type='l')
 
 
 
## ----unnamed-chunk-32---- ##
musselDat$ave_summer_tempsq<-musselDat$ave_summer_temp^2
mod<-lm(mussel_growth ~ ave_summer_temp + ave_summer_tempsq, musselDat)
 
 
 
## ----warning=FALSE---- ##
#lower, median, and upper values from Expert 2
w<-c(82,86,88)
c<-0.9
CI<-c((1-c)/2,1-(1-c)/2)
est<-qmedist(w,"norm",probs=CI)
#Estimates
parms2<-est$estimate
parms2
test<-rnorm(1000,parms2[1],parms2[2])
quantile(test,c(0.01,0.5,0.99))
 
 
 
## ----unnamed-chunk-33---- ##
### load library first
library(fitdistrplus)
# 90% confidence so specify lower 5 and upper 95 percentiles
est<-qmedist(data=w,
    distr="beta",
    probs=c(0.05,0.95))
parms<-est$estimate
parms
 
 
 
## ----unnamed-chunk-34---- ##
test<-rbeta(1000,parms[1],parms[2])
quantile(test,c(0.05,0.5,0.95))
 
 
 
## ----unnamed-chunk-35---- ##
parms<-est$estimate
parms
## lets see how close we got
test<-rnorm(1000,parms[1],parms[2])
quantile(test,c(0.10,0.5,0.90))
 
 
 
## ----unnamed-chunk-36---- ##
test<-rnorm(1000,parms1[1],parms1[2])
quantile(test,c(0.005,0.5,0.995))
 
 
 
## ----unnamed-chunk-37---- ##
mu<-seq(50,100,0.1) # possible outcomes
expert1_lik<-dnorm(x=mu,
    mean=parms1[1],
    sd=parms1[2])
expert1_lik<- expert1_lik/sum(expert1_lik) # integrate to 1
 
 
 
## ----unnamed-chunk-38---- ##
#Addition of second expert
expert2_lik<-dnorm(mu,parms2[1],parms2[2])
expert2_lik<- expert2_lik/sum(expert2_lik)
prior<- expert1_lik/sum(expert1_lik) # integrate to 1
plot(x=mu,
    y=prior,
    col="black",
    lty=1,
    lwd=3,
    type='l',
    ylim=c(0,0.025),
    ylab="Probability",
    xlab="Temperature")
lines(x=mu,
    y=expert2_lik,
    col="black",
    lty=2,
    lwd=3,
    type='l')
legend("topleft",c("Expert 1 (prior)",
    "Expert 2 (new)"),
    lty=c(1,2),
    col=c("black","black"))
 
 
 
## ----unnamed-chunk-39---- ##
post<-(prior*expert2_lik)/sum(prior*expert2_lik)
 
 
 
## ----unnamed-chunk-40---- ##
plot(x=mu,
    y=prior,
    col="black",
    lty=1,
    lwd=3,
    type='l',
    ylim=c(0,0.025),
    ylab="Probability",
    xlab="Temperature")
lines(x=mu,
    y=expert2_lik,
    col="black",
    lty=2,
    lwd=3,
    type='l')
lines(mu,post,
    col="red",
    lty=1,
    lwd=3)
legend("topleft",c("Expert 1 (prior)",
    "Expert 2 (new)",
    "Posterior"),
    lty=c(1,2,1),
    lwd=2,
    col=c("black","black","red"))
 
 
 
