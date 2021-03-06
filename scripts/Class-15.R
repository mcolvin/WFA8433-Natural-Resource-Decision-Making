## ----unnamed-chunk-1---- ##
set.seed(8675309)

ngroups=50
beta0<- 10
beta0<- beta0+rnorm(ngroups,0,20) # random effect of group

beta1<- 0.95
beta1<- beta1+rnorm(ngroups,0,1.3) # random effect of group
 
 
 
## ----unnamed-chunk-2---- ##
dat<- data.frame(
    beta0 = rep(beta0,30), 
    beta1= rep(beta1, 30), 
	group=rep(c(1:ngroups),30),
    x=runif(ngroups*30,10,50))
dat$group<- as.factor(dat$group)
 
 
 
## ----unnamed-chunk-3---- ##
dat$obs<- dat$beta0+ dat$beta1*dat$x
 
 
 
## ----unnamed-chunk-4---- ##
dat$obs<- rnorm(ngroups*30,dat$obs,50)
 
 
 
## ----unnamed-chunk-5---- ##
library(lattice) # need for xypot
xyplot(obs~x,
    data=dat,
    group=group)
 
 
 
## ----unnamed-chunk-6---- ##
library(lme4)
fit<- lmer(obs~x + (1+x|group), dat)
summary(fit)
 
 
 
## ----unnamed-chunk-7---- ##
nwatersheds<- 35

# A WATERSHED LEVEL COVARIATE
catchmentSize<- c(213,91,326,30,267,
    216,178,167,251,261,139,400,399,  
    56,261,34,90,108,224,312,85,64,
    254,188,266,95,391,327,351,314,
    211,305,170,273,253)
 
 
 
## ----unnamed-chunk-8---- ##
beta0_ws<- 5
beta1_ws<- 0.8
 
 
 
## ----unnamed-chunk-9---- ##
beta0<- beta0_ws +beta1_ws*catchmentSize + rnorm(nwatersheds,0,55)
 
 
 
## ----unnamed-chunk-10---- ##
plot(catchmentSize,beta0,
    xlab="Catchment size",
    ylab="Intercept value")
 
 
 
## ----unnamed-chunk-11---- ##
withinsites<- 80
dat<- data.frame(
    beta0 = rep(beta0,withinsites), 
    beta1= -3.6, 
	group=rep(c(1:nwatersheds),withinsites),
	catchmentSize=rep(catchmentSize,withinsites),
    x=runif(nwatersheds*withinsites,10,50))
dat$group<- as.factor(dat$group)
 
 
 
## ----unnamed-chunk-12---- ##
dat$y<- dat$beta0 + dat$beta1*dat$x
 
 
 
## ----unnamed-chunk-13---- ##
dat$obs<- rnorm(nrow(dat),dat$y,15)
 
 
 
## ----unnamed-chunk-14---- ##
xyplot(obs~x,
    data=dat,
    xlab="Catchment size",
    ylab="Intercept value",
    group=group)
 
 
 
## ----unnamed-chunk-15---- ##
fit<- lmer(obs~x+ catchmentSize + (1|group) , dat)
summary(fit)
 
 
 
## ----unnamed-chunk-16---- ##
fixef(fit)
 
 
 
## ----unnamed-chunk-17---- ##
#install.packages("unmarked")
library(unmarked)
 
 
 
## ----unnamed-chunk-18---- ##
#detections<- matrix(c(),nrow=6, ncol=???)
#detections<-  unmarkedFrameOccu(detections, siteCovs=NULL, obsCovs=NULL)
#detections
 
 
 
## ----unnamed-chunk-19---- ##
#fit <- occu(~ 1 ~ 1, detections)
#fit
#backTransform(fit, type="state")
#backTransform(fit, type="det")
 
 
 
## ----unnamed-chunk-20---- ##
beta0<- -0.6190392 # set occupancy probability to 0.35
psi<- exp(beta0)/(1+exp(beta0)) 
psi
samp_frame<- 3000
site_status<- rbinom(samp_frame,1,psi)
table(site_status) # should be close to psi*3000 and (1-psi)*3000
mean(site_status)# should be close to psi
 
 
 
## ----unnamed-chunk-21---- ##
sites<- data.frame(id=c(1:samp_frame), occupied=site_status)
# lets look at the first 10 rows of the data.frame
head(sites,10)
 
 
 
## ----unnamed-chunk-22---- ##
# the sample function takes a random sample 
# of 35 sites without replacement
my_sample<- sample(sites$id, 35,replace=FALSE)
my_srs<- sites[which(sites$id %in% my_sample),]# get the samples 
# look at our sites
my_srs
 
 
 
## ----unnamed-chunk-23---- ##
table(my_srs$occupied) # should be close to psi*35 and (1-psi)*35
mean(my_srs$occupied)# should be close to psi
 
 
 
## ----unnamed-chunk-24---- ##
p<-1
occasion1<- rbinom(35,1,my_srs$occupied*p) # occasion 1
occasion2<- rbinom(35,1,my_srs$occupied*p) # occasion 2
occasion3<- rbinom(35,1,my_srs$occupied*p) # occasion 3
occasion4<- rbinom(35,1,my_srs$occupied*p) # occasion 4
 
 
 
## ----unnamed-chunk-25---- ##
detections<- cbind(occasion1,occasion2,occasion3,occasion4)
# lets look at the detections
detections
 
 
 
## ----unnamed-chunk-26---- ##
beta_p_0<- -1.386294
p<-exp(beta_p_0)/(1+exp(beta_p_0))
p
occasion1<- rbinom(35,1,my_srs$occupied*p) 
occasion2<- rbinom(35,1, my_srs$occupied*p) 
occasion3<- rbinom(35,1, my_srs$occupied*p) 
occasion4<- rbinom(35,1, my_srs$occupied*p) 
detections<- cbind(occasion1,occasion2,occasion3,occasion4)
detections # lets look at the detections
 
 
 
## ----unnamed-chunk-27---- ##
cbind(detections, my_srs$occupied)
 
 
 
## ----unnamed-chunk-28---- ##
no_detections<- rowSums(detections)# how many detections
occupied<- ifelse(no_detections>0,1,0) # assign sites as occupied or not
mean(occupied)
 
 
 
## ----unnamed-chunk-29---- ##
psi<-0.35
p<- seq(0.1,1,0.1)
p
 
 
 
## ----unnamed-chunk-30---- ##
occupancy_est<- c() # define object to save outputs to
for(i in 1:length(p))
	{
	occasion1<- rbinom(35,1,psi*p[i])
	occupancy_est<- c(occupancy_est,mean(occasion1))
	}
plot(p,occupancy_est, # we are plotting 2 vectors so no data argument
    xlab="Detection probability",
    ylab="Estimated occupancy")
 
 
 
## ----unnamed-chunk-31---- ##
# define matrix to save outputs to for 1000 replictes
occupancy_est<- matrix(NA,nrow=length(p),ncol=1000) 
for(i in 1:length(p))
	{
	for(rep in 1:1000)
		{
		occasion1<- rbinom(35,1,psi*p[i])
		occupancy_est[i,rep]<-mean(occasion1)
		}
	}
boxplot(t(occupancy_est),
    xlab="Detection probability",
    ylab="Estimated occupancy",
    names=p) # name the x axis as the values of p
abline(h=psi,lty=2)
 
 
 
## ----unnamed-chunk-32---- ##
p<-0.2
occasion1<- rbinom(35,1,my_srs$occupied*p) 
occasion2<- rbinom(35,1, my_srs$occupied*p) 
occasion3<- rbinom(35,1, my_srs$occupied*p) 
occasion4<- rbinom(35,1, my_srs$occupied*p) 
detections<- cbind(occasion1,occasion2,occasion3,occasion4)
detections # lets look at the detections
 
 
 
## ----unnamed-chunk-33---- ##
library(unmarked)
 
 
 
## ----unnamed-chunk-34---- ##
detections<-  unmarkedFrameOccu(detections, siteCovs=NULL, obsCovs=NULL)
detections
 
 
 
## ----unnamed-chunk-35---- ##
fit <- occu(~ 1 ~ 1, detections)
fit
 
 
 
## ----unnamed-chunk-36---- ##
coef(fit)[1]# occupancy
coef(fit)[2]# detection
 
 
 
## ----unnamed-chunk-37---- ##
occ_est_lo<- coef(fit)[1]# occupancy logit
exp(occ_est_lo)/(1+exp(occ_est_lo)) # it is close to 0.35!
det_est_lo<- coef(fit)[2]# detection logit
exp(det_est_lo)/(1+exp(det_est_lo)) # it is close to 0.2!
 
 
 
## ----unnamed-chunk-38---- ##
backTransform(fit, type="state")
backTransform(fit, type="det")
 
 
 
