## ----unnamed-chunk-1---- ##
N<- 324
n=35
beta_psi_0<- -0.6190392 # set occupancy probability to 0.35
psi_i<- exp(beta_psi_0)/(1+exp(beta_psi_0)) 
psi_i # 
Z_i<- rbinom(N,1,psi_i)
table(Z_i) # should be close to psi_i*N and (1-psi_i)*N
mean(Z_i)# should be close to 0.35
 
 
 
## ----unnamed-chunk-2---- ##
sites<- data.frame(id=c(1:N), occupied=Z_i)
# lets look at the first 10 rows of the data.frame
head(sites,10)
 
 
 
## ----unnamed-chunk-3---- ##
# the sample function takes a random sample 
# of 35 sites without replacement
n<-35
# does a simple random sample
# by sampling a row number between 1 to N
my_sample<- sample(sites$id, n,replace=FALSE)
# The which() function returns the rows where 
# the condition is true.
my_srs<- sites[which(sites$id %in% my_sample),]# get the samples 
# look at our sites
head(my_srs)
 
 
 
## ----unnamed-chunk-4---- ##
table(my_srs$occupied) # should be close to psi*35 and (1-psi)*35
mean(my_srs$occupied)# should be close to psi
 
 
 
## ----unnamed-chunk-5---- ##
p_i<-1 # homogenous detection prob among sites
# occasion 1
occasion1<- rbinom(35,1,my_srs$occupied*p_i) 
# occasion 2
occasion2<- rbinom(35,1,my_srs$occupied*p_i) 
# occasion 3
occasion3<- rbinom(35,1,my_srs$occupied*p_i) 
# occasion 4
occasion4<- rbinom(35,1,my_srs$occupied*p_i) 
 
 
 
## ----unnamed-chunk-6---- ##
detections<- cbind(occasion1,occasion2,occasion3,occasion4)
# lets look at the detections
head(detections)
 
 
 
## ----unnamed-chunk-7---- ##
beta_p_0<- -1.386294 # 0.2 log odds
p_i<-exp(beta_p_0)/(1+exp(beta_p_0))
p_i
 
 
 
## ----unnamed-chunk-8---- ##
# occasion 1
occasion1<- rbinom(35,1,my_srs$occupied*p_i) 
# occasion 2
occasion2<- rbinom(35,1, my_srs$occupied*p_i) 
# occasion 3
occasion3<- rbinom(35,1, my_srs$occupied*p_i) 
# occasion 4
occasion4<- rbinom(35,1, my_srs$occupied*p_i) 
detections<- cbind(occasion1,occasion2,occasion3,occasion4)
head(detections) # lets look at the detections
 
 
 
## ----unnamed-chunk-9---- ##
head(cbind(detections, my_srs$occupied))
 
 
 
## ----unnamed-chunk-10---- ##
no_detections<- rowSums(detections)# how many detections
occupied<- ifelse(no_detections>0,1,0) # assign sites as occupied or not
mean(occupied) # close to 0.35?
 
 
 
## ----unnamed-chunk-11---- ##
beta_psi_0<- -0.6190392 # set occupancy probability to 0.35
psi_i<- exp(beta_psi_0)/(1+exp(beta_psi_0)) 
psi_i # 
 
 
 
## ----unnamed-chunk-12---- ##
p_i<- seq(0.1,1,0.1)
p_i
 
 
 
## ----unnamed-chunk-13---- ##
# define object to save the naive 
# occupancy estimates
output<- data.frame(p_i=p_i,
    occupancy_est=NA)
for(i in 1:length(p_i))
	{
    # OBSERVATION 1 OF ALL 35 SITES
	occasion1<- rbinom(n,1,psi_i*p_i[i])
	output$occupancy_est[i]<- mean(occasion1)
	}    
 
 
 
## ----unnamed-chunk-14---- ##
plot(occupancy_est~p_i,
    data=output,
    xlab="Detection probability",
    ylab="Estimated occupancy")
 
 
 
## ----unnamed-chunk-15---- ##
# define matrix to save outputs to for 1000 replicates
occupancy_est<- matrix(NA,nrow=1000,ncol=length(p_i)) 
 
 
 
## ----unnamed-chunk-16---- ##
for(i in 1:length(p_i)) #loop over p
	{
	for(rep in 1:1000) # do 1000 stochastic replicates
		{
        # simulate observations
		occasion1<- rbinom(35,1,psi_i*p_i[i]) 
        # calculate naive occupancy
		occupancy_est[rep,i]<-mean(occasion1)
		}
	}
 
 
 
## ----unnamed-chunk-17---- ##
boxplot(occupancy_est,
    xlab="Detection probability",
    ylab="Estimated occupancy",
    names=p_i) # name the x axis as the values of p
# add a line for the true value of psi
abline(h=psi_i,lty=2) 
 
 
 
## ----unnamed-chunk-18---- ##
beta_p_0<- -0.8472979 # set p to 0.3
p_i<- exp(beta_p_0)/(1+exp(beta_p_0)) 
p_i # 
occasion1<- rbinom(35,1,my_srs$occupied*p_i) 
occasion2<- rbinom(35,1, my_srs$occupied*p_i) 
occasion3<- rbinom(35,1, my_srs$occupied*p_i) 
occasion4<- rbinom(35,1, my_srs$occupied*p_i) 
detections<- cbind(occasion1,occasion2,occasion3,occasion4)
head(detections) # lets look at the detections
 
 
 
## ----unnamed-chunk-19---- ##
#install.packages("unmarked")
library(unmarked)
 
 
 
## ----unnamed-chunk-20---- ##
detections<-  unmarkedFrameOccu(detections, 
    siteCovs=NULL, 
    obsCovs=NULL)
head(detections)
 
 
 
## ----unnamed-chunk-21---- ##
fit <- occu(~ 1 ~ 1, detections)
fit
 
 
 
## ----unnamed-chunk-22---- ##
coef(fit)[1]# occupancy
coef(fit)[2]# detection
 
 
 
## ----unnamed-chunk-23---- ##
occ_est_lo<- coef(fit)[1]# occupancy logit
exp(occ_est_lo)/(1+exp(occ_est_lo)) # it is close to 0.35!
det_est_lo<- coef(fit)[2]# detection logit
exp(det_est_lo)/(1+exp(det_est_lo)) # it is close to 0.2!
 
 
 
## ----unnamed-chunk-24---- ##
# back transform occupancy
backTransform(fit, type="state")
# back transform detection probability
backTransform(fit, type="det")
 
 
 
## ----unnamed-chunk-25---- ##
head(my_srs)# the true occupancy
 
 
 
## ----unnamed-chunk-26---- ##
beta_p_0<- -0.8472979 # set p to 0.3
beta_p_1<- -0.02 #
my_srs$X<- runif(n,0,100)
# log odds detection probability
my_srs$lo_p_i<- beta_p_0+beta_p_1*my_srs$X
# detection probability
my_srs$p_i<-exp(my_srs$lo_p_i)/(1+exp(my_srs$lo_p_i))
 
 
 
## ----unnamed-chunk-27---- ##
plot(p_i~X,my_srs,
    xlab="Covariate X",
    ylab="Detection probability")
 
 
 
## ----unnamed-chunk-28---- ##
occasion1<- rbinom(35,1,my_srs$occupied*p_i) 
occasion2<- rbinom(35,1, my_srs$occupied*p_i) 
occasion3<- rbinom(35,1, my_srs$occupied*p_i) 
occasion4<- rbinom(35,1, my_srs$occupied*p_i) 
detections<- cbind(occasion1,occasion2,occasion3,occasion4)
head(detections) # lets look at the detections
 
 
 
## ----unnamed-chunk-29---- ##
site_covs<- my_srs
 
 
 
## ----unnamed-chunk-30---- ##
detections<-  unmarkedFrameOccu(y=detections, 
    siteCovs=site_covs, 
    obsCovs=NULL)
head(detections)
 
 
 
## ----unnamed-chunk-31---- ##
fit <- occu(~ X ~ 1, detections)
fit
 
 
 
## ----unnamed-chunk-32---- ##
coef(fit)[1]# occupancy
coef(fit)[2]# detection
 
 
 
## ----unnamed-chunk-33---- ##
occ_est_lo<- coef(fit)[1]# occupancy logit
exp(occ_est_lo)/(1+exp(occ_est_lo)) 
det_est_lo<- coef(fit)[2]# detection logit
exp(det_est_lo)/(1+exp(det_est_lo)) 
 
 
 
## ----unnamed-chunk-34---- ##
dat<- read.csv("occ-01.csv")
 
 
 
## ----unnamed-chunk-35---- ##
beta_psi_0<- 1.1 
beta_psi_1<- -0.35 #

# log odds occupancy
dat$lo_psi_i<- beta_psi_0+beta_psi_1*dat$depth
# detection probability
dat$psi_i<-exp(dat$lo_psi_i)/(1+exp(dat$lo_psi_i))
 
 
 
## ----unnamed-chunk-36---- ##
plot(psi_i~depth,
    data=dat,
    xlab="Depth",
    ylab="Occupancy")
 
 
 
## ----unnamed-chunk-37---- ##
N<- nrow(dat)
dat$occupancy<- rbinom(N,1,dat$psi_i)
 
 
 
## ----unnamed-chunk-38---- ##
#install.packages("fields")
library(fields)
library(reshape2)
depths<- dcast(dat,x~y,value.var="depth")
x<-depths[,1]
y<-as.numeric(names(depths[-1]))
depths<- as.matrix(depths[,-1])
image.plot(x,y,depths,xlim=c(0,7),xlab="X",
    ylab="Y",
    ylim=c(0,9), 
    col=rainbow(n=20,start=3/6,end=4/6),
    asp=1)	
points(y~x,sampleSites, pch=3,col="black",
    cex=0.6)
legend("topleft",legend="Sample Site",pch=3,bty='n')
 
 
 
## ----unnamed-chunk-39---- ##
# create new plot of depths
image.plot(x,y,depths,xlim=c(0,7),xlab="X",
    ylab="Y",
    ylim=c(0,9), 
    col=rainbow(n=20,start=3/6,end=4/6),
    asp=1)	
points(y~x,
    data=dat, 
    pch=1,
    col="red",
    cex=0.6,
    subset=occupancy==1)
points(y~x,
    data=dat, 
    pch=1,
    col="blue",
    cex=0.6,
    subset=occupancy==0)    
legend("topleft",
    legend=c("Occupied","Unoccupied"),
    pch=1,
    col=c("red","blue"),
    bty='n')
 
 
 
## ----unnamed-chunk-40---- ##
true_occ<- mean(dat$occupancy)
true_occ
 
 
 
## ----unnamed-chunk-41---- ##
beta_p_0<- -0.8472979 # set p to 0.3
beta_p_1<- 8.8 #
# log odds detection probability
sampleSites$lo_p_i<- beta_p_0+beta_p_1*sampleSites$velocity
# detection probability
sampleSites$p_i<-exp(sampleSites$lo_p_i)/(1+exp(sampleSites$lo_p_i))
 
 
 
## ----unnamed-chunk-42---- ##
plot(p_i~velocity,
    data=sampleSites,
    xlab="Velocity",
    ylab="Capture probability")
 
 
 
## ----unnamed-chunk-43---- ##
# GENERATE CAPTURE HISTORIES
nocc<-5
Y_i_t<- matrix(0,n,nocc)
for(i in 1:nocc)
	{
    # FILL EACH ROW WITH DETECTIONS [0,1]
	Y_i_t[,i]<- rbinom(n,1,sampleSites$p_i*sampleSites$occupancy)
	}
 
 
 
## ----unnamed-chunk-44---- ##
site_covs<- sampleSites
 
 
 
## ----unnamed-chunk-45---- ##
detections<-  unmarkedFrameOccu(y=Y_i_t, 
    siteCovs=site_covs, 
    obsCovs=NULL)
head(detections)
 
 
 
## ----unnamed-chunk-46---- ##
fit <- occu(~ velocity ~ depth, detections)
fit
 
 
 
## ----unnamed-chunk-47---- ##
betas<- coef(fit)
dat$pred_lo_psi<- betas[1] + betas[2]*dat$depth
dat$psi_hat_i<- exp(dat$pred_lo_psi)/(1+exp(dat$pred_lo_psi))
 
 
 
## ----unnamed-chunk-48---- ##
par(mfrow=c(1,2),oma=c(1,2,1,2))

# TRUE PSI
psi_true<- dcast(dat,x~y,value.var="psi_i")
x<-psi_true[,1]
y<-as.numeric(names(psi_true[-1]))
psi_true<- as.matrix(psi_true[,-1])
image.plot(x=x,
    y=y,
    z=psi_true,
    xlim=c(0,7),
    breaks=seq(0,1,0.1),
    xlab="X",
    ylab="Y",
    ylim=c(0,9), 
    col=rainbow(n=10,start=3/6,end=4/6),
    asp=1)	
# ESTIMATED PSI
psi_est<- dcast(dat,x~y,value.var="psi_hat_i")
x<-psi_est[,1]
y<-as.numeric(names(psi_est[-1]))
psi_est<- as.matrix(psi_est[,-1])
image.plot(x=x,
    y=y,
    z=psi_est,
    xlim=c(0,7),
    breaks=seq(0,1,0.1),
    xlab="X",
    ylab="Y",
    ylim=c(0,9), 
    col=rainbow(n=10,start=3/6,end=4/6),
    asp=1)
 
 
 
