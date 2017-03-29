## ----echo=FALSE, out.width="100%"----------------------------------------
include_graphics("media/banner-03.jpg")
rm(list=objects())

## ------------------------------------------------------------------------
#install.packages("reshape2")
#install.packages("unmarked")
#install.packages("fields")

## ----echo=FALSE, eval=FALSE----------------------------------------------
## dat<-read.csv("tornado.csv")
## par(mar=c(3,10,1,1))
## plot(0,0,type='n',
##     xlim=range(unlist(dat[,c(2,3)])),
##     ylim=c(1,7),yaxt='n',xlab="",ylab="")
## segments( dat$Minimum,dat$id,  dat$Maximum,dat$id,lwd=4)
## axis(side=2, at=c(1:7), labels=dat$Node,las=1)
## 
## dat<- read.csv("response-profile.csv")
## plot(dat$Crabtree.Creek,xaxt='n',xlab="Number to outplant",
##     ylab="Marginal gain", las=1,type='l',ylim=c(78.5,79),lwd=2)
## points(dat$MF.Paddy.s,lty=2, type='l',lwd=2)
## points(dat$Thomas.Creek,lty=3, type='l',lwd=2)
## legend("topleft", c("Crabtree Creek","MF Paddy's","Thomas Creek"),
##     lty=c(1,2,3),bty='n')
## axis(side=1, at=c(1:6),labels=dat[,1])

## ----echo=FALSE----------------------------------------------------------
## make a dataset
x<- c(1,2,3,4,5,6,5,4,3,2,1)
y<- c(1,3,6,7,9,8,1,1,2,0,1)
thalx<-seq(-10,10,0.1)
slp<- 2.2
thaly<- -3 + slp*thalx
xx<- as.matrix(expand.grid(
	x=seq(0,10,0.2),
	y=seq(0,10,0.2)))
pip<-mgcv::in.out(as.matrix(cbind(x,y)),xx)

irc<- as.data.frame(xx[pip,])
names(irc)<-c("x","y")
irc$siteId<- c(1:nrow(irc))
irc$depth<- sapply(1:nrow(irc),function(x)
	{
	a=-3
	b=2.2
	x0<-irc$x[x]
	y0<-irc$y[x]
	val<-(((x0+b*y0-b*a)/(b^2 + 1))-x0)^2 + (b*((x0+b*y0-b*a)/(b^2+1))+a-y0)^2
	d<- sqrt(val)
	return(d)
	})
irc$depth<- exp(1.5 + -1.75*irc$depth)
z<- reshape2::dcast(irc,x~y,value.var="depth")
x<-z[,1]
y<-as.numeric(names(z[-1]))
z<- as.matrix(z[,-1])
sa<-irc
## end make dataset
save(sa,x,y,z, file="study-area.Rdata")

## ------------------------------------------------------------------------
load("study-area.Rdata")
library(fields)
library(reshape2)
image.plot(x,y,z,xlim=c(0,7),
    xlab="X",ylab="Y",
    ylim=c(0,9), 
    col=rainbow(n=20,start=3/6,end=4/6),
    asp=1)	

## ----echo=FALSE----------------------------------------------------------
lambda <- 4 # UNDERLYING DENSITY
set.seed(1234) # for reproducibility
sa$abundance<- rpois(nrow(sa),lambda)
z<- dcast(sa,x~y,value.var="abundance")
x<-z[,1]
y<-as.numeric(names(z[-1]))
z<- as.matrix(z[,-1])
image.plot(x,y,z,
    xlim=c(0,7),
    xlab="X",ylab="Y",
    ylim=c(0,9),
    col=heat.colors(n=20),
    asp=1)

## ----echo=FALSE----------------------------------------------------------
nsamples<- 20
sa$siteId<- c(1:nrow(sa))
indx<- sample(1:nrow(sa),nsamples)
sampleSites<- sa[indx,]
image.plot(x=x,y=y,z=z,
    xlim=c(0,7),
    xlab="X",
    ylab="Y",
    ylim=c(0,9),
    col=heat.colors(n=20),
    asp=1)
points(y~x,sampleSites, pch=3,col="black")
legend("topleft",legend="Sample Site",pch=19,bty='n')

## ------------------------------------------------------------------------

nsamples<- 50 # i = 1,2,3,...20
beta_0<- 1.386 # UNDERLYING DENSITY
gamma_0<- -0.405 # LOG ODDS CAPTURE PROBABILITY

# TRANSFORM TO REAL VALUES
lambda <- exp(beta_0)
lambda
p<- exp(gamma_0)/(1+exp(gamma_0) )
p

# SIMULATE ABUNDANCES 
set.seed(1985)# FOR REPRODUCABILITY; LAST YEAR DLR WAS IN VAN HALEN
sa$N<- rpois(nrow(sa),lambda)

# GENERATE CAPTURE HISTORIES
visits<-5 # k = 1,2,3,4,5
# MATRIX TO HOLD VALUES
y<- matrix(0,nrow(sa),visits) # matrix for all possible sites
for(i in 1:nrow(sa))
	{
    for(k in 1:visits)
        {
        y[i,k]<- rbinom(1,sa$N[i],p)#obs count for visit k and site i
        }
	}

## ------------------------------------------------------------------------
sample_indx<- sample(1:nrow(sa),nsamples,replace=FALSE)
obs<- y[sample_indx,]
obs

## ------------------------------------------------------------------------
# Prepare data
library(unmarked)
data <- unmarkedFramePCount(y = y)

# ~DETECTION ~ ABUNDANCE
fit <- pcount(~1 ~ 1, 
    data=data, 
    K=50) # SET THIS HIGHER THAN YOUR EXPECTED ABUNDANCE
summary(fit)

## ------------------------------------------------------------------------
# Density
lambda
exp(coef(fit)[1]) # should be close to lambda

# Capture probability
p
exp(coef(fit)[2])/(1+exp(coef(fit)[2])) # should be close p

## ----echo=FALSE----------------------------------------------------------
beta_0 <- 3
beta_1 <- -0.5

# GET A FEEL FOR THE RELATIONSHIP 
depth <- seq(0,max(sa$depth),0.1)
expectedDensity<- exp(beta_0+beta_1*depth)
plot(depth, # VECTOR OF X VALUES
    expectedDensity,# VECTOR OF Y VALUES
    xlab="Depth",
    ylab="Expected density",
    type='l',
    lwd=2,
    las=1)

## ----echo=FALSE----------------------------------------------------------
sa$abundance <- rpois(nrow(sa),exp(beta_0+beta_1*sa$depth))
plot(abundance~depth,sa,ylab="Abundance",xlab="Depth",las=1)
nsamples<- 40
indx<- sample(1:nrow(sa),nsamples)
sampleSites<- sa[indx,]

## ----echo=FALSE----------------------------------------------------------
gamma_0<- 1
gamma_1<- -0.5
y<- gamma_0+gamma_1*depth
p<- exp(y)/(1+exp(y))

## ----echo=FALSE----------------------------------------------------------
plot(depth,p,xlab="Depth",ylab="Capture probability",las=1,type='l')

## ------------------------------------------------------------------------
# GENERATE CAPTURE HISTORIES
visits<-5
p<- exp(gamma_0+gamma_1*sampleSites$depth)/
    (1+exp(gamma_0+gamma_1*sampleSites$depth))
y<- matrix(0,nsamples,visits)
for(i in 1:nsamples)
	{
	y[i,]<- rbinom(visits,sampleSites$abundance[i],p[i])
	}

## ---- echo=FALSE---------------------------------------------------------
y

## ------------------------------------------------------------------------
# PREPARE DATA
data <- unmarkedFramePCount(y = y,
    siteCovs=data.frame(depth=sampleSites$depth))
# FIT THE MODEL WITH DEPTH AS A COVARIATE FOR LAMBDA AND P
fit <- pcount(~depth +1 ~depth+1, 
    data=data, 
    K=150)
fit

## ----echo=FALSE----------------------------------------------------------
sa$pred<-predict(fit,type="state", 
    newdata=data.frame(depth=sa$depth))$Predicted
par(mfrow=c(1,2),oma=c(1,4,1,1))

## BREAKS FOR PLOTTING
brks<- seq(0,max(sa$pred,sa$abundance)+10,10)
col<- heat.colors(n=length(brks)-1)
z<- dcast(sa,x~y,value.var="abundance")
x<-z[,1]
y<-as.numeric(names(z[-1]))
z<- as.matrix(z[,-1])
image(x,y,z,
    xlim=c(0,7),
    xlab="X",
    ylab="Y",
    ylim=c(0,9),
    breaks=brks,
    col=col,
    asp=1,
    las=1,
    main="True")
# SHOW SAMPLING POINTS
points(y~x,sampleSites, pch=3)    
    
z<- dcast(sa,x~y,
    value.var="pred")
x<-z[,1]
y<-as.numeric(names(z[-1]))
z<- as.matrix(z[,-1])
image.plot(x,y,z,
    xlim=c(0,7),
    xlab="X",
    ylab="Y",
    ylim=c(0,9),
    breaks=brks,
    col=col,
    asp=1,
    las=1,
    main="Predicted")


