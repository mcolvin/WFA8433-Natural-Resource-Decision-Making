
## ----unnamed-chunk-1---- ##
load("study-area.Rdata")
library(fields)
library(reshape2)
image.plot(x,y,z,xlim=c(0,7),
    xlab="X",ylab="Y",
    ylim=c(0,9), 
    col=rainbow(n=20,start=3/6,end=4/6),
    asp=1)	
 
 
 
## ----unnamed-chunk-2---- ##
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
 
 
 
## ----unnamed-chunk-3---- ##
sample_indx<- sample(1:nrow(sa),nsamples,replace=FALSE)
obs<- y[sample_indx,]
obs
 
 
 
## ----unnamed-chunk-4---- ##
# Prepare data
library(unmarked)
data <- unmarkedFramePCount(y = y)

# ~DETECTION ~ ABUNDANCE
fit <- pcount(~1 ~ 1, 
    data=data, 
    K=50) # SET THIS HIGHER THAN YOUR EXPECTED ABUNDANCE
summary(fit)
 
 
 
## ----unnamed-chunk-5---- ##
# Density
lambda
exp(coef(fit)[1]) # should be close to lambda

# Capture probability
p
exp(coef(fit)[2])/(1+exp(coef(fit)[2])) # should be close p
 
 
 
## ----unnamed-chunk-6---- ##
# GENERATE CAPTURE HISTORIES
visits<-5
p<- exp(gamma_0+gamma_1*sampleSites$depth)/
    (1+exp(gamma_0+gamma_1*sampleSites$depth))
y<- matrix(0,nsamples,visits)
for(i in 1:nsamples)
	{
	y[i,]<- rbinom(visits,sampleSites$abundance[i],p[i])
	}
 
 
 
## ----unnamed-chunk-7---- ##
# PREPARE DATA
data <- unmarkedFramePCount(y = y,
    siteCovs=data.frame(depth=sampleSites$depth))
# FIT THE MODEL WITH DEPTH AS A COVARIATE FOR LAMBDA AND P
fit <- pcount(~depth +1 ~depth+1, 
    data=data, 
    K=150)
fit
 
 
 
