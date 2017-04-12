## ----unnamed-chunk-1---- ##
#install.packages("msm")
#install.packages("MDPtoolbox")
 
 
 
## ----unnamed-chunk-2---- ##
lambda <- 10 
brks<-c(0,8,11,20,1000)
outcomes<-rpois(50000,lambda)
states<- cut(x=outcomes,
    breaks= brks,
    labels=c("0-8","8-11","11-20","20+"),
    include.lowest=TRUE)
counts<- table(states)
probs<- prop.table(counts)
probs
 
 
 
## ----unnamed-chunk-3---- ##
values<-c(10,15,21,23)
weightedValues<- values*probs
# EXPECTED VALUE WITH PERFECT INFORMATION
ev<- sum(weightedValues) 
 
 
 
## ----unnamed-chunk-4---- ##
# prior probability of presence 
priorPresent<- 0.5 
#prior probability of absence 
priorAbsent<- 1- priorPresent 
 
 
 
## ----unnamed-chunk-5---- ##
#probability of detection given presence 
detectionProbability<-0.8 
#probability not detection given absence  (no false positives)
nonDetecionProbability <-1 
 
 
 
## ----unnamed-chunk-6---- ##
# PROBABILITY OF DETECTION GIVEN PRESENT
detections<- rbinom(500000,1,detectionProbability)
counts<- table(detections)
probs<- prop.table(counts)
probs[1] # Present and not detected
probs[2] # Present and detected
 
 
 
## ----unnamed-chunk-7---- ##
post<-probs[1]*priorPresent/
    (probs[1]*priorPresent + nonDetecionProbability*priorAbsent) 
post 
 
 
 
## ----unnamed-chunk-8---- ##
post<-nonDetecionProbability*priorAbsent/
    (probs[1]*priorPresent + nonDetecionProbability*priorAbsent) 
post 
 
 
 
## ----unnamed-chunk-9---- ##
Z<-matrix(c(0.8,0.2,0,1),
    ncol=2, 
    byrow=TRUE,
    dimnames=list(c("Present","Not present"),
     c("Detected","Not detected")))
Z
 
 
 
## ----unnamed-chunk-10---- ##
p<-matrix(c(0.5,0.5),ncol=2,nrow=2, byrow=TRUE,
    dimnames=list(c("Present","Not present")))
 
 
 
## ----unnamed-chunk-11---- ##
xx<-t(Z) *p
xx/rowSums(xx)
 
 
 
## ----unnamed-chunk-12---- ##
pDetection<-0.23
sites<-c(1,2,3,5,10,20,30,50,75,100)
prob<- c()
for(i in 1:length(sites))
    {
    prob<- c(prob, 
    # proportion of reps with at least 1 site detected
    mean(rbinom(5000,sites[i],pDetection)>=1))
    }
probs<- cbind(sites,prob,1-prob)  
 
 
 
## ----unnamed-chunk-13---- ##
evii<- c(0.48, 0.87, 1.16, 1.58, 2.04, 2.19, 2.20, 2.20, 2.20, 2.20) 
 
 
 
## ----unnamed-chunk-14---- ##
plot(x=sites,y=evii,
    xlab="Number of sites to sample",
    ylab="Value of information",
    pch=19)
abline(h=2.2,lty=2)# Value of perfect information
 
 
 

 
