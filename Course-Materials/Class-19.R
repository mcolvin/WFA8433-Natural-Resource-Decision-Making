## ----unnamed-chunk-1---- ##
A_poor<- matrix(c(
    0.13, 0.56, 1.64,
    0.25,0,0,
    0, 0.31, 0.58),nrow=3, ncol=3,
    byrow=TRUE)
A_poor  
 
 
 
## ----unnamed-chunk-2---- ##
A_intermediate<- matrix(c(
    0.26, 0.94, 1.93,
    0.33,0,0,
    0, 0.4, 0.66),nrow=3, ncol=3,
    byrow=TRUE)
A_intermediate   
 
 
 
## ----unnamed-chunk-3---- ##
A_good<- matrix(c(
    0.59, 1.76, 2.29,
    0.52,0,0,
    0, 0.6, 0.71),nrow=3, ncol=3,
    byrow=TRUE)
A_good   
 
 
 
## ----unnamed-chunk-4---- ##
N<- c(100,20,50) # STAGE SPECIFIC ABUNDANCE
 
 
 
## ----unnamed-chunk-5---- ##
years<- 20
 
 
 
## ----unnamed-chunk-6---- ##
# A MATRIX TO HOLD ABUNDANCES
output<- matrix(0,3, years)
output[,1]<- N
 
 
 
## ----unnamed-chunk-7---- ##
for(i in 2:years)
    {
    # SOME MATRIX MULTIPLICATION MAGIC
    output[,i]<- A_poor %*% output[,i-1]  
    }
 
 
 
## ----unnamed-chunk-8---- ##
matplot(t(output),type='l',
    xlab="Years",
    ylab="Abundance",
    main="Poor conditions")
legend("topright",legend=c("Juvenile",
    "Yearling","Adult"),
    lty=c(1,2,3), 
    col=c("black","red","green"))
 
 
 
## ----unnamed-chunk-9---- ##
N<- c(100,20,50) # STAGE SPECIFIC ABUNDANCE
years<- 20
# A MATRIX TO HOLD ABUNDANCES
output<- matrix(0,3, years)
output[,1]<- N
for(i in 2:years)
    {
    # SOME MATRIX MULTIPLICATION MAGIC
    output[,i]<- A_intermediate %*% output[,i-1]  
    }
matplot(t(output),type='l',
    xlab="Years",
    ylab="Abundance",
    main="Intermediate conditions")
legend("topleft",legend=c("Juvenile",
    "Yearling","Adult"),
    lty=c(1,2,3), 
    col=c("black","red","green"))
 
 
 
## ----unnamed-chunk-10---- ##
N<- c(100,20,50) # STAGE SPECIFIC ABUNDANCE
years<- 20
# A MATRIX TO HOLD ABUNDANCES
output<- matrix(0,3, years)
output[,1]<- N
for(i in 2:years)
    {
    # SOME MATRIX MULTIPLICATION MAGIC
    output[,i]<- A_good %*% output[,i-1]  
    }
matplot(t(output),type='l',
    xlab="Years",
    ylab="Abundance",
    main="Good conditions")
legend("topleft",legend=c("Juvenile",
    "Yearling","Adult"),
    lty=c(1,2,3), 
    col=c("black","red","green"))
 
 
 
## ----unnamed-chunk-11---- ##
lambda_poor<-eigen(A_poor)$values[1]
lambda_poor<-Re(lambda_poor)# convert from imaginary
 
 
 
## ----unnamed-chunk-12---- ##
library("popbio")
lambda_poor
lambda(A_poor)
 
 
 
## ----unnamed-chunk-13---- ##
lambda_intermediate<- lambda(A_intermediate)
lambda_good<- lambda(A_good)
 
 
 
## ----unnamed-chunk-14---- ##
library(popbio)

reproductive.value(A_poor)
stable.stage(A_poor)
reproductive.value(A_intermediate)
stable.stage(A_intermediate)
reproductive.value(A_good)
stable.stage(A_good)

s_poor<-sensitivity(A_poor) # SENSITIVITY FOR MATRIX A
s_poor # sensitivity matrix
s_intermediate<-sensitivity(A_intermediate) # SENSITIVITY FOR MATRIX A
s_intermediate # sensitivity matrix
s_good<-sensitivity(A_good) # SENSITIVITY FOR MATRIX A
s_good # sensitivity matrix
 
 
 
## ----unnamed-chunk-15---- ##
e_poor<-elasticity(A_poor)
e_poor # elasticity matrix
e_intermediate<-elasticity(A_intermediate) 
e_intermediate # elasticity matrix
e_good<-elasticity(A_good) 
e_good # elasticity matrix
 
 
 
## ----unnamed-chunk-16---- ##
out<-stoch.projection(
    matrices=list(A_poor,
        A_intermediate,
        A_good),
    n0 = c(3000,900, 800),
    tmax=100,
    nreps=5000,
    prob= c(0.35,0.35,0.30),
    nmax=100000) # cut off at 100k all stages

gr<-stoch.growth.rate(
    matrices=list(A_poor,
        A_intermediate,
        A_good), 
  prob = c(0.35,0.35,0.30))  
gr        
 
 
 
## ----unnamed-chunk-17---- ##
n0<-N
F<-c(0.13,0.56,1.64) 
S<-c(0.25,0.31,0.58) 
   
yy<-c(sum(n0*F), # Juveniles
    n0[1]*S[1] , # Yearlings
    n0[2]*S[2]+n0[3]*S[3])# Adults
   
# COMPARE TO MATRIX CALCULATION
y<-A_poor %*% N 

cbind(y,yy)  # winner winner 
 
 
 
## ----unnamed-chunk-18---- ##
n0<-N
F<-c(0.13,0.56,1.64) 
S<-c(0.25,0.31,0.58) 

yy<-c(sum(rpois(3,n0*F)), # Juveniles
   rbinom(1,n0[1],S[1]), # Yearlings
   rbinom(1,n0[2],S[2])+rbinom(1,n0[3],S[3]))# Adults

 
 
 
## ----unnamed-chunk-19---- ##
F_j=c(0.13,0.56,1.64)# poor, intermediate, good
F_y=c(0.56,0.94,1.93)
F_a =c(1.64,1.76,2.29)
        
S_j=c(0.25,0.31,0.58)
S_y=c(0.31,0.40,0.66)
S_a =c(0.58,0.60,0.71)       
 
 
 
## ----unnamed-chunk-20---- ##
x<- c(5,6,7)
# a vector of indices
indexes<- c(1,1,3,2,2,1,1)# 1,2,3 indexes condition
x[indexes] # EXPANDS X GIVEN THE INDEX
 
 
 
## ----unnamed-chunk-21---- ##
set.seed(8433)
reps<- 250000
outcomes<- data.frame(
    condition=sample(c(1:3),reps,replace=TRUE),
    n0_j=rpois(reps,N[1]),
    n0_y=rpois(reps,N[2]),
    n0_a= rpois(reps,N[3])) 
 
 
 
## ----unnamed-chunk-22---- ##
head(outcomes)
 
 
 
## ----unnamed-chunk-23---- ##
outcomes$n1_j<-rpois(reps,outcomes$n0_j*F_j[outcomes$condition])+
    rpois(reps,outcomes$n0_y*F_y[outcomes$condition])+
    rpois(reps,outcomes$n0_a*F_a[outcomes$condition])
 
 
 
## ----unnamed-chunk-24---- ##
outcomes$n1_y<- rbinom(reps,outcomes$n0_j,S_j[outcomes$condition])  
outcomes$n1_a<- rbinom(reps,outcomes$n0_y,S_y[outcomes$condition])+
    rbinom(reps,outcomes$n0_a,S_a[outcomes$condition])
 
 
 
## ----unnamed-chunk-25---- ##
rngs<- apply(outcomes,2,range)
 
 
 
## ----unnamed-chunk-26---- ##
# Juveniles Initial Abundance
bins<- seq(25,175,length.out=5)
labs<- paste(bins[-length(bins)],bins[-1],sep="-")
outcomes$n0_j_bin<-cut(outcomes$n0_j,
    breaks=bins,
    labels=labs,
    include.lowest=TRUE)
# Juveniles Future Abundance
bins<- seq(50,500,length.out=5)
labs<- paste(bins[-length(bins)],bins[-1],sep="-")
outcomes$n1_j_bin<-cut(outcomes$n1_j,
    breaks=bins,
    labels=labs,
    include.lowest=TRUE) 
 
 
 
## ----unnamed-chunk-27---- ##
# Yearling Initial Abundance
bins<- seq(0,50,length.out=5)
labs<- paste(bins[-length(bins)],bins[-1], sep = "-")
outcomes$n0_y_bin<- cut(outcomes$n0_y,
    breaks=bins,
    labels=labs,
    include.lowest=TRUE)
# Yearling Future  Abundance
bins<- seq(0,100,length.out=5)
labs<- paste(bins[-length(bins)],bins[-1],sep="-")
outcomes$n1_y_bin<- cut(outcomes$n1_y,
    breaks=bins,
    labels=labs,
    include.lowest=TRUE) 
 
 
 
## ----unnamed-chunk-28---- ##
# Adult Initial Abundance
bins<- seq(0,100,length.out=5)
labs<- paste(bins[-length(bins)],bins[-1],sep="-")
outcomes$n0_a_bin<- cut(outcomes$n0_a,
    breaks=bins,
    labels=labs,
    include.lowest=TRUE)  
# Adult Future  Abundance
bins<- seq(0,100,length.out=5)
labs<- paste(bins[-length(bins)],bins[-1],sep="-")
outcomes$n1_a_bin<- cut(outcomes$n1_a,
    breaks=bins,
    labels=labs,
    include.lowest=TRUE)
 
 
 
## ----unnamed-chunk-29---- ##
prop.table(table(outcomes$n0_j_bin))
prop.table(table(outcomes$n0_y_bin))
prop.table(table(outcomes$n0_a_bin))
 
 
 
## ----unnamed-chunk-30---- ##
library(reshape2) # for the dcast function
outcomes$tmp<-1 # a value to sum over for counts

future_juveniles<- dcast(data=outcomes, 
    formula=n0_j_bin+n0_y_bin+n0_a_bin+condition~n1_j_bin,
    value.var="tmp",
    fun.aggregate=sum,
    drop=FALSE)  
    
future_yearlings<- dcast(data=outcomes, 
    formula=n0_j_bin+condition~n1_y_bin,
    value.var="tmp",
    fun.aggregate=sum,
    drop=FALSE)  
    
future_adults<- dcast(data=outcomes, 
    formula=n0_a_bin+n0_y_bin+condition~n1_a_bin, 
    value.var="tmp",
    fun.aggregate=sum,
    drop=FALSE) 
 
 
 
## ----unnamed-chunk-31---- ##
future_juveniles[,-c(1,2)]<- prop.table(as.matrix(future_juveniles[,-c(1,2)]),1)
future_yearlings[,-c(1,2)]<- prop.table(as.matrix(future_yearlings[,-c(1,2)]),1)
future_adults[,-c(1,2)]<- prop.table(as.matrix(future_adults[,-c(1,2)]),1)
 
 
 
## ----unnamed-chunk-32---- ##
future_juveniles
future_yearlings
future_adults
write.csv(future_adults,"future_adults.csv")
 
 
 
