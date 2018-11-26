## ----unnamed-chunk-1---- ##
#install.packages("adagio")
 
 
 
## ----unnamed-chunk-2---- ##
F_j=c(0.13,0.56,1.64)# poor, intermediate, good
F_y=c(0.56,0.94,1.93)
F_a =c(1.64,1.76,2.29)
        
S_j=c(0.25,0.31,0.58)
S_y=c(0.31,0.40,0.66)
S_a =c(0.58,0.60,0.71)       

set.seed(8433)
reps<- 250000
outcomes<- data.frame(
    condition=sample(c(1:3),reps,replace=TRUE),
    n0_j=round(runif(reps,25,175),0),
    n0_y=round(runif(reps,0,50),0),
    n0_a=round(runif(reps,0,100),0)) 

outcomes$n1_j<-rpois(reps,outcomes$n0_j*F_j[outcomes$condition])+
    rpois(reps,outcomes$n0_y*F_y[outcomes$condition])+
    rpois(reps,outcomes$n0_a*F_a[outcomes$condition])
outcomes$n1_y<- rbinom(reps,outcomes$n0_j,S_j[outcomes$condition])  
outcomes$n1_a<- rbinom(reps,outcomes$n0_y,S_y[outcomes$condition])+
    rbinom(reps,outcomes$n0_a,S_a[outcomes$condition])
 
 
 
## ----unnamed-chunk-3---- ##
# Juveniles Initial Abundance
bins<- seq(0,175,length.out=5)
labs<- paste(bins[-length(bins)],bins[-1],sep="-")
outcomes$n0_j_bin<-cut(outcomes$n0_j,
    breaks=bins,
    labels=labs,
    include.lowest=TRUE)
# Juveniles Future Abundance
bins<- seq(0,650,length.out=5)
labs<- paste(bins[-length(bins)],bins[-1],sep="-")
outcomes$n1_j_bin<-cut(outcomes$n1_j,
    breaks=bins,
    labels=labs,
    include.lowest=TRUE) 
# Yearling Initial Abundance
bins<- seq(0,50,length.out=5)
labs<- paste(bins[-length(bins)],bins[-1], sep = "-")
outcomes$n0_y_bin<- cut(outcomes$n0_y,
    breaks=bins,
    labels=labs,
    include.lowest=TRUE)
# Yearling Future  Abundance
bins<- seq(0,200,length.out=5)
labs<- paste(bins[-length(bins)],bins[-1],sep="-")
outcomes$n1_y_bin<- cut(outcomes$n1_y,
    breaks=bins,
    labels=labs,
    include.lowest=TRUE) 
# Adult Initial Abundance
bins<- seq(0,100,length.out=5)
labs<- paste(bins[-length(bins)],bins[-1],sep="-")
outcomes$n0_a_bin<- cut(outcomes$n0_a,
    breaks=bins,
    labels=labs,
    include.lowest=TRUE)  
# Adult Future  Abundance
bins<- seq(0,200,length.out=5)
labs<- paste(bins[-length(bins)],bins[-1],sep="-")
outcomes$n1_a_bin<- cut(outcomes$n1_a,
    breaks=bins,
    labels=labs,
    include.lowest=TRUE)
 
 
 
## ----unnamed-chunk-4---- ##
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
    formula=n0_a_bin+n0_j_bin+condition~n1_a_bin, 
    value.var="tmp",
    fun.aggregate=sum,
    drop=FALSE)    
 
 
 
## ----unnamed-chunk-5---- ##
future_juveniles[,-c(1:4)]<- prop.table(
    as.matrix(future_juveniles[,-c(1:4)]),
    margin=1)
future_yearlings[,-c(1,2)]<- prop.table(
    as.matrix(future_yearlings[,-c(1,2)]),
    margin=1)
future_adults[,-c(1:3)]<- prop.table(
    as.matrix(future_adults[,-c(1:3)]),
    margin=1)
 
 
 
## ----unnamed-chunk-6---- ##
write.csv(future_juveniles,"future_juveniles.csv")
write.csv(future_yearlings,"future_yearlings.csv")
write.csv(future_adults,"future_adults.csv")
 
 
 
## ----unnamed-chunk-7---- ##
F_j=c(0.13,0.56,1.64)# poor, intermediate, good
F_y=c(0.56,0.94,1.93)
F_a =c(1.64,1.76,2.29)
        
S_j=c(0.25,0.31,0.58)
S_y=c(0.31,0.40,0.66)
S_a =c(0.58,0.60,0.71)       

set.seed(8433)
reps<- 250000
outcomes<- data.frame(
    condition=sample(c(1:3),reps,replace=TRUE),
    n0_j=round(runif(reps,25,175),0),
    n0_y=round(runif(reps,0,50),0),
    n0_a=round(runif(reps,0,100),0),
    decision=sample(c(1:7),reps,replace=TRUE)) 
 
 
 
## ----unnamed-chunk-8---- ##
harvest_matrix<- matrix(0,nrow=7, ncol=3)
harvest_matrix[1,] <- c(0,0,0.05) 
harvest_matrix[2,] <- c(0,0,0.10)   
harvest_matrix[3,] <- c(0,0,0.15)   
harvest_matrix[4,] <- c(0.05,0.05,0.05)  
harvest_matrix[5,] <- c(0.10,0.10,0.10)   
harvest_matrix[6,] <- c(0.15,0.15,0.15)   
harvest_matrix[7,] <- c(0.00,0.00,0.00)  
 
 
 
## ----unnamed-chunk-9---- ##
outcomes$n1_j<-rpois(reps,rbinom(reps,outcomes$n0_j,1-harvest_matrix[outcomes$decision,1])*F_j[outcomes$condition])+
    rpois(reps,rbinom(reps,outcomes$n0_y,1-harvest_matrix[outcomes$decision,2])*F_y[outcomes$condition])+
    rpois(reps,rbinom(reps,outcomes$n0_a,1-harvest_matrix[outcomes$decision,3])*F_a[outcomes$condition])
 
 
 
## ----unnamed-chunk-10---- ##
outcomes$n1_y<- rbinom(reps,outcomes$n0_j,
    1-harvest_matrix[outcomes$decision,1]*S_j[outcomes$condition])  
outcomes$n1_a<- rbinom(reps,outcomes$n0_y,1-harvest_matrix[outcomes$decision,2]*S_y[outcomes$condition])+
    rbinom(reps,outcomes$n0_a,1-harvest_matrix[outcomes$decision,3]*S_a[outcomes$condition])
 
 
 
## ----unnamed-chunk-11---- ##
# Juveniles Initial Abundance
bins<- seq(0,175,length.out=5)
labs<- paste(bins[-length(bins)],bins[-1],sep="-")
outcomes$n0_j_bin<-cut(outcomes$n0_j,
    breaks=bins,
    labels=labs,
    include.lowest=TRUE)
# Juveniles Future Abundance
bins<- seq(0,650,length.out=5)
labs<- paste(bins[-length(bins)],bins[-1],sep="-")
outcomes$n1_j_bin<-cut(outcomes$n1_j,
    breaks=bins,
    labels=labs,
    include.lowest=TRUE) 
# Yearling Initial Abundance
bins<- seq(0,50,length.out=5)
labs<- paste(bins[-length(bins)],bins[-1], sep = "-")
outcomes$n0_y_bin<- cut(outcomes$n0_y,
    breaks=bins,
    labels=labs,
    include.lowest=TRUE)
# Yearling Future  Abundance
bins<- seq(0,200,length.out=5)
labs<- paste(bins[-length(bins)],bins[-1],sep="-")
outcomes$n1_y_bin<- cut(outcomes$n1_y,
    breaks=bins,
    labels=labs,
    include.lowest=TRUE) 
# Adult Initial Abundance
bins<- seq(0,100,length.out=5)
labs<- paste(bins[-length(bins)],bins[-1],sep="-")
outcomes$n0_a_bin<- cut(outcomes$n0_a,
    breaks=bins,
    labels=labs,
    include.lowest=TRUE)  
# Adult Future  Abundance
bins<- seq(0,200,length.out=5)
labs<- paste(bins[-length(bins)],bins[-1],sep="-")
outcomes$n1_a_bin<- cut(outcomes$n1_a,
    breaks=bins,
    labels=labs,
    include.lowest=TRUE)
 
 
 
## ----unnamed-chunk-12---- ##
library(reshape2) # for the dcast function
outcomes$tmp<-1 # a value to sum over for counts

future_juveniles<- dcast(data=outcomes, 
    formula=n0_j_bin+n0_y_bin+n0_a_bin+condition+decision~n1_j_bin,
    value.var="tmp",
    fun.aggregate=sum,
    drop=FALSE)  
    
future_yearlings<- dcast(data=outcomes, 
    formula=n0_j_bin+condition+decision~n1_y_bin,
    value.var="tmp",
    fun.aggregate=sum,
    drop=FALSE)  
    
future_adults<- dcast(data=outcomes, 
    formula=n0_a_bin+n0_j_bin+condition+decision~n1_a_bin, 
    value.var="tmp",
    fun.aggregate=sum,
    drop=FALSE)    

future_juveniles[,-c(1:5)]<- prop.table(as.matrix(future_juveniles[,-c(1:5)]),1)
future_yearlings[,-c(1:3)]<- prop.table(as.matrix(future_yearlings[,-c(1:3)]),1)
future_adults[,-c(1:4)]<- prop.table(as.matrix(future_adults[,-c(1:4)]),1)
 
 
 
## ----unnamed-chunk-13---- ##
write.csv(future_juveniles,"future_juveniles-with-decisions.csv")
write.csv(future_yearlings,"future_yearlings-with-decisions.csv")
write.csv(future_adults,"future_adults-with-decisions.csv")
 
 
 
## ----unnamed-chunk-14---- ##
S<- 0.8
H<- seq(0,1,by=0.01)

# additive
S_add<- S*(1-H)

# compensatory
C<- 1-S
b<-ifelse(H > C,1,0)
S_comp<-ifelse (H > C, 
    (S*(1-b*H))/(1-C), 
    S*(1-b*H))

# plot results
plot(x=H,y=S_add,
    xlab="Harvest rate",
    ylab="Survival rate",
    type='l',
    lty=1)
points(x=H,y=S_comp,type='l',lty=2)
legend("topright",c("Addtive","Compensatory"),
    lty=c(1,2))
 
 
 
## ----unnamed-chunk-15---- ##
F_j=c(0.13,0.56,1.64)# poor, intermediate, good
F_y=c(0.56,0.94,1.93)
F_a =c(1.64,1.76,2.29)
        
S_j=c(0.25,0.31,0.58)
S_y=c(0.31,0.40,0.66)
S_a =c(0.58,0.60,0.71)       

set.seed(8433)
reps<- 500000
outcomes<- data.frame(
    condition=sample(c(1:3),reps,replace=TRUE),
    n0_j=round(runif(reps,25,175),0),
    n0_y=round(runif(reps,0,50),0),
    n0_a=round(runif(reps,0,100),0),
    decision=sample(c(1:7),reps,replace=TRUE),
    mortalityType=sample(c(1:2),reps,replace=TRUE))# 1 add, 2 pc, 3, comp
 
 
 
## ----unnamed-chunk-16---- ##
harvest_matrix<- matrix(0,nrow=7, ncol=3)
harvest_matrix[1,] <- c(0,0,0.05) 
harvest_matrix[2,] <- c(0,0,0.10)   
harvest_matrix[3,] <- c(0,0,0.15)   
harvest_matrix[4,] <- c(0.05,0.05,0.05)  
harvest_matrix[5,] <- c(0.10,0.10,0.10)   
harvest_matrix[6,] <- c(0.15,0.15,0.15)   
harvest_matrix[7,] <- c(0.00,0.00,0.00)  
 
 
 
## ----unnamed-chunk-17---- ##
# BASELINE SURVIVAL FOR JUVENILES
outcomes$S_j<- S_j[outcomes$condition]# condition specific survival

## ADDITIVE
indx_add<- which(outcomes$mortalityType==1)
outcomes[indx_add,]$S_j<-outcomes[indx_add,]$S_j*
    (1-harvest_matrix[outcomes[indx_add,]$decision,3])

## COMPENSATORY
indx_c<- which(outcomes$mortalityType==2)
C<- 1-outcomes[indx_c,]$S_j
H<- harvest_matrix[outcomes[indx_c,]$decision,3]
b<-ifelse(H > C,1,0)
outcomes[indx_c,]$S_j<-ifelse (H > C, 
    (outcomes[indx_c,]$S_j*(1-b*H))/(1-C), 
    outcomes[indx_c,]$S_j*(1-b*H))    
 
 
 
## ----unnamed-chunk-18---- ##
# BASELINE SURVIVAL FOR YEARLINGS
outcomes$S_y<- S_y[outcomes$condition]# condition specific survival

## ADDITIVE
indx_add<- which(outcomes$mortalityType==1)
outcomes[indx_add,]$S_y<-outcomes[indx_add,]$S_y*(1-harvest_matrix[outcomes[indx_add,]$decision,1])

## COMPENSATORY
indx_c<- which(outcomes$mortalityType==2)
C<- 1-outcomes[indx_c,]$S_y
H<- harvest_matrix[outcomes[indx_c,]$decision,1]
b<-ifelse(H > C,1,0)
outcomes[indx_c,]$S_y<-ifelse (H > C, 
    (outcomes[indx_c,]$S_y*(1-b*H))/(1-C), 
    outcomes[indx_c,]$S_y*(1-b*H))  
 
 
 
## ----unnamed-chunk-19---- ##
# BASELINE SURVIVAL FOR ADULTS
outcomes$S_a<- S_a[outcomes$condition]# condition specific survival

## ADDITIVE
indx_add<- which(outcomes$mortalityType==1)
outcomes[indx_add,]$S_a<-outcomes[indx_add,]$S_a*(1-harvest_matrix[outcomes[indx_add,]$decision,1])

## COMPENSATORY
indx_c<- which(outcomes$mortalityType==2)
C<- 1-outcomes[indx_c,]$S_a
H<- harvest_matrix[outcomes[indx_c,]$decision,3]
b<-ifelse(H > C,1,0)
outcomes[indx_c,]$S_a<-ifelse (H > C, 
    (outcomes[indx_c,]$S_a*(1-b*H))/(1-C), 
    outcomes[indx_c,]$S_a*(1-b*H))   
 
 
 
## ----unnamed-chunk-20---- ##
## FUTURE NUMBER OF JUVENILES
outcomes$n1_j<-rpois(reps,rbinom(reps,outcomes$n0_j,outcomes$S_j)*F_j[outcomes$condition])+
    rpois(reps,rbinom(reps,outcomes$n0_y,outcomes$S_y)*F_y[outcomes$condition])+
    rpois(reps,rbinom(reps,outcomes$n0_a,outcomes$S_a)*F_a[outcomes$condition])
## FUTURE NUMBER OF JUVENILES
outcomes$n1_y<- rbinom(reps,outcomes$n0_j,outcomes$S_j)  
## FUTURE NUMBER OF ADULTS
outcomes$n1_a<- rbinom(reps,outcomes$n0_y,outcomes$S_y)+
    rbinom(reps,outcomes$n0_a,outcomes$S_a)
 
 
 
## ----unnamed-chunk-21---- ##
# Juveniles Initial Abundance
bins<- seq(0,175,length.out=5)
labs<- paste(bins[-length(bins)],bins[-1],sep="-")
outcomes$n0_j_bin<-cut(outcomes$n0_j,
    breaks=bins,
    labels=labs,
    include.lowest=TRUE)
# Juveniles Future Abundance
bins<- seq(0,650,length.out=5)
labs<- paste(bins[-length(bins)],bins[-1],sep="-")
outcomes$n1_j_bin<-cut(outcomes$n1_j,
    breaks=bins,
    labels=labs,
    include.lowest=TRUE) 
# Yearling Initial Abundance
bins<- seq(0,50,length.out=5)
labs<- paste(bins[-length(bins)],bins[-1], sep = "-")
outcomes$n0_y_bin<- cut(outcomes$n0_y,
    breaks=bins,
    labels=labs,
    include.lowest=TRUE)
# Yearling Future  Abundance
bins<- seq(0,200,length.out=5)
labs<- paste(bins[-length(bins)],bins[-1],sep="-")
outcomes$n1_y_bin<- cut(outcomes$n1_y,
    breaks=bins,
    labels=labs,
    include.lowest=TRUE) 
# Adult Initial Abundance
bins<- seq(0,100,length.out=5)
labs<- paste(bins[-length(bins)],bins[-1],sep="-")
outcomes$n0_a_bin<- cut(outcomes$n0_a,
    breaks=bins,
    labels=labs,
    include.lowest=TRUE)  
# Adult Future  Abundance
bins<- seq(0,200,length.out=5)
labs<- paste(bins[-length(bins)],bins[-1],sep="-")
outcomes$n1_a_bin<- cut(outcomes$n1_a,
    breaks=bins,
    labels=labs,
    include.lowest=TRUE)
 
 
 
## ----unnamed-chunk-22---- ##
library(reshape2) # for the dcast function
outcomes$tmp<-1 # a value to sum over for counts

future_juveniles<- dcast(data=outcomes, 
    formula=n0_j_bin+n0_y_bin+n0_a_bin+condition+decision+mortalityType~n1_j_bin,
    value.var="tmp",
    fun.aggregate=sum,
    drop=FALSE)  
    
future_yearlings<- dcast(data=outcomes, 
    formula=n0_j_bin+condition+decision+mortalityType~n1_y_bin,
    value.var="tmp",
    fun.aggregate=sum,
    drop=FALSE)  
    
future_adults<- dcast(data=outcomes, 
    formula=n0_a_bin+n0_j_bin+condition+decision+mortalityType~n1_a_bin, 
    value.var="tmp",
    fun.aggregate=sum,
    drop=FALSE)    

future_juveniles[,-c(1:6)]<- prop.table(as.matrix(future_juveniles[,-c(1:6)]),1)
future_yearlings[,-c(1:4)]<- prop.table(as.matrix(future_yearlings[,-c(1:4)]),1)
future_adults[,-c(1:5)]<- prop.table(as.matrix(future_adults[,-c(1:5)]),1)
 
 
 
## ----unnamed-chunk-23---- ##
write.csv(future_juveniles,"future_juveniles-with-decisions-and-su.csv")
write.csv(future_yearlings,"future_yearlings-with-decisions-and-su.csv")
write.csv(future_adults,"future_adults-with-decisions-and-su.csv")
 
 
 
## ----unnamed-chunk-24---- ##
outcomes$abundance<- outcomes$n1_j+
  outcomes$n1_y+
  outcomes$n1_a
 
 
 
## ----unnamed-chunk-25---- ##
ev<- tapply(outcomes$abundance,outcomes$decision,mean)
ev[which.min(ev)]
 
 
 
## ----unnamed-chunk-26---- ##
library(plyr)
outcomes$abundance<- outcomes$n1_j+outcomes$n1_y+outcomes$n1_a
ev_comp<- dcast(outcomes,decision~"ev",
    value.var="abundance",
    subset=.(mortalityType==2),
    fun.aggregate=mean)
ev<- dcast(outcomes,decision~"ev",
    value.var="abundance",
    subset=.(mortalityType==1),
    fun.aggregate=mean)
 
 
 
## ----unnamed-chunk-27---- ##
stocking<- read.csv("trout-stocking.csv")
head(stocking)
 
 
 
## ----unnamed-chunk-28---- ##
propscale<-function(x)
    {
    (x-min(x))/(max(x)-min(x))
    }
stocking$uc_scaled<-propscale(stocking$urbanCenter_km)
stocking$pol_scaled<-propscale(stocking$politician)
stocking$priv_scaled<-propscale(stocking$troutPrivileges)
stocking$stocked_scaled<-1-propscale(stocking$consectutiveYearsStocked)

stocking$U<- (stocking$uc_scaled+
    stocking$pol_scaled+
    stocking$priv_scaled+
    stocking$stocked_scaled)/4
 
 
 
## ----unnamed-chunk-29---- ##
troutAvailable<-c(2000,3000,5500)
fish_we_want <- as.integer(stocking$requested)
 
 
 
## ----unnamed-chunk-30---- ##
library(adagio)
value<- as.integer(stocking$U*100)
 
 
 
## ----unnamed-chunk-31---- ##
solution <- mknapsack(value, 
      fish_we_want, 
  troutAvailable)
solution

stocking$hatchery<- solution$ksack
 
 
 
