## ----unnamed-chunk-1---- ##
weight_mn<- 145
weight_sd<- 10
n<- 150 # number of samples
weights<- rnorm(n,weight_mn,weight_sd)
weights # lets look at the weights
 
 
 
## ----unnamed-chunk-2---- ##
hist(weights)
 
 
 
## ----unnamed-chunk-3---- ##
# the mean
mean(weights) # should be close to 145
# the standard deviation
sd(weights) # should be close to 10
 
 
 
## ----unnamed-chunk-4---- ##
fit<- lm(weights~1)

# THE SUMMARY OF THE LINEAR MODEL
# THE INTERCEPT IS VERY CLOSE TO THE MEAN WE CALCULATED PREVIOUS
summary(fit)
 
output<- summary(fit)
 
 
## ----unnamed-chunk-5---- ##
typeof(fit)
 
 
 
## ----unnamed-chunk-6---- ##
names(fit)
 
 
 
## ----unnamed-chunk-7---- ##
# THE VECTOR OF COEFFICIENTS
fit$coefficients
 fit$residuals
 
 hist(fit$residuals)
 
## ----unnamed-chunk-8---- ##
beta0<- fit$coefficients
 
 beta0
 
## ----unnamed-chunk-9---- ##
mean(weights)-beta0
 
 
 
## ----unnamed-chunk-10---- ##
fit
summary(fit)
 
 
 
## ----unnamed-chunk-11---- ##
names(fit)
 
 
 
## ----unnamed-chunk-12---- ##
output<- summary(fit)
names(output)
 
 
 
## ----unnamed-chunk-13---- ##
# SHOULD BE RIGHT ABOUT 10
output$sigma
 
 
 
## ----unnamed-chunk-14---- ##
sd(weights)-output$sigma
 sd(weights)
 
 
## ----unnamed-chunk-15---- ##
n<- 500000
weights_sim1<- weight_mn + rnorm(n,0,weight_sd)
mean(weights_sim1)
sd(weights_sim1)
 
 
 
## ----unnamed-chunk-16---- ##
weights_sim2<- rnorm(n,weight_mn,weight_sd)
mean(weights_sim2)
sd(weights_sim2)
 
 
 
## ----unnamed-chunk-17---- ##
hist(weights_sim1)
hist(weights_sim2)
 
 
 
## ----unnamed-chunk-18---- ##
# THIS WILL READ IN THE CSV FILE FROM YOUR WORKING
# DIRECTORY 
damage_dat<- read.csv("damage-data.csv") 
damage_dat
 
 
 
## ----unnamed-chunk-19---- ##
plot(area_damaged~habitat_area, damage_dat)
 
 
 
## ----unnamed-chunk-20---- ##
plot(area_damaged~habitat_area, damage_dat,
    xlab="Habitat area (ha)",
    ylab="Area damaged (ha)",
    las=1) # make y-axis labels perpendicular to x-axis
 
 
 
## ----unnamed-chunk-21---- ##
plot(area_damaged~group_size, damage_dat,
    xlab="Group size (#)",
    ylab="Area damaged (ha)",
    las=1,
    pch=20,
    cex=2, col='red')
 
 
 
## ----unnamed-chunk-22---- ##
damage_dat$group_size_scl<- (damage_dat$group_size-0)/(max(damage_dat$group_size)-0)
 
 min(damage_dat$group_size)
 
## ----unnamed-chunk-23---- ##
plot(area_damaged~habitat_area, damage_dat,
    xlab="Habitat area (ha)",
    ylab="Area damaged (ha)",
    las=1,
    cex=damage_dat$group_size_scl)# point sizes scaled to group size
 
 
 
## ----unnamed-chunk-24---- ##
plot(area_damaged~habitat_area, damage_dat,
    xlab="Habitat area (ha)",
    ylab="Area damaged (ha)",
    las=1,
    cex=damage_dat$group_size_scl*2)# double the scale
 
 
 
## ----unnamed-chunk-25---- ##
plot(group_size~habitat_area, damage_dat,
    xlab="Habitat area (ha)",
    ylab="Group size (#)",
    las=1)
 
 
 
## ----unnamed-chunk-26---- ##
pairs(damage_dat)
 
 
 
## ----unnamed-chunk-27---- ##
fit<- lm(area_damaged~habitat_area+
    group_size+
    habitat_area:group_size,
    data=damage_dat)

 
## ----unnamed-chunk-28---- ##
plot(fit)
 
 
 
## ----unnamed-chunk-29---- ##
damage_dat$fitted <- fitted(fit)
plot(area_damaged~fitted,
    data=damage_dat)
abline(0,1)# add a 1:1 line for comparison
 
 
 
## ----unnamed-chunk-30---- ##
damage_dat$resids <- resid(fit)
hist(damage_dat$resids )
 
 
 
## ----unnamed-chunk-31---- ##
summary(fit)
betas<- summary(fit)$coefficients
betas
sigma<- summary(fit)$sigma
sigma
 
 
 
