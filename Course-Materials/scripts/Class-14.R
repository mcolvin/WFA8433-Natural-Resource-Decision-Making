## ----unnamed-chunk-1---- ##
# First create a 4 row by 5 column matrix
MTX <- matrix(c(1,1,1,1,1,
    2,2,2,2,2,
    3,3,3,3,3,
    4,4,4,4,4), 
    ncol = 5, # 5 columns in matrix
    byrow = T) # fill matrix by row

# Print it out
MTX 
 
 
 
## ----unnamed-chunk-2---- ##
t(MTX)
 
 
 
## ----unnamed-chunk-3---- ##
A <- 0.5

# multiply the matrix by a scalar
MTX*A
 
 
 
## ----unnamed-chunk-4---- ##
MTX*10
 
 
 
## ----unnamed-chunk-5---- ##
V = c(10,1,0.1,0.01)
MTX*V
 
 
 
## ----unnamed-chunk-6---- ##
IDENT = matrix(c(1,0,0,0,1,0,0,0,1), ncol= 3)
IDENT
 
 
 
## ----unnamed-chunk-7---- ##
V_new = c(1,2,3)
IDENT %*% V_new
 
 
 
## ----unnamed-chunk-8---- ##
IDENT * V_new
 
 
 
## ----unnamed-chunk-9---- ##
c = matrix(c(2,3,4,5,6,7), ncol = 2)
z = c(10,15)
c %*% z
 
 
 
## ----unnamed-chunk-10---- ##
## check with a little R code
c * z
 
 
 
## ----unnamed-chunk-11---- ##
set.seed(8433)# for reproducibility
n=220 
my_data<- data.frame(
    elevation=round(runif(n,20,600),0),
    habitat=sample(x=c("hab1","hab2","hab3"),
        size=n,
        replace=TRUE))
my_data$habitat<- as.factor(my_data$habitat)
 
 
 
## ----unnamed-chunk-12---- ##
betas<- c(-1.51,# intercept
    0.005, # effect of elevation
    0.1,-0.1, # effect of habitat 2 and 3
    0.002,0.004) # interaction of habitat with elevation
 
 
 
## ----unnamed-chunk-13---- ##
# 1. THE HARD WAY
my_data$Y1<-NA
my_data[my_data$habitat=="hab1",]$Y1<- exp(betas[1]+
    betas[2]*my_data[my_data$habitat=="hab1",]$elevation)
my_data[my_data$habitat=="hab2",]$Y1<- exp(betas[1]+
    betas[2]*my_data[my_data$habitat=="hab2",]$elevation +
    betas[3] +
    betas[5]*my_data[my_data$habitat=="hab2",]$elevation)
my_data[my_data$habitat=="hab3",]$Y1<- exp(betas[1]+
    betas[2]*my_data[my_data$habitat=="hab3",]$elevation +
    betas[4] +
    betas[6]*my_data[my_data$habitat=="hab3",]$elevation)  
 
 
 
## ----unnamed-chunk-14---- ##
plot(Y1~elevation,
    data=my_data,
    ylab="Predictions",
    xlab="Elevations")
 
 
 
## ----unnamed-chunk-15---- ##
# 2. THE EASIER WAY
model_matrix<- model.matrix(
    object=as.formula("~elevation+habitat+elevation:habitat"),
    data=my_data)
head(model_matrix) # THIS IS THE DESIGN MATRIX
 
 
 
## ----unnamed-chunk-16---- ##
X<- model_matrix * betas
head(X)
 
 
 
## ----unnamed-chunk-17---- ##
X<- t(model_matrix) * betas # OK this sort of works
head(X)
 
 
 
## ----unnamed-chunk-18---- ##
X<-t(X)
head(X)
 
 
 
## ----unnamed-chunk-19---- ##
my_data$Y2<- exp(rowSums(X)) # sum the linear predictors
 
 
 
## ----unnamed-chunk-20---- ##
plot(Y2~elevation,
    data=my_data,
    ylab="Predictions",
    xlab="Elevations")
    
 points(Y1~elevation,
    data=my_data,col="red")
 
 
 
## ----unnamed-chunk-21---- ##
# 3. THE EASY WAY
my_data$Y3<- exp(model_matrix %*% betas)
 
 
 
## ----unnamed-chunk-22---- ##
plot(Y2~elevation,
    data=my_data,
    ylab="Predictions",
    xlab="Elevations")
points(Y3~elevation,
    data=my_data,col="red")
 
 
 
## ----unnamed-chunk-23---- ##
plot(Y1~Y2,
    data=my_data,
    xlab="Hard way",
    ylab="Easier way")
abline(0,1)
 
 
 
## ----unnamed-chunk-24---- ##
plot(Y2~Y3,
    data=my_data,
    xlab="Easier way",
    ylab="Easy way")
abline(0,1)
 
 
 
## ----unnamed-chunk-25---- ##
my_data$obs<- rpois(n=nrow(my_data),
    lambda=my_data$Y3)
 
 
 
## ----unnamed-chunk-26---- ##
plot(obs~elevation,
    data=my_data)
 
 
 
## ----unnamed-chunk-27---- ##
fit<-glm(obs~elevation+habitat,
    data=my_data)
summary(fit)
 
 
 
## ----unnamed-chunk-28---- ##
betas_est<- coef(fit)
cbind(betas,betas_est)
 
 
 
## ----unnamed-chunk-29---- ##
my_data$Y<- exp(model_matrix %*% betas + 
    rnorm(nrow(my_data),0,0.3))
 
 
 
## ----unnamed-chunk-30---- ##
my_data$obs<- rpois(n=nrow(my_data),
    lambda=my_data$Y)
 
 
 
## ----unnamed-chunk-31---- ##
plot(obs~elevation,
    data=my_data)
 
 
 
## ----unnamed-chunk-32---- ##
# install.packages("lme4") run this to install the package
library(lme4)
 
 
 
## ----unnamed-chunk-33---- ##
my_data$id<- c(1:nrow(my_data))
head(my_data)
fit<- glmer(obs~ elevation + habitat + elevation:habitat + (1 | id),
    family = poisson(link = "log"), data = my_data) 
 
 
 
## ----unnamed-chunk-34---- ##
ele_mn<- mean(my_data$elevation)
ele_sd<- sd(my_data$elevation)
my_data$elesc<- scale(my_data$elevation,
    center=ele_mn,
    scale=ele_sd)
 
 
 
## ----unnamed-chunk-35---- ##
fit<- glmer(obs~ elesc + habitat + elesc:habitat + (1 | id),
    family = poisson(link = "log"), 
    data = my_data,
    control=glmerControl(optimizer="bobyqa")) 
 
 
 
## ----unnamed-chunk-36---- ##
summary(fit)
 
 
 
## ----unnamed-chunk-37---- ##
betas_est<- coef(fit)
betas_est
 
 
 
## ----unnamed-chunk-38---- ##
betas_est<-fixef(fit) # betsas
 
 
 
## ----unnamed-chunk-39---- ##
re<-ranef(fit) # pull the random effects
 
 
 
## ----unnamed-chunk-40---- ##
hist(unlist(re))
 
 
 
## ----unnamed-chunk-41---- ##
library(lme4)
 
 
 
## ----unnamed-chunk-42---- ##
ngroups=50
 
 
 
## ----unnamed-chunk-43---- ##
set.seed(5150)
beta0<- 10
beta0<- beta0+rnorm(ngroups,0,2) # random effect of group
 
 
 
## ----unnamed-chunk-44---- ##
beta1<- 0.95
 
 
 
## ----unnamed-chunk-45---- ##
dat<- data.frame(beta0 = rep(beta0,30), beta1= rep(beta1, 30), 
	group=rep(c(1:ngroups),30),
    x=runif(ngroups*30,10,50))
dat$group<- as.factor(dat$group)
 
 
 
## ----unnamed-chunk-46---- ##
dat$obs<- dat$beta0+ dat$beta1*dat$x
 
 
 
## ----unnamed-chunk-47---- ##
dat$obs<- rnorm(ngroups*30,dat$obs,1)
 
 
 
## ----unnamed-chunk-48---- ##
# install.packages("lattice") # run if needed
library(lattice)
xyplot(obs~x,
    data=dat,
    group=group)
 
 
 
## ----unnamed-chunk-49---- ##
fit<- lmer(obs~x + (1|group), dat)
summary(fit)
 
 
 
## ----unnamed-chunk-50---- ##
set.seed(8675309)
beta0<- 10
beta0<- beta0+rnorm(ngroups,0,20) # random effect of group

beta1<- 0.95
beta1<- beta1+rnorm(ngroups,0,1.3) # random effect of group
 
 
 
## ----unnamed-chunk-51---- ##
dat<- data.frame(
    beta0 = rep(beta0,30), 
    beta1= rep(beta1, 30), 
	group=rep(c(1:ngroups),30),
    x=runif(ngroups*30,10,50))
dat$group<- as.factor(dat$group)
 
 
 
## ----unnamed-chunk-52---- ##
dat$obs<- dat$beta0+ dat$beta1*dat$x
 
 
 
## ----unnamed-chunk-53---- ##
dat$obs<- rnorm(ngroups*30,dat$obs,50)
 
 
 
## ----unnamed-chunk-54---- ##
xyplot(obs~x,
    data=dat,
    group=group)
 
 
 
## ----unnamed-chunk-55---- ##
fit<- lmer(obs~x + (1+x|group), dat)
summary(fit)
 
 
 
## ----unnamed-chunk-56---- ##
nwatersheds<- 35

# A WATERSHED LEVEL COVARIATE
catchmentSize<- c(213,91,326,30,267,
    216,178,167,251,261,139,400,399,  
    56,261,34,90,108,224,312,85,64,
    254,188,266,95,391,327,351,314,
    211,305,170,273,253)
 
 
 
## ----unnamed-chunk-57---- ##
beta0_ws<- 5
beta1_ws<- 0.8
 
 
 
## ----unnamed-chunk-58---- ##
beta0<- beta0_ws +beta1_ws*catchmentSize + rnorm(nwatersheds,0,55)
 
 
 
## ----unnamed-chunk-59---- ##
plot(catchmentSize,beta0,
    xlab="Catchment size",
    ylab="Intercept value")
 
 
 
## ----unnamed-chunk-60---- ##
withinsites<- 80
dat<- data.frame(
    beta0 = rep(beta0,withinsites), 
    beta1= -3.6, 
	group=rep(c(1:nwatersheds),withinsites),
	catchmentSize=rep(catchmentSize,withinsites),
    x=runif(nwatersheds*withinsites,10,50))
dat$group<- as.factor(dat$group)
 
 
 
## ----unnamed-chunk-61---- ##
dat$y<- dat$beta0 + dat$beta1*dat$x
 
 
 
## ----unnamed-chunk-62---- ##
dat$obs<- rnorm(nrow(dat),dat$y,15)
 
 
 
## ----unnamed-chunk-63---- ##
xyplot(obs~x,
    data=dat,
    xlab="Catchment size",
    ylab="Intercept value",
    group=group)
 
 
 
## ----unnamed-chunk-64---- ##
fit<- lmer(obs~x+ catchmentSize + (1|group) , dat)
summary(fit)
 
 
 
## ----unnamed-chunk-65---- ##
fixef(fit)
 
 
 
