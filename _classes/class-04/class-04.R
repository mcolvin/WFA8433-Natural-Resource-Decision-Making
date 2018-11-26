


# UMBRELLA RISK PROFILE

ppp<- data.frame(
    utility= c(80,0,100),
    pr= c(1,0.3,0.7),
    d= c("Umbrella","No Umbrella","No Umbrella"))
presPlot()
plot(pr~utility,ppp,type='n',xlab="Satisfaction", ylab="Probability",las=1)
points(pr~utility,ppp,subset=d=="Umbrella",col="grey",type='h',lwd=4)
points(pr~utility,ppp,subset=d=="No Umbrella",col="black",type='h',lwd=4)
legend("topleft",title="Decision",c("Umbrella","No Umbrella"),col=c("grey","black"),lwd=4,cex=1.2)


### CUMULATIVE RISK PROFILE

ppp<- data.frame(
    utility= c(0,80,100,0,100),
    pr= c(0,1,1,0.3,1),
    d= c("Umbrella","Umbrella","Umbrella","No Umbrella","No Umbrella"))

plot(pr~utility,ppp,type='n',xlab="Satisfaction", ylab="Cumulative probability",las=1)
points(pr~utility,ppp,subset=d=="Umbrella",col="grey",type='s',lwd=4)
points(pr~utility,ppp,subset=d=="No Umbrella",col="black",type='s',lwd=4)
legend("topleft",title="Decision",c("Umbrella","No Umbrella"),col=c("grey","black"),lwd=4,cex=1.2)




#

ppp<- data.frame(
    utility= c(0,2600,
        0,2047,2320,2730),
    cum_pr= c(0,1,
        0,0.35,0.85,1),
    pr=c(NA,1,
        NA,0.25,0.55,0.15),
    d= c("Field","Field",
        "Lab","Lab","Lab","Lab"))
        
presPlot()
plot(pr~utility,ppp,type='n',
    xlab="Salary", 
    ylab="Probability",las=1,subset=pr>0)
points(pr~utility,ppp,subset=(d=="Field" & !is.na(pr)),
    col="grey",type='h',lwd=4)
points(pr~utility,ppp,subset=(d=="Lab"& !is.na(pr)),
    col="black",type='h',lwd=4)
legend("topleft",title="Decision",
    c("Field position","Lab position"),
    col=c("grey","black"),lwd=4,cex=1.2)

plot(cum_pr~utility,ppp,type='n',
    xlab="Salary", 
    ylab="Probability",las=1,subset=pr>0,ylim=c(0,1))
points(cum_pr~utility,ppp,
    subset=(d=="Field"),
    col="grey",type='s',lwd=4)
points(cum_pr~utility,ppp,subset=(d=="Lab"),
    col="black",type='s',lwd=4)
legend("topleft",title="Decision",
    c("Field position","Lab position"),
    col=c("grey","black"),lwd=4,cex=1.2)
 
 
 ppp<- data.frame(
    utility= c(0,1:4,
        0,1:4),
    cum_pr= c(0,0.1,0.3,0.6,1,
        0,0.1,0.2,0.4,1),
    pr=c(NA,0.1,0.2,0.3,0.4,
        NA,0.1,0.1,0.2,0.6),
    d= c("Field","Field","Field","Field","Field",
        "Lab","Lab","Lab","Lab","Lab"))
        
presPlot()
plot(pr~utility,ppp,type='n',
    xlab="Fun", 
    ylab="Probability",las=1,subset=pr>0)
points(pr~I(utility-0.01),ppp,subset=(d=="Field" & !is.na(pr)),
    col="grey",type='h',lwd=4)
points(pr~I(utility+0.01),ppp,subset=(d=="Lab"& !is.na(pr)),
    col="black",type='h',lwd=4)
legend("topleft",title="Decision",
    c("Field position","Lab position"),
    col=c("grey","black"),lwd=4,cex=1.2)

plot(cum_pr~utility,ppp,type='n',
    xlab="Fun", 
    ylab="Probability",las=1,subset=pr>0,ylim=c(0,1))
points(cum_pr~utility,ppp,
    subset=(d=="Field"),
    col="grey",type='s',lwd=4)
points(cum_pr~utility,ppp,subset=(d=="Lab"),
    col="black",type='s',lwd=4)
legend("topleft",title="Decision",
    c("Field position","Lab position"),
    col=c("grey","black"),lwd=4,cex=1.2)
 
No fun
Fun
Funner
Funnest