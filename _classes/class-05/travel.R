# NOT ACCOUNTING FOR LEAVE TIME
xxx<- data.frame(route=LETTERS[1:4], distance=c(rep(7.2,3), 11.4))
xxx$lambda<-0
xxx[xxx$route=="A" ,]$lambda<- 12.74
xxx[xxx$route=="B" ,]$lambda<- 12.5
xxx[xxx$route=="C" ,]$lambda<- 13.4
xxx[xxx$route=="D",]$lambda<- 13.9
xxx$combo<- 1:nrow(xxx)

out<- data.frame()
for(i in 1:nrow(xxx))
	{
	app<- data.frame(combo=i, ttime=rpois(400000,xxx$lambda[i]))
	out<- rbind(out,app)
	}
	
xxx<- merge(out,xxx,by="combo") 
xxx$tmp<- 1
xxx$bin<-NA
xxx[xxx$tt<=5,]$bin<-"0-5"
xxx[xxx$tt>5&xxx$tt<=10,]$bin<-"5-10"
xxx[xxx$tt>10&xxx$tt<=15,]$bin<-"10-15"
xxx[xxx$tt>15&xxx$tt<=20,]$bin<-"15-20"
xxx[xxx$tt>20&xxx$tt<=25,]$bin<-"25-20"
xxx[xxx$tt>25,]$bin<-"25+"
xxx$bin_f<-factor(xxx$bin, levels=c("0-5","5-10","10-15","15-20","25-20","25+"))
# CPT for tavel 2
cpt<-reshape2::dcast(xxx,route~bin,value.var="tmp",sum)
cpt[,-c(1)]<- cpt[,-c(1)]/rowSums(cpt[,-c(1)])
write.csv(cpt,"cpt.csv")



# A script to simulate varying conditions for travel time to get from work to home


# NOT ACCOUNTING FOR LEAVE TIME
xx<- data.frame(route=LETTERS[1:4], distance=c(rep(7.2,3), 11.4))
leave<- c("4-4.5","4.5-5", "5-5.5","5.5-6")


xxx<- expand.grid(route=LETTERS[1:4], leave=leave)
xxx<- merge(xxx,xx, by="route")

xxx$lambda<- NA
xxx[xxx$route=="A" & xxx$leave=="4-4.5",]$lambda<- 11
xxx[xxx$route=="A" & xxx$leave=="4.5-5",]$lambda<- 12
xxx[xxx$route=="A" & xxx$leave=="5-5.5",]$lambda<- 15
xxx[xxx$route=="A" & xxx$leave=="5.5-6",]$lambda<- 13

xxx[xxx$route=="B" & xxx$leave=="4-4.5",]$lambda<- 12.5
xxx[xxx$route=="B" & xxx$leave=="4.5-5",]$lambda<- 12.5
xxx[xxx$route=="B" & xxx$leave=="5-5.5",]$lambda<- 12.5
xxx[xxx$route=="B" & xxx$leave=="5.5-6",]$lambda<- 12.5

xxx[xxx$route=="C" & xxx$leave=="4-4.5",]$lambda<- 12
xxx[xxx$route=="C" & xxx$leave=="4.5-5",]$lambda<- 13
xxx[xxx$route=="C" & xxx$leave=="5-5.5",]$lambda<- 18
xxx[xxx$route=="C" & xxx$leave=="5.5-6",]$lambda<- 11

xxx[xxx$route=="D" & xxx$leave=="4-4.5",]$lambda<- 14
xxx[xxx$route=="D" & xxx$leave=="4.5-5",]$lambda<- 14
xxx[xxx$route=="D" & xxx$leave=="5-5.5",]$lambda<- 14
xxx[xxx$route=="D" & xxx$leave=="5.5-6",]$lambda<- 14
xxx$combo<- 1:nrow(xxx)

out<- data.frame()
for(i in 1:nrow(xxx))
	{
	app<- data.frame(combo=i, ttime=rpois(400000,xxx$lambda[i]))
	out<- rbind(out,app)
	}
	
xxx<- merge(out,xxx,by="combo") 

# CPT for tavel 2
cpt<-dcast(xxx,route+leave~bin,value.var="tmp",sum)
cpt[,-c(1:2)]<- cpt[,-c(1:2)]/rowSums(cpt[,-c(1:2)])
write.csv(cpt,"cpt.csv")

dev.new(width=10, height=6)
par(mfrow=c(2,2),mar=c(2,2.8,1.5,1),oma=c(3,3,1,1))
hist(xxx[xxx$route=="A",]$ttime,las=1,freq=FALSE,main="Route A",xlab="",xlim=c(0,30))	;box()
abline(v=mean(xxx[xxx$route=="A",]$ttime),col='red',lwd=4)
hist(xxx[xxx$route=="B",]$ttime,las=1,freq=FALSE,main="Route B",xlab="",xlim=c(0,30))	;box()
abline(v=mean(xxx[xxx$route=="B",]$ttime),col='red',lwd=4)
hist(xxx[xxx$route=="C",]$ttime,las=1,freq=FALSE,main="Route C",xlab="",xlim=c(0,30))	;box()
abline(v=mean(xxx[xxx$route=="C",]$ttime),col='red',lwd=4)
hist(xxx[xxx$route=="D",]$ttime,las=1,freq=FALSE,main="Route D",xlab="",xlim=c(0,30))	;box()
abline(v=mean(xxx[xxx$route=="D",]$ttime),col='red',lwd=4)
mtext(side=2, "Relative frequency", line=0.5, outer=TRUE,cex=1.5)
mtext(side=1, "Travel time (minutes)", line=0.5, outer=TRUE,cex=1.5)

presPlot()
# PLOT OF CUMULATIVE PROBABILITY
hist(xxx[xxx$route=="B",]$ttime,las=1,freq=FALSE,main="Route B",
    xlab="",xlim=c(0,30))	;box()
par(new=TRUE)
xxx<- xxx[xxx$route=="B",]$ttime

xxx$tmp<- 1
xxx$bin<-NA
xxx[xxx$tt<=5,]$bin<-"0-5"
xxx[xxx$tt>5&xxx$tt<=10,]$bin<-"5-10"
xxx[xxx$tt>10&xxx$tt<=15,]$bin<-"10-15"
xxx[xxx$tt>15&xxx$tt<=20,]$bin<-"15-20"
xxx[xxx$tt>20&xxx$tt<=25,]$bin<-"25-20"
xxx[xxx$tt>25,]$bin<-"25+"
xxx$bin_f<-factor(xxx$bin, levels=c("0-5","5-10","10-15","15-20","25-20","25+"))



yy<- xxx[xxx$route=="B",]$ttime
yy<-yy[order(xxx$ttime),]
xxx$cumsum<- cumsum(yy$tmp)
xxx$cumprob<- xxx$cumsum/sum(yy$tmp)
yyy<-tapply(xxx$cumprob,xxx$bin_f,min)
cp<-tapply(xxx$cumprob,xxx$ttime,max)
# PLOT OF CUMULATIVE PROBABILITY
presPlot()
par(oma=c(0,2,0,2))
hist(xxx[xxx$route=="B",]$ttime,las=1,freq=FALSE,main="Route B",
    xlab="",xlim=c(0,30))	;box()
par(new=TRUE)
plot(cp~c(0:40),xlim=c(0,30),xaxt='n',yaxt='n',ylab="",,type='l',col="red",lwd=2)
axis(4,at=seq(0,1,0.2),las=1)
points(c(0,5,10,15,20,25),yyy,pch=19,col="red",cex=1.5)



barplot(diff(c(0,yyy,1))

xxx$tmp<-1

aggregate(ttime~route+leave, xxx, mean)

require(reshape2)

yy<-dcast(xxx,route~ttime, value.var="tmp", sum)
yyy<-yy[,-1]/400000



# figure of decision uncertainty

yy<-dcast(xxx,route+leave~"eval", value.var="ttime", mean)
yy$leave_c<- rep(c(1:4),4)
dev.new(width=10, height=6)
plot(eval~leave_c,yy,type='n',ylab="Expected travel time (minutes)",xaxt="n",xlab="Departure time",
	cex.lab=1.3,las=1)
axis(1, at=c(1,2,3,4), labels=c("4:00-4:30", "4:30-5:00","5:00-5:30","6:00-6:30"))
points(eval~leave_c,yy,subset=route=="A",type='b',pch="A",col="red",lwd=3,cex=1.5)
points(eval~leave_c,yy,subset=route=="B",type='b',pch="B",col="green",lwd=3,cex=1.5)
points(eval~leave_c,yy,subset=route=="C",type='b',pch="C",col="blue",lwd=3,cex=1.5)
points(eval~leave_c,yy,subset=route=="D",type='b',pch="D",col="black",lwd=3,cex=1.5)














	
	
