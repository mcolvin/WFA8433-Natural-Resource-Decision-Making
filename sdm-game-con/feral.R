

# feral cat model


N<- matrix(0,50,3)
N[1,]<- 1000
r<- c(0.03,0.3,0.3)
k<- c(5000,5000,5000)
m<-c(1,1,1.2)

d<- data.frame(current= round(runif(10000,0,5000),0),
	d=sample(c(1:3),10000,replace=TRUE))
nxt1<-current + r[1]*current * rnorm(1,1,0.8)
nxt2<-current + r[2]*current*(1-(current/k[2])) * rnorm(1,1,0.8)
nxt3<-current + r[3]*current*(1-(current/k[3])^(m[3]-1)) * rnorm(1,1,0.8)
w<- c(1,4,2)
w<-w/sum(w)
N_true<- nxt1*w[1] + nxt2*w[2] + nxt3*w[3]












# DYNAMICS
for(i in 2:50)
	{
	N[i,1]<-N[i-1,1] + r[1]*N[i-1,1] * rnorm(1,1,0.8)
	N[i,2]<-N[i-1,2] + r[2]*N[i-1,2]*(1-(N[i-1,2]/k[2])) * rnorm(1,1,0.8)
	N[i,3]<-N[i-1,3] + r[3]*N[i-1,3]*(1-(N[i-1,3]/k[3])^(m[3]-1)) * rnorm(1,1,0.8)
	}

matplot(N,type='b')
w<- c(1,4,2)
w<-w/sum(w)
N_true<- N[,1]*w[1] + N[,2]*w[2] + N[,3]*w[3]

plot(N_true)

	
	