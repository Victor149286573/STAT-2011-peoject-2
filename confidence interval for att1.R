#confidence interval for att1

#Confidence Interval = x+/-t(n-1, 1-£\/2)*(se(xbar))

#x=point estimation
#t(n-1, 1-alpha/2)=qt(1-alpha/2,n-1)
#se(xbar)=sqrt(var(xbar))
#var(xbar)=((N-n)/N)*sum((x-xbar)^2)/(n*(n-1))
#n=length(att1) ##n=ncol(att1)

x=att1_mean
n=nrow(att1)
c<-data.frame()
N=50
a=0.01
while(a<=0.1)
{
t=qt(1-a/2,n-1)
var_xbar=((N-n)/N)*sum((att1-x)^2)/n
se=sqrt(var_xbar)
b<-c(a,x-(t*se),x+(t*se))
c<-rbind(c,b)
a=a+0.01
}
names(c)<-c("significant level","lower bound","upper bound")

write.csv(x=c,file="att3.csv")