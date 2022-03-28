#confidence interval for att3

#Confidence Interval = x+/-t(n-1, 1-??/2)*(s/???n)

#x=point estimation
#t(n-1, 1-??/2)=qt(1-alpha/2,n-1)
#s=sd(att3)
#n=length(att3) ##n=ncol(att3)

x=att3_mean
n=nrow(att3)
t=qt(1-0.05/2,n-1)
s=sd(unlist(att3))

x-(t*s/sqrt(n))
x+(t*s/sqrt(n))