#confidence interval for att2

#Confidence Interval = x+/-t(n-1, 1-??/2)*(s/???n)

#x=point estimation
#t(n-1, 1-??/2)=qt(1-alpha/2,n-1)
#s=sd(att2)
#n=length(att2) ##n=ncol(att2)

x=att2_mean
n=nrow(att2)
t=qt(1-0.05/2,n-1)
s=sd(unlist(att2))

x-(t*s/sqrt(n))
x+(t*s/sqrt(n))