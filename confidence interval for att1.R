#confidence interval for att1

#Confidence Interval = x+/-t(n-1, 1-£\/2)*(s/¡Ôn)

#x=point estimation
#t(n-1, 1-£\/2)=qt(1-alpha/2,n-1)
#s=sd(att1)
#n=length(att1) ##n=ncol(att1)

x=att1_mean
n=nrow(att1)
t=qt(1-0.05/2,n-1)
s=sd(unlist(att1))

x-(t*s/sqrt(n))
x+(t*s/sqrt(n))