#Regression for att2 and age
eliminated<- subset(data2, data2$att2 > (Q[1] - 1.5*iqr) & data2$att2 < (Q[3]+1.5*iqr))
data2<-eliminated
age=data2$age
fit1<-lm(att2~age)
alpha <- 0.05
df.new <- data.frame(age=(12:18))
conf.dist <- predict(fit1, newdata = df.new, interval="confidence", level=1-alpha) 
pred.dist <- predict(fit1, newdata = df.new, interval="prediction", level=1-alpha) 
head(conf.dist)
head(pred.dist)

#plot
library(ggplot2)
theme_set(theme_bw())
pl <- ggplot(data2) + geom_point(aes(x=age, y=att2), size=2, colour="#993399") + 
  xlab("Age(in Year)") + ylab("Lung Capacity(in Lift)")  
print(pl)

boxplot(att2)
#Regression  
df.new[c("fit","lwr.conf", "upr.conf")] <- conf.dist
df.new[c("lwr.pred", "upr.pred")] <- pred.dist[,2:3]
pl +   
  geom_ribbon(data=df.new, aes(x=age, ymin=lwr.pred, ymax=upr.pred), alpha=0.1, inherit.aes=F, fill="blue") + 
  geom_ribbon(data=df.new, aes(x=age, ymin=lwr.conf, ymax=upr.conf), alpha=0.2, inherit.aes=F, fill="#339900") +  
  geom_line(data=df.new, aes(x=age, y=fit), colour="#339900", size=1)

fit1$coefficients

anova(fit1)