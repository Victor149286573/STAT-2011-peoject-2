#Regression for att2 and age
age=data2$age
fit2<-lm(att3~age)
alpha <- 0.05
df.new <- data.frame(age=(12:18))
conf.dist <- predict(fit2, newdata = df.new, interval="confidence", level=1-alpha) 
pred.dist <- predict(fit2, newdata = df.new, interval="prediction", level=1-alpha) 
head(conf.dist)
head(pred.dist)

#plot
library(ggplot2)
theme_set(theme_bw())
pl <- ggplot(data2) + geom_point(aes(x=age, y=att3), size=2, colour="#993399") + 
  xlab("age") + ylab("Run Outdoors 1 km")  
print(pl)

#Regression  
df.new[c("fit","lwr.conf", "upr.conf")] <- conf.dist
df.new[c("lwr.pred", "upr.pred")] <- pred.dist[,2:3]
pl +   
  geom_ribbon(data=df.new, aes(x=age, ymin=lwr.pred, ymax=upr.pred), alpha=0.1, inherit.aes=F, fill="blue") + 
  geom_ribbon(data=df.new, aes(x=age, ymin=lwr.conf, ymax=upr.conf), alpha=0.2, inherit.aes=F, fill="#339900") +  
  geom_line(data=df.new, aes(x=age, y=fit), colour="#339900", size=1)


anova(fit2)