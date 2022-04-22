#Regression for att2 and age
#gender generation
eliminated<- subset(data2, data2$att2 > (Q[1] - 1.5*iqr) & data2$att2 < (Q[3]+1.5*iqr))
  data3<-eliminated
  gender<-(data3$gender)
  gender[gender=="male"]="Male"
  gender[gender=="female"]="Female"
  gender<-as.numeric(as.factor(gender))-1
    #female=0 male=1 to check whether sexual difference exists
  att2<-unlist(as.vector(data3[6]))#Run Outdoors 1 km
  age=data3$age
  fit20<-lm(att2~gender)
  alpha <- 0.05
  df.new <- data.frame(gender=c(0,1))
  conf.dist <- predict(fit20, newdata = df.new, interval="confidence", level=1-alpha) 
  pred.dist <- predict(fit20, newdata = df.new, interval="prediction", level=1-alpha) 
  head(conf.dist)
  head(pred.dist)
  
  #plot
  library(ggplot2)
  theme_set(theme_bw())
  pl <- ggplot(data3) + geom_point(aes(x=gender, y=att2), size=2, colour="#993399") + 
    xlab("gender") + ylab("att2")  
  print(pl)
  
  #Regression  
  df.new[c("fit","lwr.conf", "upr.conf")] <- conf.dist
  df.new[c("lwr.pred", "upr.pred")] <- pred.dist[,2:3]
  pl +   
    geom_ribbon(data=df.new, aes(x=gender, ymin=lwr.pred, ymax=upr.pred), alpha=0.1, inherit.aes=F, fill="blue") + 
    geom_ribbon(data=df.new, aes(x=gender, ymin=lwr.conf, ymax=upr.conf), alpha=0.2, inherit.aes=F, fill="#339900") +  
    geom_line(data=df.new, aes(x=gender, y=fit), colour="#339900", size=1)
  
anova(fit20)
