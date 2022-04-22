# Read the samples name and three atributes as name, att1, att2, att3

  library(readxl)
  
  data2 <- read_excel("D:/3 School/STAT 2011 project 2/Regression/data.xlsx")
  View(data2)
  names(data2)[5]<-"att1"
  names(data2)[6]<-"att2"
  names(data2)[7]<-"att3"
  
  gender<-as.vector(data2$gender)
  att1<-unlist(as.vector(data2[5]))#Balance Test Eyes Closed
  att2<-unlist(as.vector(data2[6]))
  att3<-unlist(as.vector(data2[7]))#Run Outdoors 1 km
  
#Regression for att1 and age
  age=data2$age
  fit0<-lm(att1~age)
  alpha <- 0.05
  df.new <- data.frame(age=(12:18))
  conf.dist <- predict(fit0, newdata = df.new, interval="confidence", level=1-alpha) 
  pred.dist <- predict(fit0, newdata = df.new, interval="prediction", level=1-alpha) 
  head(conf.dist)
  head(pred.dist)

#plot
  library(ggplot2)
  theme_set(theme_bw())
  pl <- ggplot(data2) + geom_point(aes(x=age, y=att1), size=2, colour="#993399") + 
    xlab("age") + ylab("Balance Test Eyes Closed")  
  print(pl)
  
#Regression  
  df.new[c("fit","lwr.conf", "upr.conf")] <- conf.dist
  df.new[c("lwr.pred", "upr.pred")] <- pred.dist[,2:3]
  pl +   
    geom_ribbon(data=df.new, aes(x=age, ymin=lwr.pred, ymax=upr.pred), alpha=0.1, inherit.aes=F, fill="blue") + 
    geom_ribbon(data=df.new, aes(x=age, ymin=lwr.conf, ymax=upr.conf), alpha=0.2, inherit.aes=F, fill="#339900") +  
    geom_line(data=df.new, aes(x=age, y=fit), colour="#339900", size=1)
  
  anova(fit0)
  
  