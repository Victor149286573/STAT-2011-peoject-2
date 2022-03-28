# Read the sample's name and three atributes as name, att1, att2, att3

library(readxl)

sample <- read_excel("simple random sampling1.xlsx")     #View(simple_random_sampling1)

#rename

names(sample)[5]<-"att1"
names(sample)[6]<-"att2"
names(sample)[7]<-"att3"

att1<-as.vector(sample[5])
att2<-as.vector(sample[6])
att3<-as.vector(sample[7])