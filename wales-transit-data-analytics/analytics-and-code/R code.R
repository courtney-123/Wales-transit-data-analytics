library(tidyverse)
#setting working directory and uploading data
setwd("~/Documents")
sampledata<-read.csv("MMA_marketing_data_sample.csv",header=TRUE)
View(sampledata)

#removing missing and duplicate data
na.omit(sampledata)
unique(sampledata)
!complete.cases(sampledata)
sampledata[complete.cases(sampledata), ]
sampledata<-sampledata[complete.cases(sampledata), ]
print(sampledata)

#viewing the amount of variables
dim(sampledata)

#viewing the different types of variables 
str(sampledata)

#creating the graphs
library(ggplot2)

ggplot(sampledata)+
  geom_point(mapping=aes(x=emp.var.rate, y=k))

ggplot(sampledata)+
  geom_smooth(mapping=aes(x=age, y=emp.var.rate))

ggplot(sampledata)+
  geom_smooth(mapping=aes(x=cons.price.idx, y=emp.var.rate))

ggplot(sampledata)+
  geom_smooth(mapping=aes(x=cons.price.idx, y=euribor3m))

#linear regression
cor(sampledata$euribor3m, sampledata$emp.var.rate)
plot(sampledata$euribor3m, sampledata$emp.var.rate)

model<-lm(euribor3m~emp.var.rate, data=sampledata)
model

#linear model: y=3.530 + (1.076=x)

summary(model)
sigma(model)
summary(model)$r.squared

#predications
predict(model)
predict(model,data.frame(emp.var.rate=1))
predict(model,data.frame(emp.var.rate=-2.5))

rangeRev<-data.frame(emp.var.rate<-c(1,2,3,4,5))

predict(model,rangeRev)
