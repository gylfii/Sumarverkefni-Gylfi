library(dbplyr)
library(dplyr)
library(RMySQL)
library(tidyr)
library(MASS)
library(ggplot2)
library(hash)

#lesum inn gögninn okkar
hashAnswer <- read.csv('Data/hashAnswer.csv')
hashAnswer <- hashAnswer %>% subset(select=-c(X))
hashAnswer$hsta <- hashAnswer$hsta%>%as.character()

ans.glm <- glm(correct~fsfat,family = binomial(link="logit"),data=hashAnswer)
ans.glm2 <- glm(correct~fsfat+hsta,family = binomial(link="logit"),data=hashAnswer)
ans.glm3 <- glm(correct~fsfat*hsta,family = binomial(link="logit"),data=hashAnswer)


?glm

#Teiknum fyrst aðeins gögninn með 
ggplot(data=cbind(hashAnswer,pred=predict.glm(ans.glm,type = "response")),aes(x=fsfat,y=correct))+
  geom_point()+
  geom_line(aes(y=pred))
ggplot(data=cbind(hashAnswer,pred=predict.glm(ans.glm2,type = "response")),aes(x=fsfat,y=correct,color=hsta))+
  geom_point()+
  geom_line(aes(y=pred))
ggplot(data=cbind(hashAnswer,pred=predict.glm(ans.glm3,type = "response")),aes(x=fsfat,y=correct, color=hsta))+
  geom_point()+
  geom_line(aes(y=pred))+
  xlim(0,80)


summary(ans.glm)
summary(ans.glm2)
summary(ans.glm3)


glimpse(hashAnswer)
summary(hashAnswer)
