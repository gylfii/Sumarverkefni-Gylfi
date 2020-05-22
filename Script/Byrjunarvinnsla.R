library(dbplyr)
library(dplyr)
library(RMySQL)
library(tidyr)
library(MASS)
library(ggplot2)
library(gridExtra)

#lesum inn gögninn okkar, fyrst fyrir NOTA+ skoðað eins og hitt
hashAnswer <- read.csv('Data/hashAnswer.csv')
hashAnswer <- hashAnswer %>% subset(select=-c(X))
hashAnswer$hsta <- hashAnswer$hsta%>%as.character()

ans.glm <- glm(correct~fsfat,family = binomial(link="logit"),data=hashAnswer)
ans.glm2 <- glm(correct~fsfat+hsta,family = binomial(link="logit"),data=hashAnswer)
ans.glm3 <- glm(correct~fsfat*hsta,family = binomial(link="logit"),data=hashAnswer)

#næst fyrir þegar NOTA+ er horft á aðeins öðruvísi
hashAnswer2 <- read.csv('Data/hashAnswer2.csv')
hashAnswer2 <- hashAnswer2 %>% subset(select=-c(X))
hashAnswer2$hsta <- hashAnswer2$hsta%>%as.character()

ans2.glm <- glm(correct~fsfat,family = binomial(link="logit"),data=hashAnswer2)
ans2.glm2 <- glm(correct~fsfat+hsta,family = binomial(link="logit"),data=hashAnswer2)
ans2.glm3 <- glm(correct~fsfat*hsta,family = binomial(link="logit"),data=hashAnswer2)

#næst er að skoða fyrir þegar NOTA + er horft á miðað við fyrst skiptið í hverjum fyrirlestri
hashAnswer3 <- read.csv('Data/hashAnswer3.csv')
hashAnswer3 <- hashAnswer3 %>% subset(select=-c(X))
hashAnswer3$hsta <- hashAnswer3$hsta%>%as.character()

ans3.glm <- glm(correct~fsfat,family = binomial(link="logit"),data=hashAnswer3)
ans3.glm2 <- glm(correct~fsfat+hsta,family = binomial(link="logit"),data=hashAnswer3)
ans3.glm3 <- glm(correct~fsfat*hsta,family = binomial(link="logit"),data=hashAnswer3)

#Svo er gögninn öll, án NOTA+
hashAnswer4 <- read.csv('Data/hashAnswer4.csv')
hashAnswer4 <- hashAnswer4 %>% subset(select=-c(X))
hashAnswer4$hsta <- hashAnswer4$hsta%>%as.character()

ans4.glm <- glm(correct~fsfat,family = binomial(link="logit"),data=hashAnswer4)
ans4.glm2 <- glm(correct~fsfat+hsta,family = binomial(link="logit"),data=hashAnswer4)
ans4.glm3 <- glm(correct~fsfat*hsta,family = binomial(link="logit"),data=hashAnswer4)

?glm

#Teiknum fyrst aðeins gögninn með 
p11 <- ggplot(data=cbind(hashAnswer,pred=predict.glm(ans.glm,type = "response")),aes(x=fsfat,y=correct))+
  geom_point()+
  geom_line(aes(y=pred))+
  ggtitle("Answer1")
p12 <- ggplot(data=cbind(hashAnswer,pred=predict.glm(ans.glm2,type = "response")),aes(x=fsfat,y=correct,color=hsta))+
  geom_point()+
  geom_line(aes(y=pred))+
  ggtitle("Answer1")
p13 <- ggplot(data=cbind(hashAnswer,pred=predict.glm(ans.glm3,type = "response")),aes(x=fsfat,y=correct, color=hsta))+
  geom_point()+
  geom_line(aes(y=pred))+
  ggtitle("Answer1")

p21 <- ggplot(data=cbind(hashAnswer2,pred=predict.glm(ans2.glm,type = "response")),aes(x=fsfat,y=correct))+
  geom_point()+
  geom_line(aes(y=pred))+
  ggtitle("Answer2")
p22 <- ggplot(data=cbind(hashAnswer2,pred=predict.glm(ans2.glm2,type = "response")),aes(x=fsfat,y=correct,color=hsta))+
  geom_point()+
  geom_line(aes(y=pred))+
  ggtitle("Answer2")
p23 <- ggplot(data=cbind(hashAnswer2,pred=predict.glm(ans2.glm3,type = "response")),aes(x=fsfat,y=correct, color=hsta))+
  geom_point()+
  geom_line(aes(y=pred))+
  ggtitle("Answer2")

p31 <- ggplot(data=cbind(hashAnswer3,pred=predict.glm(ans3.glm,type = "response")),aes(x=fsfat,y=correct))+
  geom_point()+
  geom_line(aes(y=pred))+
  ggtitle("Answer3")
p32 <- ggplot(data=cbind(hashAnswer3,pred=predict.glm(ans3.glm2,type = "response")),aes(x=fsfat,y=correct,color=hsta))+
  geom_point()+
  geom_line(aes(y=pred))+
  ggtitle("Answer3")
p33 <- ggplot(data=cbind(hashAnswer3,pred=predict.glm(ans3.glm3,type = "response")),aes(x=fsfat,y=correct, color=hsta))+
  geom_point()+
  geom_line(aes(y=pred))+
  ggtitle("Answer3")

p41 <- ggplot(data=cbind(hashAnswer4,pred=predict.glm(ans4.glm,type = "response")),aes(x=fsfat,y=correct))+
  geom_point()+
  geom_line(aes(y=pred))+
  ggtitle("Answer4")
p42 <- ggplot(data=cbind(hashAnswer4,pred=predict.glm(ans4.glm2,type = "response")),aes(x=fsfat,y=correct,color=hsta))+
  geom_point()+
  geom_line(aes(y=pred))+
  ggtitle("Answer4")
p43 <- ggplot(data=cbind(hashAnswer4,pred=predict.glm(ans4.glm3,type = "response")),aes(x=fsfat,y=correct, color=hsta))+
  geom_point()+
  geom_line(aes(y=pred))+
  ggtitle("Answer4")

grid.arrange(p11, p21, nrow=1)
grid.arrange(p12, p22, nrow=1)
grid.arrange(p13, p23, nrow=1)

grid.arrange(p11, p21, p31, nrow=1)
grid.arrange(p12, p22, p32, nrow=1)
grid.arrange(p13, p23, p33, nrow=1)

grid.arrange(p11, p21, p31, p41, nrow=2)
grid.arrange(p12, p22, p32, p42, nrow=2)
grid.arrange(p13, p23, p33, p43, nrow=2)

hashAnswer %>% summarise(n_distinct(studentId))
hashAnswer %>% filter(fsfat>300) %>% summarise(n_distinct(studentId))

hashAnswer %>% group_by(lectureId) %>% summarise(n_distinct())

ggplot(data=hashAnswer)+geom_bar(aes(x=hsta,y=stat(prop),group=1))


summary(ans.glm)
summary(ans.glm2)
summary(ans.glm3)


glimpse(hashAnswer)
summary(hashAnswer)
