library(dbplyr)
library(dplyr)
library(RMySQL)
library(tidyr)
library(MASS)
library(ggplot2)
library(gridExtra)

#lesum inn gögninn okkar
hashAnswer <- read.csv('Data/hashAnswer4.csv')
hashAnswer <- hashAnswer %>% subset(select=-c(X))
hashAnswer$hsta <- hashAnswer$hsta%>%as.character()
hashAnswer$timeDif <- hashAnswer$timeDif/360


#Skoðum stuttlega þegar skorið er af gögnunum
hashAnswery150 <- hashAnswer%>% filter(fsfat<150)
hashAnswery200 <- hashAnswer%>% filter(fsfat<200)
hashAnswery250 <- hashAnswer%>% filter(fsfat<250)
hashAnswery300 <- hashAnswer%>% filter(fsfat<300)

ans.glm150 <- glm(correct~fsfat*hsta,family = binomial(link="logit"),data=hashAnswery150)
ans2.glm150 <- glm(correct~fsfat+timeDif,family = binomial(link = "logit"), data = hashAnswery150)
ans.glm200 <- glm(correct~fsfat*hsta,family = binomial(link="logit"),data=hashAnswery200)
ans.glm250 <- glm(correct~fsfat*hsta,family = binomial(link="logit"),data=hashAnswery250)
ans.glm300 <- glm(correct~fsfat*hsta,family = binomial(link="logit"),data=hashAnswery300)

summary(ans2.glm150)

p1 <- ggplot(data=cbind(hashAnswery150,pred=predict.glm(ans.glm150,type = "response")),aes(x=fsfat,y=correct, color=hsta))+
  geom_point()+
  geom_line(aes(y=pred))+
  ggtitle("Undir 150")+
  annotate("text",x=100,y=0.4,label= paste("fjoldi nemenda sem fóru lengra er",hashAnswer%>%
                                             filter(fsfat>150) %>% summarise(n_distinct(studentId)), sep = " " ))
p12 <-ggplot(data=cbind(hashAnswery150,pred=predict.glm(ans2.glm150,type = "response")),aes(x=fsfat,y=correct, color=hsta))+
  geom_point()+
  geom_line(aes(y=pred))+
  ggtitle("Undir 150")+
  annotate("text",x=100,y=0.4,label= paste("fjoldi nemenda sem fóru lengra er",hashAnswer%>%
                                             filter(fsfat>150) %>% summarise(n_distinct(studentId)), sep = " " ))
ggplot(hashAnswery150,aes(x=fsfat,y=timeDif))+geom_point()
p2 <- ggplot(data=cbind(hashAnswery200,pred=predict.glm(ans.glm200,type = "response")),aes(x=fsfat,y=correct, color=hsta))+
  geom_point()+
  geom_line(aes(y=pred))+
  ggtitle("Undir 200")+
  annotate("text",x=150,y=0.4,label= paste("fjoldi nemenda sem fóru lengra er",hashAnswer%>%
                                             filter(fsfat>200) %>% summarise(n_distinct(studentId)), sep = " " ))

p3 <- ggplot(data=cbind(hashAnswery250,pred=predict.glm(ans.glm250,type = "response")),aes(x=fsfat,y=correct, color=hsta))+
  geom_point()+
  geom_line(aes(y=pred))+
  ggtitle("Undir 250")+
  annotate("text",x=200,y=0.4,label= paste("fjoldi nemenda sem fóru lengra er",hashAnswer%>%
                                             filter(fsfat>250) %>% summarise(n_distinct(studentId)), sep = " " ))

p4 <- ggplot(data=cbind(hashAnswery300,pred=predict.glm(ans.glm300,type = "response")),aes(x=fsfat,y=correct, color=hsta))+
  geom_point()+
  geom_line(aes(y=pred))+
  ggtitle("Undir 300")+
  annotate("text",x=250,y=0.4,label= paste("fjoldi nemenda sem fóru lengra er",hashAnswer%>%
                                             filter(fsfat>300) %>% summarise(n_distinct(studentId)), sep = " " ))

grid.arrange(p1, p2, p3, p4, nrow= 2)


#skoða svo aðeins meðaltal eftir fsfta

p5 <- hashAnswer %>% group_by(fsfat,hsta) %>% filter(fsfat>=0) %>%
  summarise("med"=mean(correct),"fjold"=n_distinct(studentId)) %>%
  ggplot(aes(x=fsfat,y=med,color=hsta), show.legend=FALSE)+
  geom_point(show.legend = FALSE)+
  geom_smooth(show.legend = FALSE)
p5
p6 <- hashAnswer %>% group_by(fsfat,hsta) %>% filter(fsfat>=0) %>% 
  summarise("fjoldi"=n_distinct(studentId)) %>%
  ggplot(aes(x=fsfat,y=fjoldi,color=hsta))+
  geom_line(show.legend = FALSE)
p6

grid.arrange(p5,p6,nrow=1)

