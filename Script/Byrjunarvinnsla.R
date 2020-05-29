library(dbplyr)
library(dplyr)
library(RMySQL)
library(tidyr)
library(MASS)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(modelr)
library(rlang)

#lesum inn gögninn okkar
hashAnswer <- read.csv('Data/hashAnswer4.csv')
hashAnswer <- hashAnswer %>% subset(select=-c(X))
hashAnswer$hsta <- hashAnswer$hsta%>%as.character()


#Skoðum stuttlega þegar skorið er af gögnunum


ans <- glm(correct~fsfat*hsta,family = binomial(link="logit"),data=hashAnswer)
ans2 <- glm(correct~fsvfat*hsta,family = binomial(link="logit"),data=hashAnswer)
ans3 <- glm(correct~(fsfat+fsvfat)*hsta,family = binomial(link="logit"),data=hashAnswer)

#Fall til að teikna upp logistic línu fyrir fsfat og limitað eftir fjolda svara
draw_by_limit <- function(hashA, limit) {
  hashAnswery <- hashA%>% filter(fsfat<limit)
  ans.glm <- glm(correct~fsfat*hsta,family = binomial(link="logit"),data=hashAnswery)
  p <- ggplot(data=cbind(hashAnswery,pred=predict.glm(ans.glm,type = "response")),aes(x=fsfat,y=correct, color=hsta))+
    geom_point()+
    geom_line(aes(y=pred))+
    ggtitle(paste("Undir ", limit,sep = ""))+
    annotate("text",x=limit-50,y=0.4,label= paste("fjoldi nemenda sem fóru lengra er",hashA%>%
                                               filter(fsfat>limit) %>% summarise(n_distinct(studentId)), sep = " " ))
  return(p)
}

p1 <- draw_by_limit(hashAnswer,150)
p2 <- draw_by_limit(hashAnswer,200)
p3 <- draw_by_limit(hashAnswer,250)
p4 <- draw_by_limit(hashAnswer,300)


#p12 <-ggplot(data=cbind(hashAnswery150,pred=predict.glm(ans2.glm150,type = "response")),aes(x=fsvfat,y=correct, color=hsta))+
#  geom_point()+
#   geom_line(aes(y=pred))+
#   ggtitle("Undir 150")
#   annotate("text",x=100,y=0.4,label= paste("fjoldi nemenda sem fóru lengra er",hashAnswer%>%
                                             # filter(fsfat>150) %>% summarise(n_distinct(studentId)), sep = " " ))
#grid.arrange(p1,p12,nrow=1)

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

p7 <- hashAnswer %>% group_by(fsvfat,hsta) %>%
  summarise("med"=mean(correct),"fjold"=n_distinct(studentId)) %>%
  ggplot(aes(x=fsvfat,y=med,color=hsta), show.legend=FALSE)+
  geom_line(show.legend = FALSE)+
  geom_smooth(show.legend = FALSE)

p7

ggplot(hashAnswer,aes(x=fsfat,y=fsvfat,color=hsta))+geom_point()
grid.arrange(p5,p6,nrow=1)


#p12 <-ggplot(data=cbind(hashAnswery150,pred=predict.glm(ans2.glm150,type = "response")),aes(x=fsvfat,y=correct, color=hsta))+
#  geom_point()+
#   geom_line(aes(y=pred))+
#   ggtitle("Undir 150")
#   annotate("text",x=100,y=0.4,label= paste("fjoldi nemenda sem fóru lengra er",hashAnswer%>%
# filter(fsfat>150) %>% summarise(n_distinct(studentId)), sep = " " ))

?add_predictions

hashAnswer %>% mutate(pred = predict.glm(ans2, type = "response")) %>%
  ggplot(aes(x = fsvfat, y = correct, color = hsta)) + 
  geom_point() + 
  geom_line(aes(y = pred))

#Drawing a different glm lines by student


fsvfat_model <- function(df) {
  glm(correct ~ fsvfat * hsta, family = binomial(link = "logit"), data = df)
}




#Býr til fall sem hægt er að teikna sitthvort fall fyrir hvert lectureId
Drawable_by_Id <- function(df, Id) {
  by_Id <- df %>% group_by(!!sym(Id)) %>%
    nest() %>% 
    filter(map_dbl(data, nrow) > 10)
  by_Id <- by_Id %>% 
    mutate(model = map(data, fsvfat_model))
  by_Id <- by_Id %>% 
    mutate(pred = map(model, predict.glm, type = "response"))
  Id_unested <- by_Id %>% 
    unnest(data, pred)
  return(Id_unested)
}
lecture_drawa <- Drawable_by_Id(hashAnswer, "lectureId")
student_unested <- Drawable_by_Id(hashAnswer, "studentId")

lecture_drawa %>%
  ggplot(aes(x = fsvfat, y = correct, color = hsta)) +
  geom_point() + 
  geom_line(aes(y = pred)) + 
  facet_wrap(vars(lectureId))

lecture_drawa$lectureId <- as.factor(lecture_drawa$lectureId)
lecture_drawa %>% filter(hsta == 0) %>%
  ggplot(aes(x = fsvfat, y = correct, color = lectureId)) +
  geom_point() + 
  geom_line(aes(y = pred))

student_unested %>% filter(hsta == 0, lectureId == 3214) %>%
  ggplot(aes(x = fsvfat, y = correct)) +
  geom_point() + 
  geom_line(aes(y = pred, group = studentId)) + 
  facet_wrap(vars(lectureId))