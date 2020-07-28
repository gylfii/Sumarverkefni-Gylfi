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
library(lme4)
library(ggthemes)
library(ggpubr)

theme_set(theme_tufte() +
            theme(panel.border = element_rect('black', fill = NA),
                  text = element_text(size = 14),
                  legend.text=element_text(size=14),
                  axis.text=element_text(size=14),
                  axis.title = element_text(size = 14),
                  plot.title = element_text(hjust = 0.5)))


#?glmer
#lesum inn gögninn okkar
hashAnswer <- read.csv('Data/hashAnswer4.csv')
hashAnswer <- hashAnswer %>% subset(select=-c(X))
hashAnswer$hsta <- hashAnswer$hsta%>%as.character()
hashAnswer$lectureId <- hashAnswer$lectureId %>% as.factor()
hashAnswer$studentId <- hashAnswer$studentId %>% as.factor()

#Skoðum stuttlega þegar skorið er af gögnunum


ans <- glm(correct~fsfat*hsta,family = binomial(link="logit"),data=hashAnswer)
ans2 <- glm(correct~fsvfat*hsta,family = binomial(link="logit"),data=hashAnswer)
ans3 <- glm(correct~(fsfat+fsvfat)*hsta,family = binomial(link="logit"),data=hashAnswer)


#Fall til að teikna upp logistic línu fyrir fsfat og limitað eftir fjolda svara

draw_by_limit <- function(hashA, limit) {
  hashAnswery <- hashA%>% filter(fsfat<limit)
  ans.glm <- glm(correct~fsfat*hsta,family = binomial(link="logit"),data=hashAnswery)
  dlab <- tibble(
    fsfat = -Inf,
    correct = 0.4,
    label = paste("fjoldi nemenda sem \nfóru lengra er",hashA%>%
                    filter(fsfat>limit) %>% summarise(n_distinct(studentId)), sep = " " )
  )
  p <- ggplot(data=cbind(hashAnswery,pred=predict.glm(ans.glm,type = "response")),aes(x=fsfat,y=correct))+
    geom_point(aes(color=hsta))+
    geom_line(aes(y=pred, color=hsta))+
    ggtitle(paste("Undir ", limit,sep = ""))+
    geom_text(aes(label = label), data = dlab, hjust = "left")
  return(p)
}
draw_by_limit_sv <- function(hashA, limit) {
  hashAnswery <- hashA%>% filter(fsfat<limit)
  ans.glm <- glm(correct~fsvfatu*hsta,family = binomial(link="logit"),data=hashAnswery)
  dlab <- tibble(
    fsvfatu = -Inf,
    correct = 0.4,
    label = paste("fjoldi nemenda sem \nfóru lengra er",hashA%>%
                    filter(fsfat>limit) %>% summarise(n_distinct(studentId)), sep = " " )
  )
  p <- ggplot(data=cbind(hashAnswery,pred=predict.glm(ans.glm,type = "response")),aes(x=fsvfatu,y=correct))+
    geom_point(aes(color = hsta))+
    geom_line(aes(y=pred, color = hsta))+
    ggtitle(paste("Undir ", limit,sep = "")) +
    geom_text(aes(label = label), data = dlab, hjust = "left")
  return(p)
}

draw_range_by_fun <- function(hashA, Fun){
  p00 <- Fun(hashAnswer, 50)
  p01 <- Fun(hashAnswer, 100)
  p1 <- Fun(hashAnswer,150)
  p2 <- Fun(hashAnswer,200)
  p3 <- Fun(hashAnswer,250)
  p4 <- Fun(hashAnswer,300)
  
  return(grid.arrange(p00, p01, p1, p2, p3, p4, nrow= 2))
}
p1 <- draw_range_by_fun(hashAnswer, draw_by_limit)
p2 <- draw_range_by_fun(hashAnswer, draw_by_limit_sv)

ggsave("Img/plot1.png", plot = p1, width = 15, height = 10)
ggsave("Img/plot2.png", plot = p2, width = 15, height = 10)


make_limit_frame <- function(hashA, limit) {
  hashlim <- hashA %>% filter(fsfat < limit)
  ans <- glm(correct ~ fsfat * hsta, family = binomial(link = "logit"), data = hashlim)
  hashlim <- hashlim %>% mutate(pred = predict.glm(ans, type = "response")) %>% filter(hsta == 0) %>% data.frame(term = limit)
  return(hashlim)
}

draw_at_once <- function(hashA) {
  mylist <- list()
  hasht <- make_limit_frame(hashA, 400)
  for (i in 1:6) {
    hasht <- make_limit_frame(hashA, 50*i) %>% add_row(hasht)
  }
  hasht$term <- as.factor(hasht$term)
  p <- hasht %>% filter(hsta == 0) %>%
    ggplot(aes(x = fsfat, y = correct)) +
    geom_point() +
    geom_line(aes(y = pred, color = term))
  return(p)
}

p <- draw_at_once(hashAnswer)
ggsave("Img/plot3.png", plot = p)



draw_by_limit_sv(hashAnswer, 100)



testlist <- list()

testlist[1] <- 3
testlist[2] <- c(1,3)
testlist[3] <- list(data.frame(x = 1:50, y = 51:100))
?ggsave
# p00 <- draw_by_limit(hashAnswer, 50)
# p01 <- draw_by_limit(hashAnswer, 100)
# p1 <- draw_by_limit(hashAnswer,150)
# p2 <- draw_by_limit(hashAnswer,200)
# p3 <- draw_by_limit(hashAnswer,250)
# p4 <- draw_by_limit(hashAnswer,300)


#p12 <-ggplot(data=cbind(hashAnswery150,pred=predict.glm(ans2.glm150,type = "response")),aes(x=fsvfat,y=correct, color=hsta))+
#  geom_point()+
#   geom_line(aes(y=pred))+
#   ggtitle("Undir 150")
#   annotate("text",x=100,y=0.4,label= paste("fjoldi nemenda sem fóru lengra er",hashAnswer%>%
                                             # filter(fsfat>150) %>% summarise(n_distinct(studentId)), sep = " " ))
#grid.arrange(p1,p12,nrow=1)

grid.arrange(p00, p01, p1, p2, p3, p4, nrow= 2)


#skoða svo aðeins meðaltal eftir fsfta

p5 <- hashAnswer %>% group_by(fsfat,hsta) %>% filter(fsfat>=0) %>%
  summarise("med"=mean(correct),"fjold"=n_distinct(studentId)) %>%
  ggplot(aes(x=fsfat,y=med,color=hsta))+
  geom_point()+
  geom_smooth()
p5

ggsave("Imgsimplify/plotbymean.png", p5, width = 10, height = 10)

p6 <- hashAnswer %>% group_by(fsfat,hsta) %>% filter(fsfat>=0) %>% 
  summarise("fjoldi"=n()) %>%
  ggplot(aes(x=fsfat,y=fjoldi,color=hsta))+
  geom_line()
p6
ggsave("Imgsimplify/plotbyamount.png", p6, width = 10, height = 10)


p7 <- hashAnswer %>% group_by(fsvfatu,hsta) %>%
  summarise("med"=mean(correct),"fjold"=n_distinct(studentId)) %>%
  ggplot(aes(x=fsvfatu,y=med,color=hsta), show.legend=FALSE)+
  geom_point(show.legend = FALSE)+
  geom_smooth(show.legend = FALSE)

p8 <- hashAnswer %>% group_by(fsvfatu,hsta) %>% 
  summarise("fjoldi"=n_distinct(studentId)) %>%
  ggplot(aes(x=fsvfatu,y=fjoldi,color=hsta))+
  geom_line(show.legend = FALSE)

ps <- grid.arrange(p5, p6, p7, p8, nrow = 2)

mean_plot_by_limit <- function(hashA, limit) {
  hashl <- hashA %>% filter(fsfat < limit)
  p5 <- hashl %>% group_by(fsfat,hsta) %>% 
    summarise("med"=mean(correct),"fjold"=n_distinct(studentId)) %>%
    ggplot(aes(x=fsfat,y=med,color=hsta), show.legend=FALSE)+
    geom_point(show.legend = FALSE)+
    geom_smooth(show.legend = FALSE)
  p6 <- hashl %>% group_by(fsfat,hsta) %>% 
    summarise("fjoldi"=n_distinct(studentId)) %>%
    ggplot(aes(x=fsfat,y=fjoldi,color=hsta))+
    geom_line(show.legend = FALSE)
  
  p7 <- hashl %>% group_by(fsvfatu,hsta) %>%
    summarise("med"=mean(correct),"fjold"=n_distinct(studentId)) %>%
    ggplot(aes(x=fsvfatu,y=med,color=hsta), show.legend=FALSE)+
    geom_point(show.legend = FALSE)+
    geom_smooth(show.legend = FALSE)
  
  p8 <- hashl %>% group_by(fsvfatu,hsta) %>% 
    summarise("fjoldi"=n_distinct(studentId)) %>%
    ggplot(aes(x=fsvfatu,y=fjoldi,color=hsta))+
    geom_line(show.legend = FALSE)
  
  grid.arrange(p5, p6, p7, p8, nrow = 2, top = paste0("undir ", limit)) %>% return()
}

for (i in 1:6) {
  tpath <- paste0('Img/plotbymean', i*50, '.png')
  ps <- mean_plot_by_limit(hashAnswer, i*50)
  ggsave(tpath, ps, width = 10, height = 10)
}

ggsave('Img/plotbymean.png', ps, width = 10, height = 10)

mean_plot_by_limit(hashAnswer, 100)
?grid.arrange

hashAnswer %>% summarise(n_distinct(studentId))
hashAnswer %>% filter(fsvfat > 60) %>% summarise(n_distinct(studentId))


ggplot(hashAnswer,aes(x=fsfat,y=fsvfat,color=hsta))+geom_point()
grid.arrange(p5, p7, nrow=1)


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


#fall til að save-a myndina limitað eftir vali
save_by_limit <- function(hashA, limit) {
  hashl <- hashA %>% filter(fsfat < limit)
  p5 <- hashl %>% group_by(fsfat,hsta) %>% 
    summarise("med"=mean(correct),"fjold"=n_distinct(studentId)) %>%
    ggplot(aes(x=fsfat,y=med,color=hsta))+
    geom_point()+
    geom_smooth() +
    ggtitle(paste0("Undir ", limit)) + 
    scale_colour_brewer(type = "qual", palette = "Set1")+
    theme(legend.position = 'bottom') +
    labs(x = "Fjöldi spurninga fram að þessu", y = "Meðaltal")
  tpath <- paste0('Imgsimplify/plotbymean', limit, '.png')
  ggsave(tpath, p5, width = 10, height = 10)
}

save_by_limit(hashAnswer, 50)
save_by_limit(hashAnswer, 100)


# Teiknum myndirnar fyrir fsvfat á móti correct, með einni línu fyrir hvert Id

ans4 <- glm(correct ~ fsvfat*hsta*lectureId, family = binomial(link = "logit"), data = hashAnswer)

p4fsv <- hashAnswer %>% mutate(pred = predict.glm(ans4, type = "response")) %>%
  filter(hsta == 0) %>%
  ggplot(aes(x = fsvfat, y = correct, color = lectureId)) + 
  geom_point() + 
  geom_line(aes(y = pred))


ans4f <- glm(correct ~ fsfat*hsta*lectureId, family = binomial(link = "logit"), data = hashAnswer)

p4fsf <- hashAnswer %>% mutate(pred = predict.glm(ans4f, type = "response")) %>%
  filter(hsta == 0) %>%
  ggplot(aes(x = fsfat, y = correct, color = lectureId)) + 
  geom_point() + 
  geom_line(aes(y = pred))

ps <- grid.arrange(p4fsv, p4fsf, nrow = 1)

ggsave('Img/plotbylectureId.png', ps, width = 15, height = 7.5)
#This here is a drastic over complication of the subject. Instead it is best to simply use glm directly
#This actually is the best thing for studentId work, as that is to big to put directly into glm

fsvfat_model <- function(df) {
  glm(correct ~ fsvfat * hsta, family = binomial(link = "logit"), data = df)
}
fsfat_model <- function(df) {
  glm(correct ~ fsfat * hsta, family = binomial(link = "logit"), data = df)
}

#Býr til fall sem hægt er að teikna sitthvort fall fyrir hvert lectureId
Drawable_by_Id <- function(df, Id, fun) {
  by_Id <- df %>% group_by(!!sym(Id)) %>%
    nest() %>%
    filter(map_dbl(data, nrow) > 10)
  by_Id <- by_Id %>%
    mutate(model = map(data, fun))
  by_Id <- by_Id %>%
    mutate(pred = map(model, predict.glm, type = "response"))
  Id_unested <- by_Id %>%
    unnest(data, pred)
  return(Id_unested)
}
student_unested1 <- Drawable_by_Id(hashAnswer, "studentId", fsfat_model)
student_unested <- Drawable_by_Id(hashAnswer, "studentId", fsvfat_model)

hashAnswer %>% group_by(lectureId) %>% summarise(n_distinct(studentId))

p1 <- student_unested %>% filter(lectureId == 3082 & hsta == 0) %>%
  ggplot(aes(x = fsvfat, y = correct)) +
  geom_point() +
  geom_line(aes(y = pred, group = studentId)) 
p2 <- student_unested1 %>% filter(lectureId == 3082 & hsta == 0) %>%
  ggplot(aes(x = fsfat, y = correct)) +
  geom_point() +
  geom_line(aes(y = pred, group = studentId))

ps <- grid.arrange(p2, p1, nrow = 1)

ggsave('Img/plotbystudentId.png', ps, width = 20, height = 10)


#skoða aðeins plotByMean aftur, nema þetta sinn með tilliti til hvern fyrirlesturs fyrir sig


# hashAnswer %>% group_by(fsfat,hsta) %>% filter(fsfat>=0) %>%
#   summarise("med"=mean(correct),"fjold"=n_distinct(studentId)) %>%
#   ggplot(aes(x=fsfat,y=med,color=hsta))+
#   geom_point()+
#   geom_smooth()

#Teikningar gagnasett
dfmean <- hashAnswer %>% group_by(fsfat, hsta, lectureId) %>% filter(fsfat <50) %>%
  summarise("med"=mean(correct), "fjoldi" = n())

max(dfmean)

dfmean %>% ggplot(aes(x = fsfat, fill = hsta)) +
  geom_histogram(position = "dodge2", binwidth = 1)

dfmean %>% filter(lectureId == "3082") %>%
  ggplot() +
  geom_histogram(aes(x = fsfat, y = fjoldi, fill = hsta), stat = "identity", position = "dodge2", show.legend = F)

?grid.arrange

unique(dfmean$lectureId)

for (lId in unique(dfmean$lectureId)) {
  #filtering out for this lecture
  dftemp <- dfmean %>% filter(lectureId == lId)
  #drawing the cool graph
  p1t <- dftemp %>%
    ggplot(aes(x = fsfat, y = med, color = hsta))+
    geom_point(show.legend = F) + 
    geom_smooth(show.legend = F)
  #Drawing the histogram
  p2t <- dftemp %>% ggplot(aes(x = fsfat, y = fjoldi, fill = hsta)) +
    geom_bar(stat = "identity", position = "dodge2", show.legend = F)
  #combining the 2
  pcom <- grid.arrange(p1t, p2t, nrow = 1, top = paste0('fyrirlestur ', lId))
  ggsave(paste0('Img/lecmean', lId, '.png'), pcom, width = 20, height = 10)
  
}


#teikna meðaltals myndirnar byggt ofaná hlutföllin
# hashAnswer$hluta <- hashAnswer$hluta %>% as.factor()
hashAnswer$hluta2 <- cut_interval(hashAnswer$hluta, n = 5)

#Mean fyrir hluta og alls konar myndir með því
dfmean2 <- hashAnswer %>% group_by(fsfat, hluta2) %>% filter(fsfat < 50 & hsta == "0") %>%
  summarise("med" = mean(correct), "fjoldi" = n())

dfmean3 <- hashAnswer %>% filter(fsfat < 50) %>% group_by(fsfat, hsta) %>%
  summarise("med" = mean(correct), fjoldi = n())

dfmean4 <- hashAnswer %>% filter(fsfat < 100 & hsta == "0") %>% group_by(fsfat, hluta2) %>%
  summarise("med" = mean(correct), "fjoldi" = n())

dfmean5 <- hashAnswer %>% filter(fsfat < 100) %>% group_by(fsfat, hsta) %>%
  summarise("med" = mean(correct), fjoldi = n())

cor(hashTest2$fsfat, hashTest2$hluta)

ggplot(dfmean3, aes(x = fsfat, y = med, color = hsta)) +
  geom_point() +
  geom_smooth() +
  geom_smooth(data = dfmean2, aes(color = hluta2), se = F)

p2 <- dfmean3 %>% filter(hsta == 0) %>% ggplot(aes(x = fsfat, y = med)) +
  geom_point() +
  geom_smooth() +
  geom_smooth(data = dfmean2 %>% filter(fjoldi > 1), aes(color = hluta2), se = F) +
  coord_cartesian(ylim = c(yminim, ymaxim)) + 
  scale_colour_brewer(type = "qual", palette = "Set1")+
  theme(legend.position = 'bottom') +
  labs(x = "Fjöldi spurninga fram að þessu", y = "Meðaltal")

p2
p1 <- dfmean2 %>% ggplot(aes(x = fsfat, y = fjoldi, color = hluta2)) +
  geom_line(show.legend = F) 

p2 <- dfmean3 %>% filter(hsta == 0) %>% ggplot(aes(x = fsfat, y = fjoldi)) +
  geom_line()

grid.arrange(p1, p2, nrow = 1)

p1 <- dfmean2 %>% ggplot(aes(x = fsfat, y = med, color = hluta2)) + 
  geom_point() +
  geom_smooth(se = F) +
  scale_colour_brewer(type = "qual", palette = "Set1")+
  theme(legend.position = 'bottom') +
  labs(x = "Fjöldi spurninga fram að þessu", y = "Meðaltal")

p3 <- dfmean2 %>% filter(fjoldi > 1) %>% ggplot(aes(x = fsfat, y = med, color = hluta2)) +
  geom_point(aes()) +
  geom_smooth(se = F) +
  scale_colour_brewer(type = "qual", palette = "Set1")+
  theme(legend.position = 'bottom') +
  labs(x = "Fjöldi spurninga fram að þessu", y = "Meðaltal")
p3
dfmean4 %>% filter(fjoldi > 5) %>% ggplot(aes(x = fsfat, y = med, color = hluta2)) +
  geom_point() +
  geom_smooth(se = F) +
  labs(x = "Fjöldi spurninga fram að þessu", y = "Meðaltal")
yminim <- min(dfmean3$med)
ymaxim <- max(dfmean3$med)

yminim2 <- min(dfmean5$med)
ymaxim2 <- max(dfmean5$med)

p4 <- dfmean5 %>% filter(hsta == 0) %>% ggplot(aes(x = fsfat, y = med)) +
  geom_point() +
  geom_smooth() +
  geom_smooth(data = dfmean4 %>% filter(fjoldi > 1), aes(color = hluta2), se = F) +
  coord_cartesian(ylim = c(yminim2, ymaxim2)) +
  scale_colour_brewer(type = "qual", palette = "Set1")+
  theme(legend.position = 'bottom') +
  labs(x = "Fjöldi spurninga fram að þessu", y = "Meðaltal")
p4

ggsave('Img/meanbyhlutfall.png', p1, width = 10, height = 10)
ggsave('Img/meabwhsbyhlutfall.png', p2, width = 10, height = 10)
ggsave('Img/meanbyhlutfallLim.png', p3, width = 10, height = 10)
ggsave('Img/meabwhsbyhlutfall100.png', p4, width = 10, height = 10)

?geom_smooth






hashTest3 %>% ggplot(aes(x = hluta2, color = hsta)) +
  geom_bar(position = "dodge2")

dfmean2 %>% ggplot(aes(x = fsfat, y = med, color = hluta2)) + 
  #geom_point() +
  geom_smooth(se = F) + 
  geom_text(aes(label = fjoldi))

?geom_text

dfmean2 %>% ggplot(aes(x = fsfat, y = med, color = hluta2)) + 
  geom_point(aes(size = fjoldi)) +
  geom_smooth(se = F)  
