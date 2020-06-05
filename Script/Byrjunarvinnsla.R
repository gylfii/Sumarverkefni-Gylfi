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

hashAnswer <- read.csv('Data/hashAnswer4.csv')
hashAnswer <- hashAnswer %>% subset(select=-c(X))
hashAnswer$hsta <- hashAnswer$hsta%>%as.character()
hashAnswer$lectureId <- hashAnswer$lectureId %>% as.factor()
hashAnswer$studentId <- hashAnswer$studentId %>% as.factor()
hashAnswer$nicc <- hashAnswer$nicc %>% as.factor()
hashAnswer$fsfat <- hashAnswer$fsfat/10
hashAnswer$fsvfatu <- hashAnswer$fsvfatu/10

#Skoðum stuttlega þegar skorið er af gögnunum

hashTest <- hashAnswer %>% group_by(studentId) %>% mutate("count" = n()) %>%
  filter(count > 7 & fsfat < 5)
hashAnswer %>% group_by(lectureId) %>% summarise(n_distinct(studentId))




ans <- glmer(correct ~ fsvfatu*hsta + lectureId + (1 | studentId), family = binomial(link = "logit"), data = hashTest, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
ans2 <- glmer(correct ~ fsfat*hsta + lectureId + (1 | studentId), family = binomial(link = "logit"), data = hashTest, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
ans3 <- glmer(correct ~ fsvfatu*hsta + nicc + lectureId + (1 | studentId), family = binomial(link = "logit"), data = hashTest, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(ans)
summary(ans2)
summary(ans3)
predict(ans)
predict.glm(ans, type = "response")
hashTest$pred <- predict(ans, type = "response")
hashTest$pred2 <- predict(ans2, type = "response")
hashTest$pred3 <- predict(ans3, type = "response")


p1 <- hashTest %>% filter(hsta == "0") %>% ggplot(aes(x = fsvfatu, y = correct, group = studentId), ) +
  geom_point() +
  geom_line(aes(y = pred)) + 
  theme(legend.position = "none") +
  facet_wrap(vars(lectureId))
p1
p2 <- hashTest %>% filter(lectureId == "3082" & hsta == "1") %>% ggplot(aes(x = fsvfatu, y = correct, group = studentId), ) +
  geom_point() +
  geom_line(aes(y = pred)) + 
  theme(legend.position = "none") 

p5 <- hashTest %>% filter(lectureId == "3082" & studentId %in% c("18788")) %>% 
  ggplot(aes(x = fsvfatu, y = correct, color = hsta), ) +
  geom_point() +
  geom_line(aes(y = pred3)) 
  facet_wrap(vars(nicc))
p5
grid.arrange(p1, p2, nrow = 1)
