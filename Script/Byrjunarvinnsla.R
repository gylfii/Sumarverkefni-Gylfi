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
library(kableExtra)

hashAnswer <- read.csv('Data/hashAnswer4.csv')
hashAnswer <- hashAnswer %>% subset(select=-c(X))
hashAnswer$hsta <- hashAnswer$hsta%>%as.character()
hashAnswer$lectureId <- hashAnswer$lectureId %>% as.factor()
hashAnswer$studentId <- hashAnswer$studentId %>% as.factor()
hashAnswer$nicc <- hashAnswer$nicc %>% as.factor()
hashAnswer$fsfat <- hashAnswer$fsfat/10
hashAnswer$fsvfatu <- hashAnswer$fsvfatu/10
hashAnswer$gpow2 <- as.factor(floor(log2(hashAnswer$gpow)))


tmp <- unique(hashAnswer$lectureId)[-1]
tmp2 <- unique(hashAnswer$lectureId)
#Skoðum stuttlega þegar skorið er af gögnunum


hashTest <- hashAnswer %>% group_by(studentId) %>% mutate("count" = n()) %>%
  filter(count > 7 & fsfat < 5)
hashAnswer %>% group_by(lectureId) %>% summarise(n_distinct(studentId))




ans <- glmer(correct ~ fsvfatu*hsta + nicc + lectureId + (1 | studentId), family = binomial(link = "logit"), data = hashTest, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
ans2 <- glmer(correct ~ fsfat*hsta + nicc + gpow2 + lectureId + (1 | studentId), family = binomial(link = "logit"), data = hashTest, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
ans3 <- glmer(correct ~ fsvfatu*hsta + nicc + gpow2 + lectureId + (1 | studentId), family = binomial(link = "logit"), data = hashTest, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e6)))
broom::tidy(ans)
summary(ans)
summary(ans2)
summary(ans3)
predict(ans)
predict.glm(ans, type = "response")
hashTest$pred <- predict(ans, type = "response")
hashTest$pred2 <- predict(ans2, type = "response")
hashTest$pred3 <- predict(ans3, type = "response")


p1 <- hashTest %>% filter(hsta == "0" & lectureId == "3082") %>% ggplot(aes(x = fsvfatu, y = correct, group = studentId), ) +
  geom_point() +
  geom_line(aes(y = pred)) + 
  theme(legend.position = "none") 
  facet_wrap(vars(lectureId))
p1
p2 <- hashTest %>% filter(lectureId == "3082" & hsta == "1") %>% ggplot(aes(x = fsvfatu, y = correct, group = studentId), ) +
  geom_point() +
  geom_line(aes(y = pred)) + 
  theme(legend.position = "none") 

p3 <- hashTest %>% filter(hsta == "0" & lectureId == "3082") %>% ggplot(aes(x = fsfat, y = correct, group = studentId), ) +
  geom_point() +
  geom_line(aes(y = pred2)) + 
  theme(legend.position = "none") 

p4 <- hashTest %>% filter(lectureId == "3082" & hsta == "1") %>% ggplot(aes(x = fsfat, y = correct, group = studentId), ) +
  geom_point() +
  geom_line(aes(y = pred2)) + 
  theme(legend.position = "none") 

p5 <- hashTest %>% filter(lectureId == "3082" & studentId %in% c("18788")) %>% 
  ggplot(aes(x = fsvfatu, y = correct, color = hsta), ) +
  geom_point() +
  geom_line(aes(y = pred3)) 
  facet_wrap(vars(nicc))
p5
grid.arrange(p1, p2, nrow = 1)
grid.arrange(p3, p4, nrow = 1)


#Skoðum aðeins hlutföll svara eftir limit
hashAnswer %>% group_by(lectureId) %>% summarise("FY50" = sum(fsfat >= 5), "FY100" = sum(fsfat >= 10), 
                                                 "FY150" = sum(fsfat >= 15), "FY200" = sum(fsfat >= 20), 
                                                 "FY250" = sum(fsfat >= 25), "FY300" = sum(fsfat >= 30))
hashAnswer %>% group_by(lectureId) %>% summarise("HY50" = mean(fsfat >= 5), "HY100" = mean(fsfat >= 10), 
                                                 "HY150" = mean(fsfat >= 15), "HY200" = mean(fsfat >= 20), 
                                                 "HY250" = mean(fsfat >= 25), "HY300" = mean(fsfat >= 30))
a <- hashAnswer %>% summarise("FY50" = sum(fsfat >= 5), "FY100" = sum(fsfat >= 10), 
                                                 "FY150" = sum(fsfat >= 15), "FY200" = sum(fsfat >= 20), 
                                                 "FY250" = sum(fsfat >= 25), "FY300" = sum(fsfat >= 30))
b <- hashAnswer %>% summarise("HY50" = mean(fsfat >= 5), "HY100" = mean(fsfat >= 10), 
                                                 "HY150" = mean(fsfat >= 15), "HY200" = mean(fsfat >= 20), 
                                                 "HY250" = mean(fsfat >= 25), "HY300" = mean(fsfat >= 30))


hashAnswer %>% group_by(lectureId) %>% summarise("FY50" = sum(fsfat >= 5), "HY50" = mean(fsfat > 5), 
                                                 "FY100" = sum(fsfat >= 10), "HY100" = mean(fsfat > 10),
                                                 "FY150" = sum(fsfat >= 15), "HY150" = mean(fsfat > 15),
                                                 "FY200" = sum(fsfat >= 20), "HY200" = mean(fsfat > 20),
                                                 "FY250" = sum(fsfat >= 25), "HY250" = mean(fsfat > 25),
                                                 "FY300" = sum(fsfat >= 30), "HY300" = mean(fsfat > 30))

#Búa til töflu til að sýna hve mikið af gögnunum fara yfir x
a <- hashAnswer %>% summarise("FY50" = sum(fsfat >= 5), "FY100" = sum(fsfat >= 10), 
                              "FY150" = sum(fsfat >= 15), "FY200" = sum(fsfat >= 20), 
                              "FY250" = sum(fsfat >= 25), "FY300" = sum(fsfat >= 30))
b <- hashAnswer %>% summarise("HY50" = mean(fsfat >= 5), "HY100" = mean(fsfat >= 10), 
                              "HY150" = mean(fsfat >= 15), "HY200" = mean(fsfat >= 20), 
                              "HY250" = mean(fsfat >= 25), "HY300" = mean(fsfat >= 30))
ab <- cbind(a, b)
FHbylim <- ab %>% pivot_longer(c('FY50', 'HY50', 'FY100', 'HY100', 'FY150', 'HY150', 'FY200', 'HY200', 'FY250', 'HY250', 'FY300', 'HY300'), 
                    names_to = "typewLim", values_to = "values") %>% 
  separate(typewLim, into = c("type", "limit"), sep = 2) %>% pivot_wider(names_from = type, values_from = values)


FHbylim

ggplot(hashAnswer, aes(x = hsta)) +
  geom_bar()
#Finna aðeins prósentur fyrir rétt svör, röng svör tengt við hve oft það hefur komið upp
prop.table(table(hashAnswer$correct))
prop.table(table(hashAnswer$hsta))
correctnam <- ifelse(hashAnswer$correct == 0, "Vitlaust", "Rétt")
hstanam <- ifelse(hashAnswer$hsta == "0", "Nýtt", "Hef séð áður")
corhsta <- prop.table(table(hashAnswer$correct+1, hashAnswer$hsta ))
corhsta <- rbind(corhsta, c(sum(corhsta[,1]), sum(corhsta[,2])))
corhsta <- cbind(corhsta, c(sum(corhsta[1,]), sum(corhsta[2,]), sum(corhsta[3,])))

row.names(corhsta) <- c("vitlaust", "rétt", "samtals")
colnames(corhsta) <- c("fyrsta", "hef séð", "samtals")

corhsta

prop.table(table(correctnam, hstanam ), margin = 1)
prop.table(table(correctnam, hstanam ), margin = 2)

#Hve mikið af svörum er fyrir hvern fyrirlestur
hashAnswer %>% group_by(lectureId) %>% summarise(n_distinct(hash))

#timeDif skoðanir, veit ekki hvort það er gott að nota
hashAnswer %>% group_by(studentId) %>% summarise("seconds" = mean(timeDif, na.rm = T), 
                                            "minutes" = mean(timeDif, na.rm = T)/60, 
                                            "hours" = mean(timeDif, na.rm = T)/3600)

prop.table(table(hashAnswer$lectureId))

summary(hashAnswer)

hashAnswer %>% group_by(lectureId) %>% mutate("count" = n()) %>% 
  ggplot(aes(x = reorder(lectureId, count, FUN = mean))) +
  geom_bar()

hashAnswer %>% group_by(studentId) %>% mutate("count" = n()) %>%
  ggplot(aes(x = reorder(studentId, count, FUN = mean))) +
  geom_bar() + 
  coord_flip()

xtabs(~studentId + correct, data = hashAnswer)

hashAnswer %>% summarise(n_distinct(studentId))

?reorder
?mean
?table
?prop.table
?geom_bar
?separate
?spread
?pivot_wider
?pivot_wider_spec
