#Fyrst eru það þægilegu dplyr og tidyr föllin. Svo RMySQL til að fá aðgang á SQL
library(dbplyr)
library(dplyr)
library(RMySQL)
library(tidyr)
#Hvað gerir MASS aftur?
library(MASS)
#GGplot gott til að teikna
library(ggplot2)
library(gridExtra)
#Tidyverse er geðveikt
library(tidyverse)
#modelr var fyrir %$% skipanir minnist mig
library(modelr)
#hvað gerir rlang aftur?
library(rlang)
#lme4 og lmerTest er fyrir glmer og fleira þægilegt
library(lme4)
library(lmerTest)
#Car er til að geta notað Anova
library(car)
#cAIC4 er svo það sé hægt að prufa stepcAIC, svo stepAIC fyrir glmer fallið, mun líklega ekki nota, tekur of langan tíma
library(cAIC4)
#til að geta gert trapz fyrir AUC í ROC
library(pracma)

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