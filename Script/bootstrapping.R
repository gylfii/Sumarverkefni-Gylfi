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
#Bootstrapping pakki
library(boot)
#Fyrir AUC reikninga
library(cvAUC)

hashAnswer <- read.csv('Data/hashAnswer4.csv')
hashAnswer <- hashAnswer %>% subset(select=-c(X))
hashAnswer$hsta <- hashAnswer$hsta%>%as.character()
hashAnswer$lectureId <- hashAnswer$lectureId %>% as.factor()
hashAnswer$studentId <- hashAnswer$studentId %>% as.factor()
hashAnswer$nicc <- hashAnswer$nicc %>% as.factor()
hashAnswer$fsfat <- hashAnswer$fsfat/10
hashAnswer$fsvfatu <- hashAnswer$fsvfatu/10

hashTest2 <- hashAnswer %>% group_by(studentId) %>% mutate("count" = n()) %>%
  filter(count > 7 & fsfat < 10)

load("Data/ans22")
load("Data/ans42")

?bootMer
test1 <- 1:10 * 3
test2 <- sample(1:10, 5, replace = TRUE)
testf <- test1[test2]

nesttest1 <- hashAnswer %>% group_by(lectureId, studentId) %>% nest()
justtest <- nesttest1 %>% filter(lectureId == 3082)


nesttest2 <- nesttest1[c(1, 2), ] %>% unnest(cols = c(data))
nesttest3 <- nesttest1[c(4, 5), ] %>% unnest(cols = c(data))
nesttest4 <- rbind(nesttest2, nesttest3)
nesttest5 <- rbind(NULL, nesttest4)
# blankfunc <- function(.){
#   sigma(.)
# }
# 
# 
# testing <- bootMer(ans22, blankfunc, nsim = 2)

#þetta er fall sem er að búa til nýtt bootstrapped gagnasafn til að geta keyrt restina í
bootcreate <- function(df) {
  #Búið til nested gagnasafn til að geta tekið fyrir hvern nemenda
  dfnest <- df %>% group_by(lectureId, studentId) %>% nest()
  bootmade <- NULL
  #Lykkja til að setja saman gagnasafnið, ýtrar fyrir hvern fyrirlestur og finnur jafnmarga nemendur aftur
  for (id in unique(dfnest$lectureId)) {
    dftemp <- dfnest %>% filter(lectureId == id)
    ni <- dftemp$studentId %>% n_distinct()
    takenNr <- sample(1:ni, replace = TRUE)
    notnest <- dftemp[takenNr, ] %>% unnest(cols = c(data))
    bootmade <- rbind(bootmade, notnest)
  }
  return(bootmade)
}

test1 <- bootcreate(hashAnswer)

test2 <- as.data.frame(x = NULL, y = NULL, z = NULL)
for (i in 1:100){
  test2 <- rbind(test2, c(i, 2 * i, 3 * i))
}
list(1, 2, 3)

#smá fall til að reikna BrierScoreið
BrierScore <- function(modl, df ) {
  predicted <- predict(modl, type = "response")
  truth <- df$correct
  return(mean((predicted-truth)^2))
}

#model sem notuð eru fyrir bootwork
modl42 <- function(df) {
  ans <- glmer(correct ~ fsfat*hsta + nicc + gpow + lectureId + (1 + fsfat * hsta | studentId), family = binomial(link = "logit"), 
        data = df, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
  return(ans)
}

test1 <- data.frame(brier = BrierScore(ans42, hashTest2), 
                      AUC = AUC(predict(ans42, type = "response"), hashTest2$correct))

#Bootwork er svo aðal Bootstrap fallið
Bootwork <- function(df, iteration, Funmod){
  #Byrjum fyrst að keyra það fyrir upprunalega gagnasafnið
  ormodel <- Funmod(df)
  original <- data.frame(brier = BrierScore(ormodel, df), 
                         AUC = AUC(predict(ormodel, type = "response"), df$correct))
  
  #Bý til nýjann grunn til að safna saman efnið frá bootstrappinu
  bootel <- data.frame(brier = numeric(0), AUC = numeric(0))
  for (i in 1:iteration){
    #Bý til nýtt safn með bootstrap
    Bdf <- bootcreate(df)
    #Bý til model fyrir þetta bootstrap
    Bmodel <- Funmod(Bdf)
    #Reiknað breyturnar sem það þarf að reikna
    Nadditions <- data.frame(brier = BrierScore(Bmodel, Bdf), 
                             AUC = AUC(predict(Bmodel, type = "response"), Bdf$correct))
    #Geyma efnið í gagnasafninu
    bootel <- rbind(bootel, Nadditions)
  }
  #skila original og bootel
  return(list(original, bootel))
}

start_time <- Sys.time()
test1 <- Bootwork(hashTest2, 3, modl42)
end_time <- Sys.time()

save(test1, file ="Data/test1")

end_time - start_time
#Time difference of 2.12855 hours