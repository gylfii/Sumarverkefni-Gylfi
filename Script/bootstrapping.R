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
#Annaðhvort þeirra gefur mem_used() til að skoða hve mikið af mininnu er notað
library(pryr)

hashAnswer <- read.csv('Data/hashAnswer4.csv')
hashAnswer <- hashAnswer %>% subset(select=-c(X))
hashAnswer$hsta <- hashAnswer$hsta%>%as.character()
hashAnswer$lectureId <- hashAnswer$lectureId %>% as.factor()
hashAnswer$studentId <- hashAnswer$studentId %>% as.factor()
hashAnswer$nicc <- hashAnswer$nicc %>% as.factor()
hashAnswer$fsfat <- hashAnswer$fsfat/10
hashAnswer$fsvfatu <- hashAnswer$fsvfatu/10
hashAnswer$hluta2 <- cut_interval(hashAnswer$hluta, n = 5)

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

test1 <- bootcreate(hashTest2)

test2 <- as.data.frame(x = NULL, y = NULL, z = NULL)
for (i in 1:100){
  test2 <- rbind(test2, c(i, 2 * i, 3 * i))
}
list(1, 2, 3)

#smá fall til að reikna BrierScoreið
BrierScore <- function(modl, df ) {
  predicted <- predict(modl, type = "response", newdata = df, allow.new.levels = T)
  truth <- df$correct
  return(mean((predicted-truth)^2))
}

SBrierScore <- function(modl, df) {
  predicted <- predict(modl, type = "response", newdata = df, allow.new.levels = T)
  truth <- df$correct
  Bs <- mean((predicted-truth)^2)
  Bmax <- mean(predicted) * (1-mean(predicted))
  return(1 - Bs/Bmax)
}
#model sem notuð eru fyrir bootwork
modl42 <- function(df) {
  ans <- glmer(correct ~ fsfat*hsta + nicc + gpow + lectureId + (1 + fsfat * hsta | studentId), family = binomial(link = "logit"), 
        data = df, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
  return(ans)
}

modl22 <- function(df) {
  ans <- glmer(correct ~ fsfat*hsta + nicc + gpow + lectureId + (1 | studentId), family = binomial(link = "logit"), 
               data = df, nAGQ = 0, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
  return(ans)
}

# modtest <- function(df) {
#   ans <- glm(correct ~ fsfat*hsta + nicc + gpow + lectureId, family = binomial(link = "logit"), data = df)
#   return(ans)
# }

modfit1 <- function(df) {
  ans <- glmer(correct ~ fsfat*hsta + nicc + gpow + lectureId + (1 | studentId), family = binomial(link = "logit"), 
               data = df, nAGQ = 0, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
  return(ans)
}

testBoot <- Bootwork(hashTest2, 10)
# #Nelder_Mead failed
# #nlminbwrap failed
# #nmkbw



modfit3 <- function(df) {
  ans <- glmer(correct ~ hluta2+hsta + nicc + gpow + lectureId + (1 | studentId), family = binomial(link = "logit"), 
               data = df, nAGQ = 0, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
  return(ans)
}

modfit7 <- function(df) {
  ans <- glmer(correct ~ hluta2 + fsfat + hsta + nicc + gpow + lectureId + (1 | studentId), family = binomial(link = "logit"), 
               data = df, nAGQ = 0, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
  return(ans)
}
fit <- modl22(hashTest2)

summary(fit)


test1 <- data.frame(brier = BrierScore(ans42, hashTest2), 
                      AUC = AUC(predict(ans42, type = "response"), hashTest2$correct))

#Bootwork er svo aðal Bootstrap fallið
# Þarf að manually setja inn glmer fallið sem maður þarf þar inni
Bootwork <- function(df, iteration){
  options(contrasts = c("contr.sum", "contr.poly"))
  #Byrjum fyrst að keyra það fyrir upprunalega gagnasafnið
  ormodel <- glmer(correct ~ hluta2 + fsfat + hsta + nicc + gpow + lectureId + (1 | studentId), family = binomial(link = "logit"), 
                   data = df, nAGQ = 0, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
  original <- data.frame(brier = BrierScore(ormodel, df), 
                         StanBrier = SBrierScore(ormodel, df),
                         AUC = AUC(predict(ormodel, type = "response"), df$correct))
  #Bý til nýjann grunn til að safna saman efnið frá bootstrappinu
  bootel <- data.frame(brier = numeric(0), StanBrier = numeric(0), AUC = numeric(0))
  bootel.or <- data.frame(brier = numeric(0), StanBrier = numeric(0), AUC = numeric(0))
  print('It begun')
  for (i in 1:iteration){
    #Bý til nýtt safn með bootstrap
    print(paste0(round(i/iteration * 100, digits = 1), "%"))
    Bdf <- bootcreate(df)
    #Bý til model fyrir þetta bootstrap
    #Bmodel <- Funmod(Bdf)
    Bmodel <- glmer(correct ~ hluta2 + fsfat + hsta + nicc + gpow + lectureId + (1 | studentId), family = binomial(link = "logit"), 
                    data = Bdf, nAGQ = 0, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
    
    #Reiknað breyturnar sem það þarf að reikna
    Nadditions <- data.frame(brier = BrierScore(Bmodel, Bdf), 
                             StanBrier = SBrierScore(Bmodel, Bdf),
                             AUC = AUC(predict(Bmodel, type = "response"), Bdf$correct))
    #Reiknað C.or eða original additions
    Nadditions.or <- data.frame(brier = BrierScore(Bmodel, df), 
                                StanBrier = SBrierScore(Bmodel, df),
                                AUC = AUC(predict(Bmodel, type = "response", newdata = df, allow.new.levels = T), df$correct))
    #Geyma efnið í gagnasafninu
    bootel <- rbind(bootel, Nadditions)
    bootel.or <- rbind(bootel.or, Nadditions.or)
    # rm(Bmodel)
  }
  #skila original og bootel
  return(list(original, bootel, bootel.or))
}

start_time <- Sys.time()
test1 <- Bootwork(hashTest2, 3, modl42)
end_time <- Sys.time()

save(test1, file ="Data/test1")

end_time - start_time
#Time difference of 2.12855 hours




start_time <- Sys.time()
test2 <- Bootwork(hashTest2, 10, modl22)
end_time <- Sys.time()

end_time - start_time
#Time difference of 1.450277 hours
#Vandi, failed to converge 50% skiptana T.T

#another run for the bootstrap to see the new standardized brier
#Time difference of 1.279231 hours
#Though sadly, it failed to converge 75% of the time T.T

start_time <- Sys.time()
BootedData <- Bootwork(hashTest2, 200, modl22)
end_time <- Sys.time()

end_time - start_time

save(BootedData, file = "Data/BootedData")
load('Data/BootedData')

#Hér eru verkfærinn fyrir reikninginn hjá fit1, fit3 og fit7 eftir að hafa bætt við hluta
memory.limit(300000)

#Muna að setja upp fallið inní
set.seed(117)
bootedfit1 <- Bootwork(hashTest2, 2500)
save(bootedfit1, file = "Data/Bootedfit1")

#Muna að setja upp fallið inní
set.seed(118)
bootedfit3 <- Bootwork(hashTest2, 2500)
save(bootedfit3, file = "Data/Bootedfit3")

#muna að setja upp fallið inní
set.seed(119)
bootedfit7 <- Bootwork(hashTest2, 2500)
save(bootedfit7, file = "Data/Bootedfit7")

?gc


?memory.limit
memory.size()
memory.size(max = T)
memory.limit(100000)
wait <- modl22(hashTest2)

summary(fit)
summary(ans22)



broom::tidy(fit)
broom::tidy(fit3)
broom::tidy(ans22)

BrierScore(fit, hashTest2)
BrierScore(ans22, hashTest2)

AUC(predict(fit, type = "response"), hashTest2$correct)
AUC(predict(ans22, type = "response"), hashTest2$correct)


# start_time <- Sys.time()
# fit2 <- glmer(correct ~ fsfat*hsta + nicc + gpow + lectureId + (1 | studentId), family = binomial(link = "logit"), 
#       data = hashTest2, control=glmerControl(optimizer="nloptwrap",optCtrl=list(maxfun=2e5)))
# end_time <- Sys.time()
# 
# end_time - start_time
# 
# start_time <- Sys.time()
# fit3 <- glmer(correct ~ fsfat*hsta + nicc + gpow + lectureId + (1 | studentId), family = binomial(link = "logit"), 
#               data = hashTest2, nAGQ = 0, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
# end_time <- Sys.time()
# 
# end_time - start_time
# 
# 
# start_time <- Sys.time()
# ans22 <- glmer(correct ~ fsfat*hsta + nicc + gpow + lectureId + (1 | studentId), family = binomial(link = "logit"), 
#                data = hashTest2, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
# end_time <- Sys.time()
# 
# end_time - start_time