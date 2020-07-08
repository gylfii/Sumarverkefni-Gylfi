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


load("Data/Bootedfit1")

optim.cal <- function(bdf) {
  # The original model on the original dataset
  B.app <- bdf[[1]]
  # The booted model on the booted dataset
  B.boot <- bdf[[2]]
  # The booted model on the original dataset
  B.or <- bdf[[3]]
  
  auc.app <- B.app$AUC
  Brier.app <- B.app$brier
  stBrier.app <- B.app$StanBrier
  
  auc.phi <- mean(B.boot$AUC - B.or$AUC)
  Brier.phi <- mean(B.boot$brier - B.or$brier)
  stBrier.phi <- mean(B.boot$StanBrier - B.or$StanBrier)
  
  optimism <- data.frame(AUC = auc.phi, brier = Brier.phi, stBrier = stBrier.phi)
  optimismcorrected <- data.frame(AUC = auc.app - auc.phi, brier = Brier.app - Brier.phi, stBrier = stBrier.app - stBrier.phi)
  
  return(list(optimism, optimismcorrected))
}

testcase <- optim.cal(testBoot)


