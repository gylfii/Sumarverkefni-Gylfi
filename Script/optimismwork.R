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
library(cowplot)


load("Data/Bootedfit1")
load("Data/Bootedfit3")
load("Data/Bootedfit7")


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

optimfit1 <- optim.cal(bootedfit1)
optimfit3 <- optim.cal(bootedfit3)
optimfit7 <- optim.cal(bootedfit7)

?pivot_longer

optim.draw.box <- function(B1, B2, B3) {
  bo1 <- B1[[2]]
  bo2 <- B2[[2]]
  bo3 <- B3[[2]]
  bot1 <- B1[[3]]
  bot2 <- B2[[3]]
  bot3 <- B3[[3]]
  
  AllAuc <- data.frame(Train.fit1 = bo1$AUC, Train.fit3 = bo2$AUC, Train.fit7 = bo3$AUC, 
                       Test.fit1 = bot1$AUC, Test.fit3 = bot2$AUC, Test.fit7 = bot3$AUC) %>% 
    pivot_longer(cols = c("Train.fit1", "Train.fit3", "Train.fit7", 
                          "Test.fit1", "Test.fit3", "Test.fit7"), names_to = "Type")
  
  AllAuc <- AllAuc %>% separate(col = "Type", into = c("Type", "Model"), sep = "\\.")
  AllAuc$Type <- AllAuc$Type %>% fct_rev()
  p1 <- ggplot(AllAuc, aes(y = value, x = Type, color = Model)) +
    #geom_point(position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0.1), alpha = 0.05) +
    geom_boxplot() +
    labs(title = "AUC") +
    theme(legend.position="top") +
    scale_colour_brewer(type = "qual", palette = "Set1")+
    theme(legend.position = 'bottom')
  
  AllBrier <- data.frame(Train.fit1 = bo1$brier, Train.fit3 = bo2$brier, Train.fit7 = bo3$brier, 
                       Test.fit1 = bot1$brier, Test.fit3 = bot2$brier, Test.fit7 = bot3$brier) %>% 
    pivot_longer(cols = c("Train.fit1", "Train.fit3", "Train.fit7", 
                          "Test.fit1", "Test.fit3", "Test.fit7"), names_to = "Type")
  
  AllBrier <- AllBrier %>% separate(col = "Type", into = c("Type", "Model"), sep = "\\.")
  AllBrier$Type <- AllBrier$Type %>% fct_rev()
  p2 <- ggplot(AllBrier, aes(y = value, x = Type, color = Model)) +
    #geom_point(position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0.1), alpha = 0.05) +
    geom_boxplot() +
    labs(title = "Brier") +
    theme(legend.position="top") +
    scale_colour_brewer(type = "qual", palette = "Set1")+
    theme(legend.position = 'bottom')
  
  
  AllstBrier <- data.frame(Train.fit1 = bo1$StanBrier, Train.fit3 = bo2$StanBrier, Train.fit7 = bo3$StanBrier, 
                       Test.fit1 = bot1$StanBrier, Test.fit3 = bot2$StanBrier, Test.fit7 = bot3$StanBrier) %>% 
    pivot_longer(cols = c("Train.fit1", "Train.fit3", "Train.fit7", 
                          "Test.fit1", "Test.fit3", "Test.fit7"), names_to = "Type")
  
  AllstBrier <- AllstBrier %>% separate(col = "Type", into = c("Type", "Model"), sep = "\\.")
  AllstBrier$Type <- AllstBrier$Type %>% fct_rev()
  
  p3 <- ggplot(AllstBrier, aes(y = value, x = Type, color = Model)) +
    #geom_point(position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0.1), alpha = 0.05) +
    geom_boxplot() +
    labs(title = "Standardized Brier") +
    theme(legend.position="top") +
    scale_colour_brewer(type = "qual", palette = "Set1")+
    theme(legend.position = 'bottom')
  
  return(plot_grid(p1, p2, p3))
}

?plot_grid
?separate
?pivot_longer
p <- optim.draw.box(bootedfit1, bootedfit3, bootedfit7)
p

ggsave('Img/optimism.png', p, width = 12, height = 12)


bo1 <- bootedfit1[[2]]
bo2 <- bootedfit3[[2]]
bo3 <- bootedfit7[[2]]

bot2 <- bootedfit3[[3]]

data.frame(Train.Auc = bo2$AUC, Test.Auc = bot2$AUC) %>% 
  as_tibble() %>% 
  gather(type, val) %>% 
  ggplot(aes(x = type, y = val)) + 
  geom_point()
AllAuc <- data.frame(fit1 = bo1$AUC, fit3 = bo2$AUC, fit7 = bo3$AUC) %>% 
  pivot_longer(cols = c("fit1", "fit3", "fit7"), names_to = "model")





#Test fyrir teikningar

