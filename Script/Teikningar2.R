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

library(ggthemes)
library(ggpubr)

theme_set(theme_tufte() +
            theme(panel.border = element_rect('black', fill = NA),
                  text = element_text(size = 14),
                  legend.text=element_text(size=14),
                  axis.text=element_text(size=14),
                  axis.title = element_text(size = 14),
                  plot.title = element_text(hjust = 0.5)))


hashAnswer <- read.csv('Data/hashAnswer4.csv')
hashAnswer <- hashAnswer %>% subset(select=-c(X))
hashAnswer$hsta <- hashAnswer$hsta%>%as.character()
hashAnswer$lectureId <- hashAnswer$lectureId %>% as.factor()
hashAnswer$studentId <- hashAnswer$studentId %>% as.factor()
hashAnswer$nicc <- hashAnswer$nicc %>% as.factor()
hashAnswer$hluta2 <- cut_interval(hashAnswer$hluta, n = 5)

hashLim50 <- hashAnswer %>% group_by(studentId) %>% mutate("count" = n()) %>%
  filter(count > 7 & fsfat < 50)
hashLim100 <- hashAnswer %>% group_by(studentId) %>% mutate("count" = n()) %>%
  filter(count > 7 & fsfat < 100)


fit1 <- glmer(correct ~ fsfat*hsta + nicc + gpow + lectureId + (1 | studentId), 
              family = binomial(link = "logit"), data = hashLim100, nAGQ = 0, control =
                glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
fit3 <- glmer(correct ~ hluta2+hsta + nicc + gpow + lectureId + (1 | studentId), 
              family = binomial(link = "logit"), data = hashLim100, nAGQ = 0, control = 
                glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
fit7 <- glmer(correct ~ hluta2 + fsfat + hsta + nicc + gpow + lectureId + (1 | studentId), 
              family = binomial(link = "logit"), data = hashLim100, nAGQ = 0, control = 
                glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))


draw_calibration_curve <- function(fit1, fit3, fit7, df) {
  p1 <- df %>% add_predictions(fit1, type = "response") %>%
    ggplot(aes(x = pred, y = correct)) + 
    geom_smooth() +
    geom_abline(intercept = 0, slope = 1, lty = 2) +
    scale_colour_brewer(type = "qual", palette = "Set1") +
    ggtitle("Fyrsta líkanið")
  
  p2 <- df %>% add_predictions(fit3, type = "response") %>%
    ggplot(aes(x = pred, y = correct)) + 
    geom_smooth() +
    geom_abline(intercept = 0, slope = 1, lty = 2) +
    scale_colour_brewer(type = "qual", palette = "Set1") +
    ggtitle("Annað líkanið")
  
  p3 <- df %>% add_predictions(fit7, type = "response") %>%
    ggplot(aes(x = pred, y = correct)) + 
    geom_smooth() +
    geom_abline(intercept = 0, slope = 1, lty = 2) +
    scale_colour_brewer(type = "qual", palette = "Set1") +
    ggtitle("Þriðja líkanið")
  
  return(plot_grid(p1, p2, p3))
}

p <- draw_calibration_curve(fit1, fit3, fit7, hashLim100)

ggsave("Img/calibration.png", p, height = 12, width = 12)


draw_prediction_histogram <- function(fit1, fit3, fit7, df){
  tdf1 <- df %>% add_predictions(fit1, type = "response")
  p1 <- tdf1 %>%
    ggplot(aes(x = pred)) +
    geom_histogram(binwidth = 2 * IQR(tdf1$pred) * nrow(tdf1)^(-1/3)) +
    scale_colour_brewer(type = "qual", palette = "Set1") +
    ggtitle("Fyrsta líkanið")
  tdf2 <- df %>% add_predictions(fit3, type = "response")
  p2 <- tdf2 %>%
    ggplot(aes(x = pred)) +
    geom_histogram(binwidth = 2 * IQR(tdf2$pred) * nrow(tdf2)^(-1/3)) +
    scale_colour_brewer(type = "qual", palette = "Set1") +
    ggtitle("Annað líkanið")
  tdf3 <- df %>% add_predictions(fit7, type = "response")
  p3 <- tdf3 %>%
    ggplot(aes(x = pred)) +
    geom_histogram(binwidth = 2 * IQR(tdf3$pred) * nrow(tdf3)^(-1/3)) +
    scale_colour_brewer(type = "qual", palette = "Set1") +
    ggtitle("Þriðja líkanið")
  
  return(plot_grid(p1, p2, p3))
}

p2 <- draw_prediction_histogram(fit1, fit3, fit7, hashLim100)
p2

ggsave("Img/histogrammulti.png", p2, height = 12, width = 12)
