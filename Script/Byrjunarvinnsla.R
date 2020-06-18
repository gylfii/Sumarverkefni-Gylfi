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

hashAnswer <- read.csv('Data/hashAnswer4.csv')
hashAnswer <- hashAnswer %>% subset(select=-c(X))
hashAnswer$hsta <- hashAnswer$hsta%>%as.character()
hashAnswer$lectureId <- hashAnswer$lectureId %>% as.factor()
hashAnswer$studentId <- hashAnswer$studentId %>% as.factor()
hashAnswer$nicc <- hashAnswer$nicc %>% as.factor()
hashAnswer$fsfat <- hashAnswer$fsfat/10
hashAnswer$fsvfatu <- hashAnswer$fsvfatu/10
hashAnswer$gpow2 <- as.factor(floor(log2(hashAnswer$gpow)))

hashAnswer %$% ifelse(gpow < 0.25, "Undir 0.25", ifelse(gpow > 0.25, "Yfir 0.25", "Er 0.25")) %>% table() %>% prop.table()


tmp <- unique(hashAnswer$lectureId)[-1]
tmp2 <- unique(hashAnswer$lectureId)
#Skoðum stuttlega þegar skorið er af gögnunum


hashTest <- hashAnswer %>% group_by(studentId) %>% mutate("count" = n()) %>%
  filter(count > 7 & fsfat < 5)
hashTest2 <- hashAnswer %>% group_by(studentId) %>% mutate("count" = n()) %>%
  filter(count > 7 & fsfat < 10)
hashAnswer %>% group_by(lectureId) %>% summarise(n_distinct(studentId))

hashTest2 %$% ifelse(gpow < 0.25, "Undir 0.25", ifelse(gpow > 0.25, "Yfir 0.25", "Er 0.25")) %>% table() %>% prop.table()

#nokkrar Glmer til að prufa
ans <- glmer(correct ~ fsvfatu*hsta + nicc + lectureId + (1 | studentId), family = binomial(link = "logit"), data = hashTest, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
options(contrasts = c("contr.sum", "contr.poly"))
ans1 <- glmer(correct ~ fsvfatu*hsta + nicc + lectureId + (1 | studentId), family = binomial(link = "logit"), 
              data = hashTest, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
ans12 <- glmer(correct ~ fsvfatu*hsta + nicc + gpow + lectureId + (1 | studentId), family = binomial(link = "logit"), 
                data = hashTest2, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
ans2 <- glmer(correct ~ fsfat*hsta + nicc + gpow + lectureId + (1 | studentId), family = binomial(link = "logit"), data = hashTest, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
ans22 <- glmer(correct ~ fsfat*hsta + nicc + gpow + lectureId + (1 | studentId), family = binomial(link = "logit"), data = hashTest2, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

ans32 <- glmer(correct ~ fsfat*hsta + nicc + lectureId + (1 | studentId), family = binomial(link = "logit"), data = hashTest2, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))



ans3 <- glmer(correct ~ fsvfatu*hsta + gpow + lectureId + (1 | studentId), family = binomial(link = "logit"), data = hashTest, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e6)))

ans42 <- glmer(correct ~ fsfat*hsta + nicc + gpow + lectureId + (1 + fsfat * hsta | studentId), family = binomial(link = "logit"), data = hashTest2, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

ans52 <- glmer(correct ~ fsfat*hsta + nicc + gpow + lectureId + (1 + fsfat | studentId), family = binomial(link = "logit"), data = hashTest2, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))


stepcAIC(ans22, direction = "backward", trace = TRUE, data = hashTest2)

hashAnswer %>% ggplot(aes(x = fsfat,  y = fsvfatu)) +
  geom_point()

save(ans42, file = "Data/ans42")
save(ans22, file = 'Data/ans22')

load("Data/ans22")
load("Data/ans42")


#Aðeins teiknað til að skoða betur
ggplot(hashAnswer, aes(x = fsfat, y = correct, color = hsta)) +
  geom_point() +
  geom_smooth()

hashAnswer %>% filter(hsta == "0") %>% ggplot(aes(x = fsfat * 10, y = correct))+
  geom_point() +
  geom_smooth(se = FALSE) + 
  facet_wrap(vars(lectureId))

?geom_smooth

broom::tidy(ans)
summary(ans12)
ranef(ans1)
Anova(ans12, type = 3)
Anova(ans3, type = 3)
summary(ans3)

BrierScore <- function(modl, df ) {
  predicted <- predict(modl, type = "response")
  truth <- df$correct
  return(mean((predicted-truth)^2))
}

BrierScore(ans42, hashTest2)
BrierScore(ans22, hashTest2)




summary(ans22)
Anova(ans22, type = 3)

Anova(ans42, type = 3)

summary(ans42)

anova(ans32, ans22)
anova(ans42, ans22)
anova(ans42, ans52)

confint(ans22)
emmeans::emmeans(ans22, ~ fsfat, type = "response")

#Teiknum ROC
ROCDraw <- function(df, modl) {
  score <- predict(modl, type = "response")
  y <- df$correct
  
  #False positive rate
  FPR <- 0
  
  #true positive rate
  TPR <- 0
  
  #setjum upp threshold
  threshold <- seq(from = 0, to = 1, by = 0.01)
  
  # number of positive and negative
  P <- sum(y)
  N <- length(y) - P
  
  #Iterate through all thresholds
  
  for (j in 1:length(threshold)) {
    FP <- 0
    TP <- 0
    
    thresh <- threshold[j]
    for (i in 1:length(score)) {
      if (score[i] >= thresh) {
        if (y[i] == 1){
          TP <- TP + 1
        }
        if (y[i] == 0){
          FP <- FP + 1
        }
      }
      
    }
    FPR[j] <- FP/N
    TPR[j] <- TP/P
  }
  #Reiknum AUC með trapezoid reglunni
  auc <- -1 * trapz(FPR, TPR)
  #Að lokum teiknum við aðeins upp
  p <- data.frame(x = FPR, y = TPR) %>%
    ggplot(aes(x = FPR, y = TPR)) +
    geom_area(fill = "lightblue")+
    geom_line(color = "orange", size = 1.5) +
    geom_line(data = data.frame(x = seq(0, 1, 0.01), y = seq(0, 1, 0.01)), aes(x = x, y = y), linetype = "dashed") +
    labs(title = paste0("ROC curve, AUC = ", round(auc, digits = 4))) +
    xlab("False Positive Rate") +
    ylab("True Positive Rate") +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))
      
  return(p)
  
  
}

p1 <- ROCDraw(hashTest2, ans42)
p2 <- ROCDraw(hashTest2, ans22)
p1
p2

grid.arrange(p1, p2, nrow = 1)

?geom_area

?boot

?geom_line
?seq
testhold <- seq(from = 0, to = 1, by = 0.01)


#að flótu bragði, þá lítur út fyrir að fsfat virkar betur en fsvfat? skoða betur seinna
hashTest %>% ungroup() %>% summarise(mean(gpow == 0.25))

summary(ans2)
Anova(ans2, type = 3)
summary(ans3)
predict(ans)
predict.glm(ans, type = "response")
hashTest$pred <- predict(ans, type = "response")
hashTest$pred2 <- predict(ans2, type = "response")
hashTest$pred3 <- predict(ans3, type = "response")

#stuttar myndir teiknaðar með glmer, mun uppfæra svo

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



