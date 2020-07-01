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
#here we go again
library(RCurl)
library(dfoptim)
library(nloptr)
library(optimx)


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


start_time <- Sys.time()
#Time difference of 28.67935 mins
fit1 <- glmer(correct ~ fsfat * hsta + nicc + gpow + lectureId + (1 | studentId), 
              family = binomial(link = "logit"), data = hashTest2, nAGQ = 0, 
              control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
end_time <- Sys.time()

end_time - start_time

tt <- getME(fit1, "theta")
ll <- getME(fit1, "lower")

load("Data/ans42")


min(tt[ll == 0])
#not singularity

?allFit

afurl <- "https://raw.githubusercontent.com/lme4/lme4/master/misc/issues/allFit.R"
eval(parse(text=getURL(afurl)))

aa2 <- allFit(fit1)

save(aa, file = "Data/aa1")
save(aa2, file = "Data/aa2")

is.OK <- sapply(aa,is,"merMod")  ## nlopt NELDERMEAD failed, others succeeded
aa.OK <- aa[is.OK]
lapply(aa.OK,function(x) x@optinfo$conv$lme4$messages)
summary(aa$nmkbw)

lapply(aa.OK, summary)

is.OK <- sapply(aa2,is,"merMod")  ## nlopt NELDERMEAD failed, others succeeded
aa2.OK <- aa2[is.OK]
lapply(aa2.OK,function(x) x@optinfo$conv$lme4$messages)

#byrjum fyrir bobyqa
start_time <- Sys.time()
#Time difference of 33.02954 mins
fit1 <- glmer(correct ~ fsfat*hsta + nicc + gpow + lectureId + (1 + fsfat * hsta | studentId), 
              family = binomial(link = "logit"), data = hashTest2, 
              control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
end_time <- Sys.time()

end_time - start_time

#næst fyrir nlminbwrap
start_time <- Sys.time()
#Time difference of 1.295588 hours
fit2 <- glmer(correct ~ fsfat*hsta + nicc + gpow + lectureId + (1 + fsfat * hsta | studentId), 
              family = binomial(link = "logit"), data = hashTest2, 
              control=glmerControl(optimizer="nlminbwrap",optCtrl=list(maxfun=2e5)))
end_time <- Sys.time()

end_time - start_time

#Í lokinn nloptwrap langa 
start_time <- Sys.time()
#Time difference of 28.67935 mins
fit3 <- glmer(correct ~ fsfat*hsta + nicc + gpow + lectureId + (1 + fsfat * hsta | studentId), 
              family = binomial(link = "logit"), data = hashTest2, 
              control=glmerControl(optimizer="nloptwrap",optCtrl=list(algorithm = "NLOPT_LN_NELDERMEAD", maxfun=2e5)))
end_time <- Sys.time()

end_time - start_time
