library(dbplyr)
library(dplyr)
library(RMySQL)
library(tidyr)
library(MASS)
library(ggplot2)
library(hash)
#les inn öll gögninn, nota mestalagi bara answer gagnasafnið
twDb<-src_mysql("tw_quizdb", host = "127.0.0.1", port = 3306, user = "gss24", password = "JuwofKWT2Ewc")
answer <- tbl(twDb, "answer")
question <- tbl(twDb,"question")
manyAota <- read.csv("manyAOTAsPois.hash",header = FALSE,sep = " ")
#les inn gagnasafnið sem var save-að
help("read.csv")

answer <- read.csv("answer.csv")
help("read.csv")
help("tbl")
#skoðumm aðeins gögninn
glimpse(answer)
answer %>% summarise("count"=n())
glimpse(question)
question %>% summarize("count"=n())




#prufa að sækja öll gögninn og save-a það
dAnswer <- as.data.frame(answer)
write.csv(dAnswer,"C:\\Users\\gylfi\\Desktop\\R powaaa\\Vinna\\spurning 4 verkefni\\answer.csv",row.names=FALSE)

#answer %>% select(answerId,lectureId,studentId,questionId,chosenAnswer,grade,correct,timeStart,lectureVersion)

#skoða einn nemanda í einu, til að fá tilfinningu
nemandi13 <- answer%>%filter(studentId%in%c(15))
nemandi13 <- as.data.frame(nemandi13)
glimpse(nemandi13)

#Skoða einn lecture til að fá tilfinningu
fyrirL5 <- answer %>% filter(lectureId%in%c(5))
fyrirL5 <- as.data.frame(fyrirL5)

skoda <- answer %>% filter(timeStart>'2020-01-01 01:00:00')

#aðeins vinnsla á nemandann til að skoða
summary(nemandi13)


nemandi13 <- nemandi13 %>% group_by(lectureId)
dat <- nemandi13 %>% summarize('einkunn'=mean(correct,na.rm = T), 'fjoldi'=n())
nemandi13 %>% group_by(answerId) %>% summarise('fjoldirett'=mean(correct), 'fjoldi'=n()) %>% View()

#Sýnir að öll answerId eru ólík
answer %>% group_by(answerId) %>% summarise('count'=n()) %>% filter(count>1) %>%summarise('count'=n())

#finn minnstu dagsetninguna
minstadagsetning <- nemandi13 %>% group_by(questionId) %>% summarise('mindag'=min(timeStart))
nemandimeddag <- full_join(nemandi13,minstadagsetning)
nemstytt <- nemandimeddag %>% subset(select= -c(practice,coinsAwarded,ugQuestionGuid,lectureVersion))

nemstytt$hefsedtheddaAdur <- ifelse(nemstytt$timeStart== nemstytt$mindag,0,1)
nemstytt$fjoldiSvaraFramAdThessu <- nemstytt %>% with(ave(timeStart,1,lectureId,FUN = seq_along))

#vinnsla með fyrirlestrana
minstadagsetningF <- fyrirL5 %>% group_by(studentId,questionId) %>% summarise('mindag'=min(timeStart))
fyrirmeddag <- full_join(fyrirL5,minstadagsetningF)
fyrirstytt <- fyrirmeddag %>% subset(select =  -c(practice,coinsAwarded,ugQuestionGuid,lectureVersion))

fyrirstytt$hefSedTheddaAdur <- ifelse(fyrirstytt$timeStart==fyrirstytt$mindag,0,1)
fyrirstytt$fjoldiSvaraFramAdThessu <- fyrirstytt %>% with(ave(timeStart,1,lectureId,studentId,FUN = seq_along))

help("seq_along")
help(ave)


#Vinnsla með allt í einu
minstadagsetningA <- answer %>% group_by(questionId,studentId) %>% summarise('mindag'=min(timeStart))
alltmeddag <- full_join(answer,minstadagsetningA)
allsvor <- alltmeddag %>% subset(select =  -c(practice,coinsAwarded,ugQuestionGuid))

allsvor$hefsedtheddaAdur <- ifelse(allsvor$timeStart==allsvor$mindag,0,1)
allsvor$fjoldiSvaraFramAdThessu <- allsvor %>% with(ave(timeStart,1,lectureId,studentId,FUN = seq_along))



help("cor")
summary(allsvor)
#barplot(main="skodun fjolda sem NA",width = 72)
help("cov")

answer$lectureVersion %>% max(na.rm = TRUE)

help("geom_histogram")


question%>%dplyr::select(plonePath,questionId)->miniQ
miniQ<-as.data.frame(miniQ)
grep("Qgen-q2100",miniQ$plonePath)
miniQ[grep("Qgen-q2100",miniQ$plonePath),]

