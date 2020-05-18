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
answer <- answer%>% filter(timeStart>'2020-01-01 01:00:00')
question <- tbl(twDb,"question")

#setja upp Svörinn sem við ætlum að skoða
hashes<-read.csv("Data/bighashfile.txt",sep=" ",col.names = c("dir","qName","hash","numQ","notaType"))
pathQ<-unique(paste(hashes$dir,hashes$qName,sep=""))
question%>%filter(plonePath%in%pathQ)->myQuestions
answer%>%filter(timeStart>"2020-01-01 00:01:01")->answerRed
inner_join(answerRed,myQuestions) -> myAnswer

# answer <- read.csv("answer.csv")
help("read.csv")
help("tbl")
#skoðumm aðeins gögninn
glimpse(answer)
answer %>% summarise("count"=n())
glimpse(question)
question %>% summarize("count"=n())

# Skoða aðeins tengingarnar
question%>%dplyr::select(plonePath,questionId)->miniQ
miniQ<-as.data.frame(miniQ)
grep("Qgen-q2100",miniQ$plonePath)
miniQ[grep("Qgen-q2100",miniQ$plonePath),]




#prufa að joina nokkuð saman
hashes$plonePath <- paste(hashes$dir,hashes$qName,sep="")
test <- right_join(miniQ,hashes)

quest <- as.data.frame(question)
test2 <- right_join(quest,hashes)

test2 %>%dplyr::select(questionId) %>% duplicated() %>% sum
hashes %>%dplyr::select(plonePath) %>% duplicated() %>% sum


answer <- as.data.frame(answer)

prufa <- inner_join(answer,test2)
prufa <- prufa %>% dplyr::select(answerId,lectureId,studentId,questionId,correct,timeStart,V3,V4,V5)

prufa2 <- inner_join(MyAnswer,hashes)

#Sést að prufa er ekki alveg rétt, ekki nægilegt að gera join með questionId sem eini lykillinn, þarf fleiri
glimpse(prufa)
summary(prufa)
