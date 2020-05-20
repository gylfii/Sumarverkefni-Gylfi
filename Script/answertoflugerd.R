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
hashes$plonePath <- paste(hashes$dir,hashes$qName,sep="")
pathQ<-unique(paste(hashes$dir,hashes$qName,sep=""))
question%>%filter(plonePath%in%pathQ)->myQuestions
answer%>%filter(timeStart>"2020-01-01 00:01:01")->answerRed
inner_join(answerRed,myQuestions) -> myAnswer
as.data.frame(myAnswer) -> MyAnswer
#Set inn fjöldi svara framAd Thessu og hefséð áður
MyAnswer <- MyAnswer[order(MyAnswer$timeStart),] %>% group_by(lectureId,studentId) %>% mutate(fsfat=row_number()-1)
hashAnswer <- inner_join(MyAnswer,hashes)
minstadagsetning <- hashAnswer %>% group_by(studentId,hash) %>% summarise('mindag'=min(timeStart))
hashAnsdag <- full_join(hashAnswer,minstadagsetning)
hashAnsdag$hsta <- ifelse(hashAnsdag$mindag==hashAnsdag$timeStart,0,1)
#Vel dálka og save-a
hashanswers <- hashAnsdag %>% dplyr::select(lectureId,studentId,questionId,correct,fsfat,hsta)
write.csv(hashanswers,'Data/hashAnswer.csv')