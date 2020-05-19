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

# hasht <- read.csv("C:\\Users\\gylfi\\Documents\\temp\\bighashfile.txt",sep=" ",col.names = c("dir","qName","hash","numQ","notaType"))
# hasht$plonePath <- paste(hasht$dir,hasht$qName,sep="")
#setja upp Svörinn sem við ætlum að skoða
hashes<-read.csv("Data/bighashfile.txt",sep=" ",col.names = c("dir","qName","hash","numQ","notaType"))
hashes$plonePath <- paste(hashes$dir,hashes$qName,sep="")
pathQ<-unique(paste(hashes$dir,hashes$qName,sep=""))
question%>%filter(plonePath%in%pathQ)->myQuestions
answer%>%filter(timeStart>"2020-01-01 00:01:01")->answerRed
inner_join(answerRed,myQuestions) -> myAnswer
as.data.frame(myAnswer) -> MyAnswer
#Set inn fjöldi svara framAd Thessu og hefséð áður
MyAnswer$fsfat <- MyAnswer %>% with(ave(timeStart,1,lectureId,studentId,FUN = seq_along))
MyAnswer <- MyAnswer[order(MyAnswer$timeStart),] %>% group_by(lectureId,studentId) %>% mutate(fsfat=row_number()-1)
#skodun <- MyAnswer%>% dplyr::select(lectureId,studentId,questionId,timeStart,fsfat)

hashAnswer <- inner_join(MyAnswer,hashes)


#skoðumm aðeins gögninn
glimpse(answer)
answer %>% summarise("count"=n())
glimpse(myQuestions)
question %>% summarize("count"=n())

# Skoða aðeins tengingarnar
question%>%dplyr::select(plonePath,questionId)->miniQ
miniQ<-as.data.frame(miniQ)
grep("Qgen-q2100",miniQ$plonePath)
miniQ[grep("Qgen-q2100",miniQ$plonePath),]





