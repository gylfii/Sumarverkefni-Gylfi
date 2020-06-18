library(Matrix)
library(dbplyr)
library(dplyr)
library(RMySQL)
Sys.setlocale("LC_ALL", "is_IS.UTF-8")
library(RMySQL)
library(MASS)



twDb<-src_mysql("tw_quizdb", host = "127.0.0.1", port = 3306, user = "gss24", password = "JuwofKWT2Ewc")
question <- tbl(twDb,"question")
student<-tbl(twDb,"student")
answer<-tbl(twDb,"answer")
lecture<-tbl(twDb,"lecture")
host<-tbl(twDb,"host")
coinAward<-tbl(twDb,"coinAward")
lectureStudentSetting<-tbl(twDb,"lectureStudentSetting")
lectureGlobalSetting <- tbl(twDb, "lectureGlobalSetting")
lectureStudentSetting%>%group_by(key)%>%summarise(stdev=sd(value,na.rm=T),stdevlog=sd(log2(value),na.rm=T))->stdevs
hashes<-read.csv("Data/bighashfile.txt",sep=" ",col.names = c("dir","qName","hash","numQ","notaType"))
pathQ<-unique(paste(hashes$dir,hashes$qName,sep=""))
question%>%filter(plonePath%in%pathQ)->myQuestions
answer%>%filter(timeStart>"2020-01-01 00:01:01")->answerRed
inner_join(answerRed,myQuestions) -> myAnswer

#
# myAnswer should now contain all answers to just the questions listed in the hash file
#

glimpse(question)
glimpse(host)
glimpse(coinAward)
glimpse(lectureStudentSetting) 
glimpse(lectureGlobalSetting)
globalgpow <- lectureGlobalSetting %>% as.data.frame()
globalgpow %>% filter(lectureId %in% tmp & key == "iaa_adaptive_gpow") %>% 
  group_by(lectureId) %>%
  summarise(n_distinct(value), mean(as.numeric(value)))
warnings()
globalgpow %>% summarise(n_distinct(lectureId))
possiblegpow <- lectureStudentSetting %>% as.data.frame() 
possiblegpow %>% group_by(key) %>% summarise(n()) %>% View
possiblegpow %>% filter(lectureId %in% tmp & key == "iaa_adaptive_gpow") %>%  View()
lectureStudentSetting %>% filter(lectureId %in% tmp & key == "iaa_adaptive_gpow") %>% summarise(n())
glimpse(answer)
glimpse(myAnswer)
myAnswer %>% summarise(n())

MyAnswer <- as.data.frame(myAnswer)
MyAnswer %>% summary()



# tAnswer <- as.data.frame(answer)
# tQuestion <- as.data.frame(question)
# questionCount <- tAnswer %>% group_by(questionId) %>% summarise("timescount" =  n())
# isit <- inner_join(tQuestion, questionCount)
# isit %>% filter(timescount != timesAnswered) %>% summarise(n())
