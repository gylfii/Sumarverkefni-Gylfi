library(dbplyr)
library(dplyr)
library(RMySQL)
library(tidyr)
library(MASS)
library(ggplot2)
library(magrittr)
#les inn öll gögninn, nota mestalagi bara answer gagnasafnið
twDb<-src_mysql("tw_quizdb", host = "127.0.0.1", port = 3306, user = "gss24", password = "JuwofKWT2Ewc")
answer<-tbl(twDb,"answer")
question<-tbl(twDb,"question")
#hashes<-read.csv("Data/bighashfile.txt",sep=" ",col.names = c("dir","qName","hash","numQ","notaType"))
hashes<-read.csv("Data/bighashfile.wide.txt",sep=" ",col.names = c("dir","qName","hash","hash2","hash3","numQ","notaType"),na.strings = ".")
#pathQ<-unique(paste(hashes$dir,hashes$qName,sep=""))
plonePath<-unique(paste(hashes$dir,hashes$qName,sep=""))
plonePath<-as.data.frame(plonePath)
hashes$plonePath<-paste(hashes$dir,hashes$qName,sep="")
left_join(plonePath,as.data.frame(question))->myQuestions
#question%>%filter(plonePath%in%pathQ)->myQuestions
#myQuestions<-as.data.frame(myQuestions)
answer%>%filter(timeStart>"2020-01-01 00:01:01")->answerRed
answerRed<-as.data.frame(answerRed)
inner_join(answerRed,myQuestions) -> myAnswer
MyAnswer<-as.data.frame(myAnswer)




#Set inn fjöldi svara framAd Thessu og hefséð áður
MyAnswer <- MyAnswer[order(MyAnswer$timeStart),] %>% group_by(lectureId,studentId) %>% mutate(fsfat=row_number()-1)
hashAnswer <- inner_join(MyAnswer,hashes) %>%
  pivot_longer(c(hash,hash2,hash3),values_to = "hash") %>% filter(!is.na(hash))
minstadagsetning <- hashAnswer %>% group_by(studentId,hash) %>% summarise('mindag'=min(timeStart))
hashAnsdag <- full_join(hashAnswer,minstadagsetning)

hashAnsdag$hsta <- ifelse(hashAnsdag$mindag==hashAnsdag$timeStart,0,1)
hashAnsdag4 <- hashAnsdag[!grepl('NOTA+',hashAnsdag$hash),]

#Vel dálka og save-a
hashanswers <- hashAnsdag %>% dplyr::select(lectureId,studentId,questionId,correct,hash,fsfat,hsta)
hashanswers4 <- hashAnsdag4 %>% dplyr::select(lectureId,studentId,questionId,correct,hash,fsfat,hsta)
write.csv(hashanswers,'Data/hashAnswer.csv')
write.csv(hashanswers4,'Data/hashAnswer4.csv')


#hugmynd til að reikna tímann milli taka
#fyrst er sort-að eftir studentId,hash og timestart. Í þessari röð. Svo að gera ifelse setningu sem reiknar
# timestart-lag(timeEnd), þetta ætti að virka með réttri röðunn, vonandi. Kannski spurning um hvernig við fáum þá raðaða rétt
# Svo er spurning um date, will líklegast fá muninn í min eða klst. Hvernig væri bæst að gera date1-date2 og fá það í mín
# Við sjáum til.
# https://stackoverflow.com/questions/33068340/how-to-get-difference-in-minutes-between-two-date-strings
