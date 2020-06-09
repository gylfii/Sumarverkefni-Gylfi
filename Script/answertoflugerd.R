library(dbplyr)
library(dplyr)
library(RMySQL)
library(tidyr)
library(MASS)
library(ggplot2)
library(magrittr)
library(lubridate)
#les inn öll gögninn, nota mestalagi bara answer gagnasafnið
twDb<-src_mysql("tw_quizdb", host = "127.0.0.1", port = 3306, user = "gss24", password = "JuwofKWT2Ewc")
answer<-tbl(twDb,"answer")
question<-tbl(twDb,"question")
lectureStudentSetting<-tbl(twDb,"lectureStudentSetting")
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
# view(answerRed)
studentgpow <- lectureStudentSetting %>% as.data.frame() %>% filter(key == "iaa_adaptive_gpow" & creationDate > "2020-01-01 00:01:01") %>%
  dplyr::select(lectureId, studentId, lectureVersion, value, creationDate)

inner_join(answerRed,myQuestions) -> myAnswer
MyAnswer<-as.data.frame(myAnswer)
explory <- left_join(MyAnswer, studentgpow)
MyAnswer$gpow <- ifelse(is.na(explory$value), 0.25, explory$value)

# View(MyAnswer)
# explory <- left_join(MyAnswer, studentgpow)
# explory$gpow <- ifelse(is.na(value), 0.25, value)

# explory %>% filter(is.na(value)) %>% group_by(lectureId, lectureVersion) %>% summarise(n()) %>% View()
# explory %>% filter(!is.na(value)) %>% group_by(lectureId, lectureVersion) %>% summarise(n())
# explory %>% group_by(lectureId, lectureVersion, studentId) %>% summarise("count" = n_distinct(value)) %>% filter(count>1)
# naexplore <- explory %>% filter(any(is.na(value)))
# explory %>% filter(lectureId == '3203') %>% dplyr::select(lectureId, studentId, timeStart, value) %>% View()
# table(naexplore$title)
# explory  %>% group_by(lectureId, lectureVersion) %>%
#   summarise("check.na" = sum(is.na(value)), "check.notna" = sum(!is.na(value)))
#   filter(check.na > 0 & check.notna > 0)
# 
# explory %>% summarise(n_distinct(lectureId, lectureVersion))

#bæti við fjölda incorrect choices, eða nicc
MyAnswer$nicc <- nchar(gsub("[^0-9]+", "", MyAnswer$incorrectChoices))

#Set inn fjöldi svara framAd Thessu og hefséð áður
#MyAnswer <- MyAnswer[order(MyAnswer$timeStart),] %>% group_by(lectureId,studentId) %>% mutate(fsfat=row_number()-1)
MyAnswer <- MyAnswer %>% arrange(timeStart) %>% group_by(lectureId,studentId) %>% mutate(fsfat=row_number()-1)
hashAnswer <- inner_join(MyAnswer,hashes) %>%
  pivot_longer(c(hash,hash2,hash3),values_to = "hash") %>% filter(!is.na(hash))
# explory <- left_join(hashAnswer, studentgpow)
hashAnsdag <- hashAnswer %>% group_by(studentId,hash) %>% mutate('mindag'=min(timeStart))
#hashAnsdag <- full_join(hashAnswer,minstadagsetning)
#unique(hashAnswer$lectureId)

hashAnsdag$hsta <- ifelse(hashAnsdag$mindag==hashAnsdag$timeStart,0,1)
#hashAnsdag <- hashAnsdag %>% mutate("dtStart"=ymd_hms(.$timeStart),"dtEnd"=ymd_hms(.$timeEnd))
hashAnsdag$dtStart <- ymd_hms(hashAnsdag$timeStart)
hashAnsdag$dtEnd <- ymd_hms(hashAnsdag$timeEnd)
hashAnsdag <-  hashAnsdag %>% group_by(studentId, hash, timeStart) %>% arrange(studentId,hash, timeStart)
hashAnsdag$timeDif <- hashAnsdag %>% arrange(studentId, hash, timeStart) %$% 
  ifelse(hash==lag(hash,1),as.duration(dtStart-lag(dtEnd)),NA)

# ?lag
# View(hashAnsdag %>% arrange(studentId, hash, timeStart))

#setjum inn fsvfat sem er fjöldi svara fram að þessu
hashAnsdag$sumable <- ifelse(hashAnsdag$hsta==0,1,0)

hashAnsdag <- hashAnsdag %>% group_by(studentId,lectureId) %>% arrange(timeStart) %>% mutate(fsvfat=cumsum(sumable)-1)
hashAnsdag <- hashAnsdag %>% group_by(lectureId, studentId, timeStart) %>% mutate(fsvfatu = min(fsvfat))

hashAnsdag4 <- hashAnsdag[!grepl('NOTA+',hashAnsdag$hash),]
#unique(hashAnsdag$lectureId)
myAnswer %>% filter(lectureId == 3203) %>% summarise(n_distinct(studentId))
#Vel dálka og save-a
hashanswers <- hashAnsdag %>% dplyr::select(lectureId,studentId,questionId,correct,hash,fsfat,fsvfat, fsvfatu,hsta,timeDif, nicc, gpow)
hashanswers4 <- hashAnsdag4 %>% dplyr::select(lectureId,studentId,questionId,correct,hash,fsfat,fsvfat, fsvfatu,hsta,timeDif, nicc, gpow)
write.csv(hashanswers,'Data/hashAnswer.csv')
write.csv(hashanswers4,'Data/hashAnswer4.csv')

?dplyr::select
hhA <- hashAnsdag

hhA %>% filter(notaType != "AOTA+") %>%
  group_by(timeStart, studentId, lectureId) %>%
  summarise("n" = n()) %>% filter(n != 1) %>% 
  summarise(n())

hhA %>% filter(notaType == "AOTA+") %>%
  group_by(timeStart, studentId, lectureId) %>%
  summarise("n" = n()) %>% filter(n != 3) %>% 
  summarise(n())
  
hhA %>% filter(notaType == "AOTA+") %>% View()

#hugmynd til að reikna tímann milli taka
#fyrst er sort-að eftir studentId,hash og timestart. Í þessari röð. Svo að gera ifelse setningu sem reiknar
# timestart-lag(timeEnd), þetta ætti að virka með réttri röðunn, vonandi. Kannski spurning um hvernig við fáum þá raðaða rétt
# Svo er spurning um date, will líklegast fá muninn í min eða klst. Hvernig væri bæst að gera date1-date2 og fá það í mín
# Við sjáum til.
# https://stackoverflow.com/questions/33068340/how-to-get-difference-in-minutes-between-two-date-strings

tmp = unique(MyAnswer$lectureId)

filter(tibble(studentgpow), lectureId %in% tmp) %>% View()

MyAnswer %>% filter(lectureId == 3082) %>% View()
