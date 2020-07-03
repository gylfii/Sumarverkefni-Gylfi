library(dbplyr)
library(dplyr)
library(RMySQL)
library(tidyr)
library(MASS)
library(ggplot2)
library(magrittr)
library(lubridate)
library(modelr)
#les inn öll gögninn, nota mestalagi bara answer gagnasafnið
twDb<-src_mysql("tw_quizdb", host = "127.0.0.1", port = 3306, user = "gss24", password = "JuwofKWT2Ewc")
answer<-tbl(twDb,"answer")
question<-tbl(twDb,"question")
lectureStudentSetting<-tbl(twDb,"lectureStudentSetting")
#hashes<-read.csv("Data/bighashfile.txt",sep=" ",col.names = c("dir","qName","hash","numQ","notaType"))
hashes<-read.csv("Data/bighashfile.wide.txt",sep=" ",col.names = c("dir","qName","hash","hash2","hash3","numQ","notaType"),na.strings = ".")
negHashes <- read.csv("bigNegHash2.txt", sep = " ", na.strings = ".", col.names = c("dir", "val", "neghash", "qName"))
#pathQ<-unique(paste(hashes$dir,hashes$qName,sep=""))
plonePath<-unique(paste(hashes$dir,hashes$qName,sep=""))
plonePath<-as.data.frame(plonePath)
hashes$plonePath<-paste(hashes$dir,hashes$qName,sep="")
negHashes$plonePath <- paste(negHashes$dir, negHashes$qName, sep = "")
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

# MyAnswer %>% summarise(n(), n_distinct(studentId))

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

#Hér er ég með negative hashes reikning
negAnswer <- inner_join(MyAnswer, negHashes)
tmp <- negAnswer %>% group_by(lectureId, studentId, timeStart, nicc) %>% summarise("count" = n()) %>%
  filter(count > nicc)
negAnswer <- negAnswer[!(negAnswer$lectureId %in% tmp$lectureId &
                               negAnswer$studentId %in% tmp$studentId &
                               negAnswer$timeStart %in% tmp$timeStart &
                               negAnswer$neghash %in% c("tsnab", "tnab", "bext", "thext", "onull", "nynull", "innull")), ]


negAnswer$hsranga <- negAnswer %>% group_by(studentId, neghash) %>% mutate("negmindag" = min(timeStart)) %$%
  ifelse(negmindag == timeStart | neghash == "Nota-" | neghash == "Aota-", 0, 1)
MyAnswer <- negAnswer %>% group_by(lectureId, studentId, questionId,  correct, timeStart, timeEnd, lectureVersion, 
                                  incorrectChoices, plonePath, gpow, nicc, fsfat) %>% summarise("hluta" = mean(hsranga))

# lookit <- negAnswer %>% filter(lectureId %in% tmp$lectureId & studentId %in% tmp$studentId & timeStart %in% tmp$timeStart)
# tmp2 <- lookit %>% group_by(lectureId, studentId, timeStart, nicc) %>% summarise("count" = n()) %>% filter(count > nicc)
# tmp <- negAnswer %>% filter(neghash %in% c("tsnab", "tnab", "bext", "thext", "onull", "nynull", "innull"))
# lookit2 <- negAnswer %>% filter(lectureId %in% tmp2$lectureId & studentId %in% tmp2$studentId & timeStart %in% tmp2$timeStart)
# lookit2 %>% ungroup() %>% filter(neghash %in% c("tsnab", "tnab", "bext", "thext", "onull", "nynull", "innull")) %>% 
#   group_by(neghash) %>% summarise(n())
# 
# tmp %>% group_by(neghash) %>% summarise(n())
# ttmp <- negAnswer %>% group_by(lectureId, studentId, timeStart, nicc) %>% summarise("count" = n()) %>% filter(count > nicc)
# #Einu skiptin sem það er of mikið, er þegar eitthvað af c("tsnab", "tnab", "bext", "thext", "onull", "nynull", "innull") er notað
# negAnswer %>% group_by(lectureId, studentId, timeStart, nicc) %>% 
#   summarise("check" = any(neghash %in% c("tsnab", "tnab", "bext", "thext", "onull", "nynull", "innull")), "count" = n()) %>%
#   filter(!check & count > nicc)


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
hashAnsdag$hluta <- hashAnsdag %$% ifelse((notaType == "AOTA-" & hluta > 0) | 
                                            (notaType == "NOTA-" & hsta == 1), hluta + 1/nicc, hluta)

hashAnsdag4 <- hashAnsdag
hashAnsdag4$hsta <- ifelse(hashAnsdag4$hash == 'NOTA+', 
                           ifelse(hashAnsdag4$hluta >= 1, 1, 0), hashAnsdag4$hsta)
#hashAnsdag4 <- hashAnsdag[!grepl('NOTA+',hashAnsdag$hash),]
#unique(hashAnsdag$lectureId)
# myAnswer %>% filter(lectureId == 3203) %>% summarise(n_distinct(studentId))
#Vel dálka og save-a
hashanswers <- hashAnsdag %>% dplyr::select(lectureId,studentId,questionId,correct,hash,fsfat,fsvfat, fsvfatu,hsta, hluta,timeDif, nicc, gpow)
hashanswers4 <- hashAnsdag4 %>% dplyr::select(lectureId,studentId,questionId,correct,hash,fsfat,fsvfat, fsvfatu,hsta, hluta,timeDif, nicc, gpow)
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




#Hér er vinnsla sem er gerð þannig að NegHashes eru rétt sett upp
neghashes<-read.csv("Data/bigNegHash.txt", na.strings = ".", col.names = c("workaround"), fileEncoding = "UTF-8")
tryshort <- gsub("All of the above", "Aota-", neghashes$workaround)
tryshort <- gsub("None of the above", "Nota-", tryshort)
tryshort <- gsub("Ekkert ofangreint er rétt", "Nota-", tryshort)
tryshort <- gsub("Allt ofangreint er rétt", "Aota-", tryshort)
tryshort <- gsub("Allt ofangreint er rÃ©tt", "Aota-", tryshort)
tryshort <- gsub("Ekkert ofangreint er rÃ©tt", "Aota-", tryshort)
tryshort <- gsub("t-gildið til að prófa hvort skurðpunkturinn sé núll er  6.142  ", "tsnab", tryshort)
tryshort <- gsub("t-gildið til að prófa hvort hallinn sé núll er 7.932  ", "tnab", tryshort)
tryshort <- gsub("99 \\\\\\% öryggisbilið er breiðara en 95\\\\\\% öryggisbilið", "bext", tryshort)
tryshort <- gsub("90 \\\\\\% öryggisbilið er þrengra en 95\\\\\\% öryggisbilið", "thext", tryshort)
tryshort <- gsub("99 \\\\\\% öryggisbilið myndi ekki innihalda gildið núll", "onull", tryshort)
tryshort <- gsub("90 \\\\\\% öryggisbil myndi ekki innihalda gildið núll", "nynull", tryshort)
tryshort <- gsub("95 \\\\\\% öryggisbilið  inniheldur ekki gildið núll", "innull", tryshort)

fileConn<-file("bigNegHash2.txt")
writeLines(tryshort, fileConn)
close(fileConn)

neghashes2 <- read.csv("bigNegHash2.txt", sep = " ", na.strings = ".", col.names = c("dir", "val", "neghash", "qName"))
testout <- neghashes2[!grepl(".*)", neghashes2$val), ]
grepl(".*)", "a)", fixed = T)
