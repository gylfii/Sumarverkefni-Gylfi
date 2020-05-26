
#þetta er það sem var ekki notað í byrjunarvinnsla

# óþarfi, chr er stortable en þetta hér er það ekki XXXXX
# #breytum dagsetningunum frá chr yfir í date
# nemandi13$timeStart <- nemandi13$timeStart %>% strptime("%Y-%m-%d %H:%M:%S")
# nemandi13$timeEnd <- nemandi13$timeEnd %>% strptime("%Y-%m-%d %H:%M:%S")

# fann betri leid XXXXX 
# #prufa ad tengja hve oft spurning kemur vid nemandan
# Fjoldispurninga <- nemandi13 %>% group_by(questionId) %>% summarise('fjoldi'=n())
# nemandimedsvor <- full_join(nemandi13,Fjoldispurninga)
# nemstytt <- nemandimedsvor %>% subset(select= -c(practice,coinsAwarded,ugQuestionGuid,lectureVersion))

# lecture<-tbl(twDb,"lecture") #inniheldur hvada lecture er hvar og version
# host<-tbl(twDb,"host") #inniheldur hvada host er hvad
# coinAward<-tbl(twDb,"coinAward") #segir til hvenar og hve mikid folk faer coins
# lectureGlobalSetting<-tbl(twDb,"lectureGlobalSetting") #einhverjar stillingar
# lectureQuestions<-tbl(twDb,"lectureQuestions") #numer spurningu og numer svars
# lectureSetting<-tbl(twDb,"lectureSetting") # hlutir fyrir nemendur i lectures
# lectureStudentSetting<-tbl(twDb,"lectureStudentSetting") #hvenar buid til og liklar
# subscription<-as.data.frame(tbl(twDb,"subscription"))


#fleiri erfiðleikar koma þegar unnið eru beint úr gagnasafninu
# #sma prufa með að vinna með gögninn beint
# prufann <- answer%>%filter(studentId%in%c(15))
# minstadagsetning <- prufann %>% group_by(questionId) %>% summarise('mindag'=min(timeStart))
# prufameddag <- full_join(prufann, minstadagsetning)
# prufs <- prufameddag %>% subset(select= -c(practice,coinsAwarded,ugQuestionGuid,lectureVersion))
# 
# prufameddag$hedsedAdur <- ifelse(prufameddag$timeStart==prufameddag$mindag,0,1)
# prufameddag$fjoldisvarafram <- prufameddag%>%with(ave(timeStart,1,lectureId,studentId,FUN = seq_along))



#Fyrir AnswerTofluGerd. Fyrst er gamla leiðin til að lesa inn efnið!
# answer <- tbl(twDb, "answer")
# answer <- answer%>% filter(timeStart>'2020-01-01 01:00:00')
# question <- tbl(twDb,"question")
# 
# #setja upp Svörinn sem við ætlum að skoða
# # hashes<-read.csv("Data/bighashfile.txt",sep=" ",col.names = c("dir","qName","hash","numQ","notaType"))
# hashes$plonePath <- paste(hashes$dir,hashes$qName,sep="")
# pathQ<-unique(paste(hashes$dir,hashes$qName,sep=""))
# question%>%filter(plonePath%in%pathQ)->myQuestions
# answer%>%filter(timeStart>"2020-01-01 00:01:01")->answerRed
# inner_join(answerRed,myQuestions) -> myAnswer
# as.data.frame(myAnswer) -> MyAnswer


#Hér er Töflugerð fyrir töflur 2 og 3, sem var ákveðið að nota ekki
# hashAnsdag2 <- hashAnsdag
# hashAnsdag2$hsta <- ifelse(hashAnsdag2$mindag==hashAnsdag2$timeStart | hashAnsdag2$hash=='NOTA+',0,1)
# 
# hashAnsdag3 <- hashAnsdag %>% group_by(lectureId,studentId,hash) %>% summarise('minNotadag'=min(timeStart)) %>%
#   full_join(hashAnsdag)
# hashAnsdag3$hsta <- ifelse(hashAnsdag3$mindag==hashAnsdag3$timeStart |
#                              (hashAnsdag3$hash=='NOTA+' & hashAnsdag3$minNotadag==hashAnsdag3$timeStart),0,1)

#Til að Save-a þær töflur
# hashanswers2 <- hashAnsdag2 %>% dplyr::select(lectureId,studentId,questionId,correct,hash,fsfat,hsta)
# hashanswers3 <- hashAnsdag3 %>% dplyr::select(lectureId,studentId,questionId,correct,hash,fsfat,hsta)
# write.csv(hashanswers2,'data/hashAnswer2.csv')
# write.csv(hashanswers3,'Data/hashAnswer3.csv')