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

#prufa að joina nokkuð saman
hashes$plonePath <- paste(hashes$dir,hashes$qName,sep="")
test <- right_join(miniQ,hashes)

quest <- as.data.frame(question)
test2 <- right_join(quest,hashes)

test2 %>%dplyr::select(questionId) %>% duplicated() %>% sum
hasht %>%dplyr::select(plonePath) %>% duplicated() %>% sum

hasht <-hashes[-grep("AOTA+",hashes$notaType),]

answer <- as.data.frame(answer)

prufa <- inner_join(answer,test2)
prufa <- prufa %>% dplyr::select(answerId,lectureId,studentId,questionId,correct,timeStart,V3,V4,V5)

prufa2 <- inner_join(MyAnswer,hashes)

#Sést að prufa er ekki alveg rétt, ekki nægilegt að gera join með questionId sem eini lykillinn, þarf fleiri
glimpse(prufa)
summary(prufa)