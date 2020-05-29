
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


# #This here is a drastic over complication of the subject. Instead it is best to simply use glm directly
# #This is a working thing when used with studentId, glm is to big to do directly otherwises
# 
# fsvfat_model <- function(df) {
#   glm(correct ~ fsvfat * hsta, family = binomial(link = "logit"), data = df)
# }
# fsfat_model <- function(df) {
#   glm(correct ~ fsfat * hsta, family = binomial(link = "logit"), data = df)
# }
# 
# hashAnswer %>% group_by(lectureId) %>% summarise(n())
# 
# #Býr til fall sem hægt er að teikna sitthvort fall fyrir hvert lectureId
# Drawable_by_Id <- function(df, Id, fun) {
#   by_Id <- df %>% group_by(!!sym(Id)) %>%
#     nest() %>% 
#     filter(map_dbl(data, nrow) > 10)
#   by_Id <- by_Id %>% 
#     mutate(model = map(data, fun))
#   by_Id <- by_Id %>% 
#     mutate(pred = map(model, predict.glm, type = "response"))
#   Id_unested <- by_Id %>% 
#     unnest(data, pred)
#   return(Id_unested)
# }
# lecture_drawa <- Drawable_by_Id(hashAnswer, "lectureId", fsvfat_model)
# student_unested <- Drawable_by_Id(hashAnswer, "studentId", fsvfat_model)
# 
# lecture_drawa_fsfat <- Drawable_by_Id(hashAnswer, "lectureId", fsfat_model)
# 
# lecture_drawa %>%
#   ggplot(aes(x = fsvfat, y = correct, color = hsta)) +
#   geom_point() + 
#   geom_line(aes(y = pred)) + 
#   facet_wrap(vars(lectureId))
# 
# 
# lecture_drawa %>% filter(hsta == 0) %>%
#   ggplot(aes(x = fsvfat, y = correct, color = lectureId)) +
#   geom_point() + 
#   geom_line(aes(y = pred))
# 
# lecture_drawa_fsfat %>% filter(hsta == 0) %>%
#   ggplot(aes(x = fsfat, y = correct, color = lectureId)) +
#   geom_point() + 
#   geom_line(aes(y = pred))
# 
# student_unested %>% filter(hsta == 0, lectureId == 3214) %>%
#   ggplot(aes(x = fsvfat, y = correct)) +
#   geom_point() + 
#   geom_line(aes(y = pred, group = studentId)) + 
#   facet_wrap


# #Seems that the other method might be needed for the student drawing. Hmmmmmm
# 
# ans5 <- glm(correct ~ fsvfat*studentId, family = binomial(link = "logit"), data = hashAnswer)
# 
# hashAnswer %>% mutate(pred = predict.glm(ans5, type = "response")) %>%
#   filter(hsta == 0) %>%
#   ggplot(aes(x = fsvfat, y = correct, color = studentId)) + 
#   geom_point() + 
#   geom_line(aes(y = pred))