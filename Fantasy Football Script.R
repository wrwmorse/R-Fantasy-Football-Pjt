setwd("C://Users//richw//Documents//Data Visualization")
f2019=read.csv("R_datafiles//2019.csv")
f2018=read.csv("R_datafiles//2018.csv")
f2017=read.csv("R_datafiles//2017.csv")
f2016=read.csv("R_datafiles//2016.csv")
f2015=read.csv("R_datafiles//2015.csv")
f2014=read.csv("R_datafiles//2014.csv")
f2013=read.csv("R_datafiles//2013.csv")

f2019$year <- '2019'
f2018$year <- '2018'
f2017$year <- '2017'
f2016$year <- '2016'
f2015$year <- '2015'
f2014$year <- '2014'
f2013$year <- '2013'



allf=rbind(f2019,f2018,f2017,f2016,f2015,f2014,f2013)
dim(allf)

head(allf)
allf=allf[,c(1,29,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28)]    
allf
colnames(allf)
 allf
colSums(is.na(allf)) #results show there are no NAs
unique(allf$Player)

aggregate(FantasyPoints~Player+Pos, allf, sum, na.rm = TRUE)
TopPts =aggregate(FantasyPoints~Player+Pos, allf, sum, na.rm = TRUE)
sort(TopPts$FantasyPoints, decreasing = TRUE)
TopPts = TopPts[order(TopPts$FantasyPoints, decreasing = TRUE),]
TopPts

ggplot(TopPts[1:10,],aes(x=reorder(Player,-FantasyPoints),y=FantasyPoints))+
  geom_bar(color = "Orange", fill = "purple", stat = "identity")

str(TopPts)

aggregate(FantasyPoints~Age, allf, sum, na.rm = TRUE)
Agepts =aggregate(FantasyPoints~Age, allf, sum, na.rm = TRUE)
sort(Agepts$FantasyPoints, decreasing = TRUE)
Agepts = Agepts[order(Agepts$FantasyPoints, decreasing = TRUE),]
Agepts

aggregate(FantasyPoints~Pos, allf, sum, na.rm = TRUE)
Pospts =aggregate(FantasyPoints~Pos, allf, sum, na.rm = TRUE)
sort(Pospts$FantasyPoints, decreasing = TRUE)
Pospts = Pospts[order(Pospts$FantasyPoints, decreasing = TRUE),]
Pospts

aggregate(FantasyPoints~Age+Pos, allf, sum, na.rm = TRUE)
Agepospts =aggregate(FantasyPoints~Age+Pos, allf, sum, na.rm = TRUE)
sort(Agepospts$FantasyPoints, decreasing = TRUE)
Agepospts = Agepospts[order(Agepospts$FantasyPoints, decreasing = TRUE),]
Agepospts  

ggplot(Agepospts[1:10,],aes(x=reorder(Age,-FantasyPoints),y=FantasyPoints))+
  geom_bar(color = "red", fill = "blue", stat = "identity")

ggplot(Pospts[1:10,],aes(x=reorder(Pos,-FantasyPoints),y=FantasyPoints))+
  geom_bar(color = "green", fill = "yellow", stat = "identity")

ggplot(Agepts[1:10,],aes(x=reorder(Age,-FantasyPoints),y=FantasyPoints))+
  geom_bar(color = "blue", fill = "red", stat = "identity")

head(allf$year)
