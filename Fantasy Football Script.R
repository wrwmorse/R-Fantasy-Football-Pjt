setwd("C://Users//richw//Documents//Data Visualization")
f2019=read.csv("R_datafiles//2019.csv")
f2018=read.csv("R_datafiles//2018.csv")
f2017=read.csv("R_datafiles//2017.csv")
f2016=read.csv("R_datafiles//2016.csv")
f2015=read.csv("R_datafiles//2015.csv")
f2014=read.csv("R_datafiles//2014.csv")
f2013=read.csv("R_datafiles//2013.csv")
Stad_Coord = read.csv("R_datafiles//NFL_Stadium_Coordinates.csv")

str(Stad_Coord)


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
allf = allf [allf$Pos !=0,]
dim(allf)
allf
colnames(allf)
colSums(is.na(allf)) #results show there are no NAs upon later inspection I found 0s to be an issue came back to fix this.
unique(allf$Player)


allf$TotalTDs = allf$PassingTD + allf$RushingTD + allf$ReceivingTD
allf$TotalYds = allf$PassingYds +allf$RushingYds +allf$ReceivingYds

colnames(allf)
head(allf)

library(ggplot2)
library(scales) 
library(RColorBrewer)
library(ggthemes)
library(plyr)
library(dplyr)
library(leaflet)
library(sqldf)
library(data.table)
library(leaflet)


aggregate(FantasyPoints~Player+Pos, allf, sum, na.rm = TRUE)
TopPts =aggregate(FantasyPoints~Player+Pos, allf, sum, na.rm = TRUE)
sort(TopPts$FantasyPoints, decreasing = TRUE)
TopPts = TopPts[order(TopPts$FantasyPoints, decreasing = TRUE),]
head(TopPts)

Top_RBs = allf[allf$Pos =="RB",]
Top_RBs =aggregate(FantasyPoints~Player+Pos, Top_RBs, sum, na.rm = TRUE)
aggregate(FantasyPoints~Player+Pos, Top_RBs, sum, na.rm = TRUE)
sort(Top_RBs$FantasyPoints, decreasing = TRUE)
Top_RBs = Top_RBs [order(Top_RBs$FantasyPoints, decreasing = TRUE),]

Top10_RBs = head(Top_RBs[order(Top_RBs$FantasyPoints, decreasing = TRUE),], n=10)

Top_QBs = allf[allf$Pos =="QB",]
Top_QBs =aggregate(FantasyPoints~Player+Pos, Top_QBs, sum, na.rm = TRUE)
sort(Top_QBs$FantasyPoints,decreasing = TRUE)
Top_QBs = Top_QBs [order(Top_QBs$FantasyPoints,decreasing = TRUE),]
head(Top_QBs)

Top10_QBs = head(Top_QBs[order(Top_QBs$FantasyPoints, decreasing = TRUE),], n=10)

Top_WRs = allf[allf$Pos =="WR",]
Top_WRs =aggregate(FantasyPoints~Player+Pos, Top_WRs, sum, na.rm = TRUE)
sort(Top_WRs$FantasyPoints,decreasing = TRUE)
Top_WRs = Top_WRs [order(Top_WRs$FantasyPoints,decreasing = TRUE),]
head(Top_WRs)

Top10_WRs = head(Top_WRs[order(Top_WRs$FantasyPoints, decreasing = TRUE),], n=10)

Top_TEs = allf[allf$Pos =="TE",]
Top_TEs =aggregate(FantasyPoints~Player+Pos, Top_TEs, sum, na.rm = TRUE)
sort(Top_TEs$FantasyPoints,decreasing = TRUE)
Top_TEs = Top_TEs [order(Top_TEs$FantasyPoints,decreasing = TRUE),]
head(Top_TEs)

Top10_TEs = head(Top_TEs[order(Top_TEs$FantasyPoints, decreasing = TRUE),], n=10)

aggregate(FantasyPoints~Player+Pos, allf, sum, na.rm = TRUE)
TopPts =aggregate(FantasyPoints~Player+Pos, allf, sum, na.rm = TRUE)
sort(TopPts$FantasyPoints, decreasing = TRUE)
TopPts = TopPts[order(TopPts$FantasyPoints, decreasing = TRUE),]

aggregate(FantasyPoints~Age, allf, sum, na.rm = TRUE)
Agepts =aggregate(FantasyPoints~Age, allf, sum, na.rm = TRUE)
sort(Agepts$FantasyPoints, decreasing = TRUE)
Agepts = Agepts[order(Agepts$FantasyPoints, decreasing = TRUE),]

aggregate(FantasyPoints~Pos, allf, sum, na.rm = TRUE)
Pospts =aggregate(FantasyPoints~Pos, allf, sum, na.rm = TRUE)
sort(Pospts$FantasyPoints, decreasing = TRUE)
Pospts = Pospts[order(Pospts$FantasyPoints, decreasing = TRUE),]
Pospts$Pct = c(Pospts$FantasyPoints / sum(Pospts$FantasyPoints)*100)

Pospts %>%
  group_by(Pos) %>%
  mutate( FantasyPoints / sum(FantasyPoints)*100)
Pospts

aggregate(FantasyPoints~Age+Pos, allf, sum, na.rm = TRUE)
Agepospts =aggregate(FantasyPoints~Age+Pos, allf, sum, na.rm = TRUE)
sort(Agepospts$FantasyPoints, decreasing = TRUE)
Agepospts = Agepospts[order(Agepospts$FantasyPoints, decreasing = TRUE),]

ggplot(TopPts[1:10,],aes(x=reorder(Player,-FantasyPoints),y=FantasyPoints))+
  geom_bar(color = "Orange", fill = "purple", stat = "identity")+
  labs(title = "Top players since 2013")+
  theme(axis.text = element_text(angle=45, hjust = 1))

ggplot(Agepts[1:10,],aes(x=reorder(Age,-FantasyPoints),y=FantasyPoints))+
  geom_bar(color = "blue", fill = "red", stat = "identity")+
  labs (title = "Top age scorers")

unique(allf$year)

AgePosptsI = allf %>%
  select (Pos, Age, FantasyPoints) %>%
  data.frame()


ggplot(AgePosptsI, aes(x = Pos, y=FantasyPoints, fill = Age, )) +
  geom_bar(stat = "identity", aes(fill = as.factor(Age)))+
  labs (title = "Fantasy points by age & Position")
  


library(ggalt)
ggplot(allf, aes(x=FantasyPoints, y=TotalTDs))+
  geom_point(aes(col=FantasyPoints, size=TotalTDs))+
  geom_smooth(method="loess", se=F)+
  ggtitle("Relationship between TDs and FantasyPoints")
 

ggplot(allf, aes(x=FantasyPoints, y=TotalYds))+
  geom_point(aes(col=FantasyPoints, size=TotalYds))+
  geom_smooth(method="loess", se=F)+
  ggtitle("Relationship between Yds and FantasyPoints")


  

AvgNFL= aggregate(AgePosptsI[,3],list(AgePosptsI$Pos, AgePosptsI$Age),mean) #Find the average points of an NFL POs over age in NFL

Avg_RB = AvgNFL[AvgNFL$Group.1 == "RB",]
Avg_QB = AvgNFL[AvgNFL$Group.1 == "QB",]  
Avg_WR = AvgNFL[AvgNFL$Group.1 == "WR",]
Avg_TE = AvgNFL[AvgNFL$Group.1 == "TE",]

ggplot(AvgNFL, aes(x=Group.2, y=x, colour=Group.1, label=Group.2))+
  geom_point()+geom_line(size=.75)+geom_text(aes(label=Group.2),hjust=-.5)+
  labs(title = "Avg Fantasy Pts by Age", x = "Age", y = "Fantasy Points") 

pie = ggplot(Pospts,aes(x="", y = FantasyPoints, fill = Pos))+
geom_bar(stat = "identity", width = 1)+
labs (Fill = "Pos",
        x=NULL, 
        Y=NULL, 
        title = "Distribution of Points by Position")+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())+
  scale_fill_brewer(palette = "Reds")


  pie+coord_polar("y", start = 1)+
  geom_text(aes(label = paste0(round(Pct), "%")), position = position_stack(vjust = 0.5))


  
colnames(Stad_Coord)[7]="Tm"
colnames(Stad_Coord)


library(sqldf)
all_Coord = sqldf("Select * FROM allf inner join Stad_Coord USING(Tm)")
all_Coord$Latitude = substr(all_Coord$Latitude,1,nchar(all_Coord$Latitude)-5)
all_Coord$Longitude = substr(all_Coord$Longitude,1,nchar(all_Coord$Longitude)-5)
head(all_Coord)
str(all_Coord)

all_Coord$Latitude = as.numeric(all_Coord$Latitude)
all_Coord$Longitude = as.numeric(all_Coord$Longitude)
all_Coord$Longitude = all_Coord$Longitude*(-1)

aggregate(FantasyPoints~Team+Latitude+Longitude, all_Coord, sum, na.rm = TRUE)
Tm_Fantasy_Pts =aggregate(FantasyPoints~Team+Latitude+Longitude, all_Coord, sum, na.rm = TRUE)
sort(Tm_Fantasy_Pts$FantasyPoints, decreasing = TRUE)
Tm_Fantasy_Pts = Tm_Fantasy_Pts[order(Tm_Fantasy_Pts$FantasyPoints, decreasing = TRUE),]

Map_Fantasy_Pts = leaflet() %>%
  addProviderTiles(providers$Wikimedia)%>%
  addCircleMarkers(lng = Tm_Fantasy_Pts$Longitude, lat = Tm_Fantasy_Pts$Latitude,
             popup = paste ("Fantasy Points",Tm_Fantasy_Pts$FantasyPoints),
             label= Tm_Fantasy_Pts$Team,
             radius = Tm_Fantasy_Pts$FantasyPoints/500,
             color= "green",
             opacity = .5)

Map_Fantasy_Pts




