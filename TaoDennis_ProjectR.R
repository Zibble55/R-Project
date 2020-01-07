#STAT 611 Project R#

dirOUT=("C:/Users/denni/Documents/STAT 611/");
sink(paste0(dirOUT,"TaoDennis_dt5108_ProjectR_Ouput.txt"));

##First I will define the file path as##

setwd("C:/Users/denni/Documents/STAT 611/R Project")
dirdata <- "C:/Users/denni/Documents/STAT 611/R Project"

##Question 1##
cat("q1\n");

#Part A
cat("q1a\n");

baseball2010<-read.csv("AL2010.csv")
kcol<-c("Player", "Team", "Pos", "G", "AB", "H", "HR")
baseball2010<-baseball2010[kcol]
head(baseball2010)

#Part B
cat("q1b\n");

baseball2014<-read.csv("AL2014.csv")
baseball2014<-baseball2014[kcol]
head(baseball2014)

#Part C
cat("q1c\n");

merge_2010_2014<-merge(baseball2010,baseball2014,by=c("Player","Team"))
# Players who appear in both datasets in the same team.

chr_baseballmerge<- baseball2010$Player %in% merge_2010_2014$Player

baseball2010new<-baseball2010[!chr_baseballmerge,]
head(baseball2010new)

#Part D
cat("q1d\n");

baseball2010new$Year<-2010
baseball2014$Year<-2014
# fullcol<-c("Player", "Team", "Pos", "G", "AB", "H", "HR","Year")
mergenew_2010_2014<-merge(baseball2010new,baseball2014,all=TRUE)
head(mergenew_2010_2014)

#Part E
cat("q1e\n");

teams<-read.fwf(("AL2014Teams.fwf"),
         width=c(30,3,-1,2,-1,2,-1,5,-1,2,-1,7),
         skip=1)

teams<-teams[1:2]
colnames(teams)<-c("TeamName","Team")
teams$Team<-as.character(teams$Team)
rightL<-c("TBR","KCR","CHW")

teams[,"Team"][(teams[,"Team"]%in%rightL)]<- c("TB","KC","CWS")

newmerge<-merge(mergenew_2010_2014,teams,by=c("Team"))

head(newmerge)

#Some observations lost because team acronym are different between two files. Wrote in code in the teams dataset in
#order to keep all the players. Following Dr. Pham's suggestions, I changed the team acronymns so that they are the 
#same between the two files.

#Part F
cat("q1f\n");

newmerge$Average<-newmerge$H/newmerge$AB
head(newmerge)

#Part G
cat("q1g\n");

library("plyr")

fMeans<-ddply(newmerge,.(Year), numcolwise(mean))
fSds<-ddply(newmerge,.(Year), numcolwise(sd))
fMedians<-ddply(newmerge,.(Year), numcolwise(median))
fMeans2<-cbind(Summary="mean",fMeans)
fSds2<-cbind(Summary="std",fSds)
fMedians2<-cbind(Summary="median",fMedians)
fBoth2<-rbind(fMeans2,fSds2,fMedians2)
fBoth2<-fBoth2[order(fBoth2$Year),]
row.names(fBoth2)<-1:nrow(fBoth2) # cleaner look
fBoth2<-fBoth2[1:3]
fBoth2

#Part H
cat("q1h\n");

gMeans<-ddply(newmerge,.(Pos), numcolwise(mean))
gMeans2<-cbind(Summary="mean",gMeans)
gMeans2<-gMeans2[order(gMeans2$HR,decreasing = TRUE),]
row.names(gMeans2)<-1:nrow(gMeans2) # cleaner look
gcols<-c("Summary",'Pos',"HR")
gMeans2<-gMeans2[gcols]
gMeans2

#Part I
cat("q1i\n");

barplot(height=gMeans2$HR,names.arg=gMeans2$Pos,main = "Average Home Runs at each Position",
                      xlab="Position",ylab="Amount of Home Runs",ylim = c(0,25))

##Question 2##
cat("q2\n");

#Section 1
cat("q2.1\n");

#Part A
cat("q2.1a\n");

aqua<-read.csv("aquastat.csv",nrows=200)

#Part B
cat("q2.1b\n");

relcol<-c("X","X.1","Agricultural.water.withdrawal..10.9.m3.year.","Industrial.water.withdrawal..10.9.m3.year.",
          "Municipal.water.withdrawal..10.9.m3.year.","Total.water.withdrawal..10.9.m3.year.",
          "Total.water.withdrawal.per.capita..m3.inhab.year.")
aqua<-aqua[relcol]
colnames(aqua)<-c("Country","GlobalReigon","Agricultural","Industrial","Municipal","Total","Total per capita")
#Part C
cat("q2.1c\n");

str(aqua)

#Looks all fine to me

#Part D
cat("q2.1d\n");

aqua$GlobalReigon<-gsub("World \\| "," ",aqua$GlobalReigon)

aqua$Country<-gsub(" *\\(.*?\\) *","",aqua$Country)

#Section 2
cat("q2.2\n");
#Part A
cat("q2.2a\n");

pop<-read.delim("Data_Extract_From_World_Development_Indicators_Data.txt",na.strings = "..",quote = "")

colnames(pop)<-c("SeriesName","SeriesCode","Country","CountryCode","Value")

#Don't know why we want to change default quote in read.delim. Didn't make any difference.

#Part B
cat("q2.2b\n");

rowkeep<-grep("Agriculture|Population",pop$SeriesName)
pop<-pop[rowkeep,]

#Part C
cat("q2.2c\n");

pop<-pop[-2]

#Part D
cat("q2.2d\n");

library(tidyr)

pop1<-spread(pop,SeriesName,Value)

#Part E
cat("q2.2e\n");

pop1$Country<-gsub("(.*),.*","\\1",pop1$Country)
pop1$Country<-gsub(" *\\(.*?\\) *","",pop1$Country)

#Part F
cat("q2.2f\n");

str(pop1)

#All classes look fine to me. 

#Section 3
cat("q2.3\n");

aquapop<-merge(aqua,pop1,by=c("Country"))

head(aquapop)

#Part A
cat("q2.3a\n");

aquapop1<-merge(aqua,pop1,by=c("Country"),all.x= TRUE)

log_aquapop1<- is.na(aquapop1$CountryCode)
aquapop1<-aquapop1[log_aquapop1,]

aquapop1[1]

#Part B
cat("q2.3b\n");

aquapop2<-merge(aqua,pop1,by=c("Country"),all.y = TRUE)

log_aquapop2<- is.na(aquapop2$GlobalReigon)
aquapop2<-aquapop2[log_aquapop2,]

aquapop2[1]

#Part C
cat("q2.3c\n");
#United States of America was changed to United States in the aqua data set. Other countries were similarly changed.
#As requeseted by Dr.Pham, I went into the dataset and changed each of the requested data points indivisually.
#This new dataset is saved into the file aquastat2.csv. 
#Code from previous section copied, adjusted, then pasted below.


aqua1<-read.csv("aquastat2.csv",nrows=200)

relcol1<-c("X","X.1","Agricultural.water.withdrawal..10.9.m3.year.","Industrial.water.withdrawal..10.9.m3.year.",
          "Municipal.water.withdrawal..10.9.m3.year.","Total.water.withdrawal..10.9.m3.year.",
          "Total.water.withdrawal.per.capita..m3.inhab.year.")
aqua1<-aqua1[relcol]
colnames(aqua1)<-c("Country","GlobalReigon","Agricultural","Industrial","Municipal","Total","Total per capita")

aqua1$GlobalReigon<-gsub("World \\| "," ",aqua1$GlobalReigon)

aqua1$Country<-gsub(" *\\(.*?\\) *","",aqua1$Country)

pop2<-read.delim("Data_Extract_From_World_Development_Indicators_Data2.txt",na.strings = "..")

colnames(pop2)<-c("SeriesName","SeriesCode","Country","CountryCode","Value")
pop2<-pop2[rowkeep,]

pop2<-pop2[-2]

pop3<-spread(pop2,SeriesName,Value)

pop3$Country<-gsub("(.*),.*","\\1",pop3$Country)
pop3$Country<-gsub(" *\\(.*?\\) *","",pop3$Country)


naquapop<-merge(aqua1,pop3,by=c("Country"))

head(naquapop)

#There are also two Koreas in the aqua dataset.Ended up having to manually change the Koreas to North Korea and 
#South Korea in both data sets to split them properly. There was an overall increase of 16 observations, which makes
#sense as there are 15 from the countries listed in the problem and one extra because Korea was split into North and
#South.

#Part D
cat("q2.3d\n");


naquapop1<-merge(aqua1,pop3,by=c("Country"),all.x = TRUE)

log_naquapop1<- is.na(naquapop1$CountryCode)
naquapop1<-naquapop1[log_naquapop1,]

naquapop1[1]

naquapop2<-merge(aqua1,pop3,by=c("Country"),all.y = TRUE)

log_naquapop2<- is.na(naquapop2$GlobalReigon)
naquapop2<-naquapop2[log_naquapop2,]

naquapop2[1]

sink()
