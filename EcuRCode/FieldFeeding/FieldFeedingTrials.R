# TODO: Add comment
# 
# Author: Ruth
###############################################################################
##Field feeding trials

library(plyr)
library(ggplot2)
library(reshape)

Fld_Feed <- read.csv("RuthEcuador2013/FieldFeedingTrials/FeedingData.csv", na.strings = NA)
Fld_Trials<-read.csv("RuthEcuador2013/FieldFeedingTrials/IndTrials.csv", na.strings = NA)
Fld_Prey <- read.csv("RuthEcuador2013/FieldFeedingTrials/PreyCapture.csv", na.strings = NA)
Fld_Census <- read.csv("RuthEcuador2013/FieldFeedingTrials/FieldFeedingCensus.csv", na.strings = NA)
Fld_TimeArr <- read.csv("RuthEcuador2013/FieldFeedingTrials/TimeFirstArrive.csv", na.strings = NA)
Fld_TrialTimes<- read.csv("RuthEcuador2013/FieldFeedingTrials/FeedingTrialsTimes.csv", na.strings = NA)

ggplot(Fld_Trials, aes(PreyLen.mm, fill = Size)) + geom_histogram()



Fld_Feed$Instar <- factor(Fld_Feed$Instar, levels =c("AdFem", "Sub2", "Sub1", "Juv34", "Juv12", "Male"))
Fld_TimeArr$Instar <- factor(Fld_TimeArr$Instar, levels =c("AdFem", "Sub2", "Sub1", "Juv34", "Juv12", "Male"))

Fld_Combo<- merge(Fld_Feed, Fld_Trials, by = (c("TrialID")))



# need proportion of total time

TotTimeByTrial<- ddply(Fld_Combo, .(TrialID), summarise, # need to discount trials where no feeding obs and eve
		TotTimeAllInd = sum(TotalTimeFeed)
)

Fld_Combo<- merge(TotTimeByTrial, Fld_Combo, by = (c("TrialID")))

Fld_Combo$PropTimeFd <- Fld_Combo$TotalTimeFeed/ Fld_Combo$TotTimeAllInd

Fld_Combo$PropTimeArc <- asin(sqrt(Fld_Combo$PropTimeFd))

ggplot(Fld_Combo, aes(PropTimeArc)) + geom_histogram()

ggplot(Fld_Combo, aes(x= Instar, y = PropTimeArc)) + geom_boxplot() + facet_wrap(~Size, scales = "free_y")

ggplot(Fld_TimeArr, aes(MinsFstEat)) + geom_histogram()


ggplot(Fld_TimeArr, aes(x= Instar, y = MinsFstEat)) + geom_boxplot() + facet_wrap(~Size, scales = "free_y")


ggplot(DF1, aes(x = Rank, y = value, fill = variable)) + 
		geom_bar(stat = "identity")

#### transpose data

mdata <- melt(Fld_TrialTimes, id = c("TrialID", "Nest", "Instar"))

mdata$variable <- as.character(mdata$variable)

print(mdata$variable)

mdata$variable <- substring(mdata$variable, 2)

mdata$variable <- as.numeric(mdata$variable)


TotIndByTrialAndTime<- ddply(mdata, .(TrialID, variable), summarise, # need to discount trials where no feeding obs and eve
		TotNumInd = sum(value)#, na.rm = TRUE)
)

mdata<- merge(mdata, TotIndByTrialAndTime, by = (c("TrialID", "variable")))

mdata$prop <- mdata$value/mdata$TotNumInd

