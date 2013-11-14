# TODO: Add comment
# 
# Author: Ruth
###############################################################################

library(data.table)
library(ggplot2)
library(plyr)

Trials <- read.csv("RuthEcuador2013/BoxFeedingTrials/Trials.csv")

Feeding <- read.csv("RuthEcuador2013/BoxFeedingTrials/Feeding.csv")

levels(Trials$Instar) <- gsub("1s", "Sub1", levels(Trials$Instar))
levels(Trials$Instar) <- gsub("2s", "Sub2", levels(Trials$Instar))

#only keeping a few field of Feeding
Feeding<-Feeding[c("TrialID", "OverallID", "SpiderID", "TotalTimeEating", 
				"IndCapture")]

TrialInfo <- Trials[c("TrialID", "Instar", "Treatment", "Day", "TimeOfDay" )]

#updates new field whether individual fed or not
Feeding$IndFeed <- ifelse (Feeding$TotalTimeEating > 0, "y", "n")



Feeding$IndFeed <- as.factor(Feeding$IndFeed)

Weights <- read.csv("RuthEcuador2013/BoxFeedingTrials/Weights.csv")

Weights$WeightDiff <- Weights$Weight.2 - Weights$Weight.1



##combining Trials and feeding tables using data.table
Feeding <- data.table(Feeding)
Trials <- data.table(Trials)
Weights <- data.table(Weights)

setkey(Trials, TrialID)

setkey(Feeding, TrialID)

TrialsFeeding <- merge(Trials, Feeding)



############################################################################################
###Lookup table for feeding, updating binary feeding and prey capture taking into account of
#whether feeding and capture was observed in the box
Capture <- data.table (IndCapture = c("y", "n", "n"), BoxCapture = c("y", "y", "n"), 
		 CaptureIndPos = c("y", "n", NA))

setkeyv(TrialsFeeding, c("IndCapture", "BoxCapture"))

setkeyv(Capture, c("IndCapture", "BoxCapture"))

TrialsFeeding<-merge(TrialsFeeding, Capture)

Feed <- data.table (IndFeed = c("y", "n", "n"), BoxFeedObs = c("y", "y", "n"), 
		FeedIndPos = c("y", "n", NA))

setkeyv(TrialsFeeding, c("IndFeed", "BoxFeedObs"))

setkeyv(Feed, c("IndFeed", "BoxFeedObs"))

TrialsFeeding<-merge(TrialsFeeding, Feed)

#####################################################################################
#Making barplot of capture vs feeding
CapVsEat <- data.frame(TrialsFeeding$FeedIndPos, TrialsFeeding$CaptureIndPos)

CapVsEat <-subset( TrialsFeeding, select = c("FeedIndPos", "CaptureIndPos") )

CapVsEat <-na.omit(CapVsEat)

test <- table(CapVsEat)

pdf("RuthEcuador2013/BoxFeedingTrials/Graphs/CaptureVsFeed.pdf")

##separate bars
ggplot(data=CapVsEat, aes(x=FeedIndPos, fill = CaptureIndPos)) +
		geom_bar(stat="bin", position="fill", colour = "black") + 
		scale_x_discrete(breaks=c("y", "n"), labels=c("Fed", "Did Not Feed")) +
		theme(axis.text=element_text(colour="black"), axis.title = element_blank()) +
		scale_fill_discrete(name = "Involved with\nprey capture?", breaks = c("n", "y"),
				labels = c("No", "Yes")) + ggtitle("Prey Capture Vs Feeding")



##100% stacked bar graph
ggplot(data=CapVsEat, aes(x=FeedIndPos, fill = CaptureIndPos)) + 
		geom_bar(stat="bin", position='fill')

##chi squared test of feeding vs capture
chisq.test(table(CapVsEat))



##comparing the proportion of eaters and captures between prey type

CapVsEatSize <-subset( TrialsFeeding, select = c("FeedIndPos", "CaptureIndPos", "Treatment") )

CapVsEatSize <-na.omit(CapVsEat) 

##separate bars
ggplot(data=CapVsEat, aes(x=FeedIndPos, fill = CaptureIndPos)) +
		geom_bar(stat="bin", position="fill", colour = "black") + 
		scale_x_discrete(breaks=c("y", "n"), labels=c("Fed", "Did Not Feed")) +
		theme(axis.text=element_text(colour="black"), axis.title = element_blank()) +
		scale_fill_discrete(name = "Involved with\nprey capture?", breaks = c("n", "y"),
				labels = c("No", "Yes")) + ggtitle("Prey Capture Vs Feeding by prey size") +
		facet_wrap(~Treatment)

#larger prey requires more individuals to capture it

dev.off()

###Chi squared test of capture vs eating
chisq.test(table(CapVsEat))

###################################################################################
###Ranking individuals for time eating

TrialsFeeding<-transform(TrialsFeeding, TimeEating.Rank = ave(TotalTimeEating, TrialID, 
				FUN = function(x) rank(x, ties.method = "average")))


TrialsFeeding<-transform(TrialsFeeding, TimeEating.Rank = ave(TotalTimeEating, TrialID, 
				FUN = function(x) rank(x, ties.method = "average")))

 ## getting average of repeated trials
 
 #need to include boxfeeding obs as well
 
 table(Feeding$SpiderID)
 
 SumarSpi <- ddply(Feeding, .(SpiderID, Spider), summarise, # need to discount trials where no feeding obs and eve
		 N = length(!is.na(TotalTimeEating)),
		 EatingTime.Mean = mean(TotalTimeEating, na.rm = TRUE)

 
 )

 #################################################################################
 ## graph of number of individuals that feed on each prey
 
 TrialInfo <- data.table(TrialInfo)
 Feeding <- data.table(Feeding)
 
 
setkey(TrialInfo, "TrialID")
 
setkey(Feeding, "TrialID")
 
FeedingMerge <-merge(TrialInfo, Feeding)

# remove evening feeds as no or little feeding observations

FeedingMorn <- subset(FeedingMerge, TimeOfDay =="morn" )

##Counting the number of individuals eating in each trial
EatCount <- ddply(FeedingMorn, .(TrialID, Treatment, Instar), summarise, 
		N = length(!is.na(IndFeed)),
		freq=length(SpiderID[IndFeed== "y"]),
		feedSum = sum(TotalTimeEating),
		logfeedSum = log(feedSum)
 )

 ##removing trials with no
 EatCount <- subset(EatCount, freq >0)
 
###histograms
ggplot(EatCount, aes(x=freq, fill = Treatment)) + geom_histogram(binwidth =1)

ggplot(EatCount, aes(x= logfeedSum, fill = Treatment)) + geom_histogram(binwidth = 0.25)
## def not parametric		 

ggplot(EatCount, aes(x= feedSum, fill = Treatment)) + geom_histogram()

 
pdf("") 

 ##graph total number of individuals feeding vs prey size
 ggplot(EatCount, aes(x=Treatment, y=freq)) + geom_boxplot() + 
		 stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
		 ggtitle("Total number of spiders fed on prey against prey size") +
		 xlab("Prey Size") + ylab("Total number of spiders feeding on prey")

 ggplot(EatCount, aes(x=Treatment, y=freq)) + geom_boxplot() + 
		 stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
		 ggtitle("Total number of spiders fed on prey against prey size") +
		 xlab("Prey Size") + ylab("Total number of spiders feeding on prey by instar") +
		 facet_wrap(~Instar)
 
 
 ##### total time eating vs prey
 
 ggplot(EatCount, aes(x=Treatment, y=feedSum)) + geom_boxplot() + 
		 stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
		 ggtitle("Total amount of time feeding on prey per box") + ylab("Total time feeding (mins)") +
		 xlab("Prey Size")

 
ggplot(EatCount, aes(x=Treatment, y=feedSum)) + geom_boxplot() + 
		 stat_summary(fun.y=mean, geom="point", shape=5, size=4) + facet_wrap(~Instar) + 
		 ggtitle("Total amount of time feeding on prey per box by instar") + 
		 ylab("Total time feeding (mins)") + xlab("Prey Size")
 

 
 
