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
				"IndCapture", "TimeOfDay")]

TrialInfo <- Trials[c("TrialID", "Instar", "Treatment", "Day", "TimeOfDay", "SheetNo", 
				"BoxID", "DateCol", "DateTrial")]

#updates new field whether individual fed or not
Feeding$IndFeed <- ifelse (Feeding$TotalTimeEating > 0, "y", "n")



Feeding$IndFeed <- as.factor(Feeding$IndFeed)

Weights <- read.csv("RuthEcuador2013/BoxFeedingTrials/Weights.csv")

Weights$WeightDiff <- Weights$Weight.2 - Weights$Weight.1



##combining Trials and feeding tables using data.table
Feeding <- data.table(Feeding)
Trials <- data.table(Trials)
Weights <- data.table(Weights)
TrialInfo <- data.table(TrialInfo)


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


 #################################################################################
 ## graph of number of individuals and total duration vs prey size and instar
 
setkey(TrialInfo, "TrialID")
 
setkey(Feeding, "TrialID")
 
FeedingMerge <-merge(TrialInfo, Feeding)

FeedingMerge$LogFeedDurP1 <- log(FeedingMerge$TotalTimeEating + 1)

###adding a field for total box feeding time
FeedingMerge<-transform(FeedingMerge, 
		TotBoxEating = ave(TotalTimeEating, TrialID, 
				FUN = function(x) sum(x)))

FeedingMerge$FeedFraction <-
		FeedingMerge$TotalTimeEating/FeedingMerge$TotBoxEating
		
		

# remove evening feeds as no or little feeding observations

FeedingMorn <- subset(FeedingMerge, TimeOfDay =="morn" )

##Counting the number of individuals eating in each trial
EatCount <- ddply(FeedingMorn, .(TrialID, Treatment, Instar), summarise, 
		N = length(!is.na(IndFeed)),
		noFeed=length(SpiderID[IndFeed== "y"]),
		feedDur = sum(TotalTimeEating),
		logFeedDur = log(feedDur),
		logNoFeed = log(noFeed),
		meanFeedDur = mean(TotalTimeEating)
 )

 ##removing trials with no
 EatCount <- subset(EatCount, feedDur > 0)
 
###histograms
ggplot(EatCount, aes(x=freq, fill = Treatment)) + geom_histogram(binwidth =1)

## def not parametric	
ggplot(EatCount, aes(x= feedDur, fill = Treatment)) + geom_histogram(binwidth = 100)

ggplot(EatCount, aes(x= logFeedDur, fill = Treatment)) + geom_histogram(binwidth = 0.25)
	 
################# exporting graphs ###########################
pdf("RuthEcuador2013/BoxFeedingTrials/Graphs/NoAndDurationFeeding.pdf", onefile = "TRUE") 

 ##graph total number of individuals feeding vs prey size
 ggplot(EatCount, aes(x=Treatment, y=noFeed)) + geom_boxplot() + 
		 stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
		 ggtitle("Log of Total number of spiders fed on prey against prey size") +
		 xlab("Prey Size") + ylab("Total number of spiders feeding on prey")  + 
		 scale_y_log10()

 ggplot(EatCount, aes(x=Treatment, y=noFeed)) + geom_boxplot() + 
		 stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
		 ggtitle("Total number of spiders fed on prey against prey size") +
		 xlab("Prey Size") + ylab("Total number of spiders feeding on prey by instar") +
		 facet_wrap(~Instar)  + scale_y_log10()
 
 
 ##### total time eating vs prey
 
 ggplot(EatCount, aes(x=Treatment, y=feedDur)) + geom_boxplot() + 
		 stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
		 ggtitle("Log of Total amount of time feeding on prey per box") + ylab("Total time feeding (mins)") +
		 xlab("Prey Size") + scale_y_log10()

 
ggplot(EatCount, aes(x=Treatment, y=feedDur)) + geom_boxplot() + 
		 stat_summary(fun.y=mean, geom="point", shape=5, size=4) + facet_wrap(~Instar) + 
		 ggtitle("Log of Total amount of time feeding on prey per box by instar") + 
		 ylab("Total time feeding (mins)") + xlab("Prey Size") + scale_y_log10()
 
 
 ####graph of individual time eating vs prey size (and instar)
 
 ggplot((subset(FeedingMorn, FeedFraction > 0)), aes(x=Treatment, y=FeedFraction)) + geom_boxplot() + 
		 stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
		 ggtitle("Log of mean amount of time feeding on prey per box") + ylab("mean time feeding (mins)") +
		 xlab("Prey Size") + facet_wrap(~Instar) + scale_y_log10()
 
 
 
 ggplot(FeedingMorn, aes(x=LogFeedDurP1, fill = Treatment)) + geom_histogram()
 
 ggplot(subset(FeedingMorn, TotalTimeEating > 0), aes(x=Treatment, y=TotalTimeEating)) + 
		 geom_boxplot()
 
dev.off()
 
#stat Tests...might be a good idea to remove ones that only fed for 15mins

test<- subset(FeedingMorn, FeedFraction > 0)

t.test(EatCount$logNoFeed ~ EatCount$Treatment)  #only sig when remove all feeing < 30 mins
t.test(EatCount$feedDur ~ EatCount$Treatment)
t.test(FeedingMorn$FeedFraction ~ FeedingMorn$Treatment)
t.test(test$FeedFraction ~ test$Treatment)

##########################################################################################
##Feeding duration (rank?) vs weight rank

##need to combine small trials ......

setkey(Feeding, SpiderID)
setkey(Weights, SpiderID)

FeedingWeights <- merge(Weights, Feeding)

#####removing evening trials






###Ranking individuals for time eating

FeedingWeights<-transform(FeedingWeights, 
		Rank.TimeEating = ave(TotalTimeEating, TrialID, 
				FUN = function(x) rank(x, ties.method = "average")))

### need to check that that ranking worked 
Trials<- levels(FeedingWeights$TrialID)

subset(FeedingWeights, TrialID == Trials[51], 
		select= c(TotalTimeEating, Rank.TimeEating))[order(TotalTimeEating),] 


####Ranking individuals by weight

FeedingWeights<-transform(FeedingWeights, 
		Rank.Weights = ave(Weight.1, TrialID, 
				FUN = function(x) rank(x, ties.method = "average")))


subset(FeedingWeights, TrialID == Trials[51], 
		select= c(Weight.1, Rank.Weights))[order(Weight.1),] 




####remove evening trials


FeedingWeights <- subset(FeedingWeights, TimeOfDay == "morn")
FeedingWeights <- subset(FeedingWeights, Moulted. != "y")

table(FeedingWeights$Moulted.)
## perhaps need to get average of repeated trials

ggplot(FeedingWeights, aes(x = Rank.Weights, y = Rank.TimeEating, colour = Instar)) +
		geom_point() + geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE)

ggplot(FeedingWeights, aes(x = Rank.Weights, y = TotalTimeEating)) + 
		geom_point() + geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE) +
		facet_wrap(~Instar)

ggplot(FeedingWeights, aes(x = LegLen.mm, y = TotalTimeEating)) + 
		geom_point() + geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE) +
		facet_wrap(~Instar)


SumarSpi <- ddply(Feeding, .(SpiderID, Spider), summarise, # need to discount trials where no feeding obs and eve
		N = length(!is.na(TotalTimeEating)),
		EatingTime.Mean = mean(TotalTimeEating, na.rm = TRUE)
)



