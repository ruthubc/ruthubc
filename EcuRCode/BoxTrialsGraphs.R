# 
# Author: Ruth
###############################################################################

library(data.table)
library(ggplot2)
library(plyr)
library(nlme)

Trials <- read.csv("RuthEcuador2013/BoxFeedingTrials/Trials.csv")

Feeding <- read.csv("RuthEcuador2013/BoxFeedingTrials/Feeding.csv")

levels(Trials$Instar) <- gsub("1s", "Sub1", levels(Trials$Instar))
levels(Trials$Instar) <- gsub("2s", "Sub2", levels(Trials$Instar))

#only keeping a few field of Feeding
Feeding<-Feeding[c("TrialID", "OverallID", "SpiderID", "TotalTimeEating", 
				"IndCapture")]

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


 #########################################################################################
 ########graph of number of individuals and total duration vs prey size and instar########
 
setkey(TrialInfo, "TrialID")
 
setkey(Feeding, "TrialID")
 
FeedingMerge <-merge(TrialInfo, Feeding)

FeedingMerge$LogFeedDurP1 <- log(FeedingMerge$TotalTimeEating + 1)

###adding a field for total box feeding time and fraction of time feeding
FeedingMerge<-transform(FeedingMerge, 
		TotBoxEating = ave(TotalTimeEating, TrialID, 
				FUN = function(x) sum(x)))

FeedingMerge$FeedFraction <-
		FeedingMerge$TotalTimeEating/FeedingMerge$TotBoxEating

		
####removing evening feeds as no or little feeding observations
FeedingMorn <- subset(FeedingMerge, TimeOfDay.x =="morn" )

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
 
 ggplot((subset(FeedingMorn, FeedFraction >= 0)), aes(x=Treatment, y=FeedFraction)) + geom_boxplot() + 
		 stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
		 ggtitle("Fraction of time feeding by individual") + ylab("Fraction of time spent eating prey by each individual") +
		 xlab("Prey Size") + facet_wrap(~Instar) + scale_y_sqrt()
 
 ggplot((subset(FeedingMorn, FeedFraction > 0)), aes(x=Treatment, y=FeedFraction)) + geom_boxplot() + 
		 stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
		 ggtitle("Fraction of time feeding by individuals\nwith individuals who didn't feed removed") + 
		 ylab("Fraction of time spent eating prey by each individual") +
		 xlab("Prey Size") + facet_wrap(~Instar) + scale_y_sqrt()
 

 
dev.off()
#___________________________________________________________________________________#
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
x
FeedingWeights <- merge(Weights, Feeding)

###Ranking individuals for time eating
FeedingWeights<-transform(FeedingWeights, 
		Rank.TimeEating = ave(TotalTimeEating, TrialID, 
				FUN = function(x) rank(x, ties.method = "average")))

####Ranking individuals by weight
FeedingWeights<-transform(FeedingWeights, 
		Rank.Weights = ave(Weight.1, TrialID, 
				FUN = function(x) rank(x, ties.method = "average")))

####Ranking individuals by leg length
FeedingWeights<-transform(FeedingWeights, 
		Rank.Legs = ave(LegLen.mm, TrialID, 
				FUN = function(x) rank(x, ties.method = "average")))


####remove evening trials and moulted individuals
FeedingWeights <- subset(FeedingWeights, TimeOfDay == "morn")
FeedingWeights <- subset(FeedingWeights, Moulted. != "y")

####calculating fraction of time feeding
FeedingWeights<-transform(FeedingWeights, 
		TotBoxEating = ave(TotalTimeEating, TrialID, 
				FUN = function(x) sum(x)))

FeedingWeights$FeedFraction <-
		FeedingWeights$TotalTimeEating/FeedingWeights$TotBoxEating

###hunger levels head length / weight1
FeedingWeights$Hunger <- FeedingWeights$HeadLen.mm/ FeedingWeights$Weight.1

####calculating rank of hunger
FeedingWeights<-transform(FeedingWeights, 
		Rank.Hunger = ave(Hunger, TrialID, 
				FUN = function(x) sum(x)))

## perhaps need to get average of repeated trials

pdf("RuthEcuador2013/BoxFeedingTrials/Graphs/InitalWeights.pdf", onefile = "TRUE")

ggplot(FeedingWeights, aes(x = Rank.Weights, y = Rank.TimeEating)) +
		geom_point() + geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE) +
		ggtitle("Weight ranked within box vs time eating ranked within box ") + ylab("Rank of Time Eating") +
		xlab("Weight rank within box") + facet_wrap(~Treatment)

ggplot(FeedingWeights, aes(x = Rank.Weights, y = Rank.TimeEating, colour = Instar)) +
		geom_point() + geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE) +
		ggtitle("Weight ranked within box vs time eating ranked within box, colour is instar ") + ylab("Rank of Time Eating") +
		xlab("Weight rank within box") + facet_wrap(~Treatment)

ggplot(FeedingWeights, aes(x = Rank.Legs, y = Rank.TimeEating, colour = Treatment)) + 
		geom_point() + geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE) +
		ggtitle("Rank of leg eating vs rank of time eating") + ylab("Rank of time eating") + 
		xlab("Rank of leg length")

ggplot(FeedingWeights, aes(x= Hunger, y = TotalTimeEating, colour = Treatment)) + geom_point() +
		geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE) + 
		ggtitle("Total Time Eating against hunger level (head length / weight")

ggplot(FeedingWeights, aes(x= Hunger, y = TotalTimeEating, colour = Treatment)) + geom_point() +
		geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE) + 
		ggtitle("Total Time Eating against hunger level (head length / weight") + 
		facet_wrap(~Instar, scales = "free_x")

#ggplot(FeedingWeights, aes(x= Hunger, y = FeedFraction, colour = Treatment)) + geom_point() +
	#	geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE) + 
	#	ggtitle("Total Time Eating against hunger level (head length / weight") +
	#	facet_wrap(~Instar, scales = "free_x")

#TODO: hunger rank

dev.off()


SumarSpi <- ddply(Feeding, .(SpiderID, Spider), summarise, # need to discount trials where no feeding obs and eve
		N = length(!is.na(TotalTimeEating)),
		EatingTime.Mean = mean(TotalTimeEating, na.rm = TRUE)
)



######Regression testing#########################
fit <- lme(Rank.TimeEating ~ Rank.Weights, random=~1|IndBoxID, data=FeedingWeights)
summary(fit)
anova(fit)
plot(fit)
hist(resid(fit))

#####################################################################################
##Behaviour vs inital weight

ggplot(Weights, aes(x= AvePokeRating, y = Weight.1)) + geom_point() + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE) + 
		facet_wrap(~Instar, scales = "free_y")

ggplot(Weights, aes(x= AveBoldness, y = Weight.1)) + geom_point() + 
		facet_wrap(~Instar, scales = "free_y") + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE)


#######################################################################################
##comparing behaviours over time

## Poke consistancy between time periods

ggplot(data=Weights, aes(x=Poke.1, fill = as.factor(Poke.2))) +
		geom_bar(stat="bin", position="fill", colour = "black")
##Does not seem that consistant over time...I will try a stats test

pdf("RuthEcuador2013/BoxFeedingTrials/Graphs/BehaviourOverTime.pdf")

ggplot(data=Weights, aes(x= PokeRating.1, y = PokeRating.2)) + geom_jitter(position = position_jitter(w = 0.1, h = 0.1)) +
		geom_smooth(method = "lm", formula =y ~  poly(x, 2 , raw = TRUE), se = TRUE) +
		ggtitle("Poke rating of same spider at different times (jittered points)")

ggplot(data = Weights, aes(x = BoldnessRank.1, y = BoldnessRank.2)) + geom_jitter(position = position_jitter(w = 0.1, h = 0.1)) +
		geom_smooth(method = "lm", formula =y ~  poly(x, 2 , raw = TRUE), se = TRUE) +
		ggtitle("Boldness rank of same spider at different times (jittered points)")

dev.off()

ggplot(data= Weights, aes(x = as.factor(CupDrop.2), fill = as.factor(CupDrop.1))) + 
		geom_bar(stat="bin", position="fill", colour = "black")

ggplot(data= Weights, aes(x = as.factor(DropBox.2), fill = as.factor(BoxDrop.1))) + 
		geom_bar(stat="bin", position="fill", colour = "black")

#Calculating the difference between poke rating 1 and poke rating 2
Weights$PokeDifference <- Weights$PokeRating.1 - Weights$PokeRating.2

##Histogram of differences .. not very informative
ggplot(Weights, aes(x = PokeDifference)) + geom_bar()

##paired t-test
t.test(Weights$PokeRating.1, Weights$PokeRating.2, alternative = "two.sided", paired = TRUE)


#####################################################################################
##Behaviour vs feeding and capture


SubWeights<- subset(Weights, select = -c(Treatment, Instar))

# changing y and n to 0 and 1 for aves taking into account when feeding or capture not observed
TrialsFeeding$IndCapNum<- ifelse(TrialsFeeding$CaptureIndPos=="y", 1,
		ifelse(TrialsFeeding$CaptureIndPos =="n", 0, "NA"))

TrialsFeeding$IndFeedNum<- ifelse(TrialsFeeding$FeedIndPos=="y", 1,
		ifelse(TrialsFeeding$FeedIndPos =="n", 0, "NA"))



##merging TrialsFeeding and Weights
setkey(TrialsFeeding, SpiderID)
setkey(SubWeights, SpiderID)

FeedingWeights <- merge(SubWeights, TrialsFeeding, IndBoxID, DateCol)

FeedingWeights$IndCapNum <- as.numeric(FeedingWeights$IndCapNum)
FeedingWeights$IndFeedNum <- as.numeric(FeedingWeights$IndFeedNum)

##removing evening trials...perhaps only necessary if using duration of feeding
#FeedingWeights<- subset(FeedingWeights, TimeOfDay == 'morn')


FeedBehv <- ddply(FeedingWeights, .(SpiderID, Treatment, Instar, AveBoldness, AvePokeRating, Poke.1 ), summarise, 
		N = length(!is.na(SpiderID)),
		AveFeed = mean(IndFeedNum, na.omit=TRUE),
		AveCap = mean(IndCapNum, na.omit= TRUE),
		TotFeedDur = sum(TotalTimeEating, na.omit = TRUE),
		MaxFeed = max(IndFeedNum, na.omit = TRUE),
		MaxCap = max(IndCapNum, na.omit= TRUE)
)

FeedBehv$Move<- ifelse(FeedBehv$AveBoldness == 0, "n", 
		ifelse(FeedBehv$AveBoldness == "NA", "NA", "y")) 

FeedBehv$Feed<- ifelse(FeedBehv$AveFeed == 0, "n", 
		ifelse(FeedBehv$AveFeed > 0, "y", "NA")) 



###histogram of AveBoldness rating... maybe need to change classifactions
ggplot(FeedBehv, aes(x=AveBoldness)) + geom_histogram()

pdf("RuthEcuador2013/BoxFeedingTrials/Graphs/Behaviour.pdf")

ggplot(FeedBehv, aes(x= AveCap, y = AveBoldness)) + geom_jitter(position = position_jitter(w = 0.1, h = 0.1)) +
		geom_smooth(method = "lm", formula =y ~  poly(x, 1 , raw = TRUE), se = TRUE) +
		facet_wrap(~Instar) + ggtitle("Ave capture vs average boldness rating")

ggplot(FeedBehv, aes(x= AveCap, y = AvePokeRating)) + geom_jitter(position = position_jitter(w = 0.1, h = 0.1)) +
		geom_smooth(method = "lm", formula =y ~  poly(x, 1 , raw = TRUE), se = TRUE)+
		facet_wrap(~Instar) + ggtitle("Ave capture vs average poke rating")

ggplot(data=subset(FeedBehv, AveCap != "NA"), aes(x=Move, fill = as.factor(AveCap))) +
		geom_bar(stat="bin", position="fill", colour = "black") + 
		ggtitle("Move at all during boldness test with particitpated in capture")

ggplot(data=subset(FeedBehv, AveFeed != "NA"), aes(x=Move, fill = Feed)) +
		geom_bar(stat="bin", position="fill", colour = "black")  +
		ggtitle("Move at all during boldness test with eat at all")

dev.off()


ggplot(data=FeedBehv, aes(x=Poke.1, fill = as.factor(AveFeed))) +
		geom_bar(stat="bin", position="fill", colour = "black")

ggplot(data=FeedBehv, aes(x=Poke.1, fill = as.factor(AveCap))) +
		geom_bar(stat="bin", position="fill", colour = "black")
