# 
# Author: Ruth
###############################################################################
###AS I HAVE CHANGED THINGS MAKE SURE THAT THE CORRECT ITEMS ARE PICKED MORNING ETC
library(data.table)
library(ggplot2)
library(plyr)
library(nlme)

Trials <- read.csv("RuthEcuador2013/BoxFeedingTrials/Trials.csv")
Feeding <-read.csv("RuthEcuador2013/BoxFeedingTrials/Feeding.csv")
Weights <-read.csv("RuthEcuador2013/BoxFeedingTrials/Weights.csv")

levels(Trials$Instar) <- gsub("1s", "Sub1", levels(Trials$Instar))
levels(Trials$Instar) <- gsub("2s", "Sub2", levels(Trials$Instar))

Weights$WeightDiff <- Weights$Weight.2 - Weights$Weight.1
Weights$Hunger <- Weights$HeadLen.mm/ Weights$Weight.1

#only keeping a few fields
Feeding<-subset(Feeding, select=c("TrialID", "OverallID", "SpiderID", "TotalTimeEating", 
				"IndCapture"))
Trials<- subset(Trials, select = c("TrialID", "Day", "TimeOfDay", "SheetNo", "DateTrial", 
				"BoxAtePrey", "BoxFeedObs", "BoxCapture"))

##combining all tables
FeedingWeights <- merge(Feeding, Weights, by = c("SpiderID"))
BoxCombo <- merge(FeedingWeights, Trials, by = c("TrialID"))

##########################  RANKING and FEEDING FRACTION #####################################

#feeding fraction
BoxCombo<-transform(BoxCombo, TotBoxEating = ave(TotalTimeEating, TrialID, 
				FUN = function(x) sum(x)))
BoxCombo$FeedFraction <- BoxCombo$TotalTimeEating/BoxCombo$TotBoxEating

# time eating
BoxCombo<-transform(BoxCombo, Rank.TimeEating = ave(TotalTimeEating, 
				TrialID, FUN = function(x) rank(x, ties.method = "average")))
# weight
BoxCombo <- transform(BoxCombo, Rank.Weights = ave(Weight.1, TrialID, 
				FUN = function(x) rank(x, ties.method = "average")))
# leg length
BoxCombo <- transform(BoxCombo, Rank.Legs = ave(LegLen.mm, TrialID, 
				FUN = function(x) rank(x, ties.method = "average")))
# hunger
BoxCombo <- transform(BoxCombo, Rank.Hunger = ave(Hunger, TrialID, 
				FUN = function(x) rank(x, ties.method = "average")))

################  Capture and eat including NAs  ######################################

BoxCombo$IndFeed <- as.factor(ifelse (BoxCombo$TotalTimeEating > 0, "y", "n"))

###replacing mmm? with n.. perhaps I need to check out what is really going on!
BoxCombo$BoxFeedObs <- replace(BoxCombo$BoxFeedObs, BoxCombo$BoxFeedObs=="mmm?", "n")

Capture <- data.frame (IndCapture = c("y", "n", "n"), BoxCapture = c("y", "y", "n"), 
		 CaptureIndPos = c("y", "n", NA))
BoxCombo <- merge(BoxCombo, Capture, by = (c("IndCapture", "BoxCapture")))



Feed <- data.table (IndFeed = c("y", "n", "n"), BoxFeedObs = c("y", "y", "n"), 
		FeedIndPos = c("y", "n", NA))
BoxComboTest <- merge(BoxCombo, Feed, by= c("IndFeed", "BoxFeedObs"))


BoxCombo2<-subset(BoxCombo, select = c("TrialID", "SpiderID", "OverallID"))
BoxCombo3<-subset(BoxComboTest, select = c("TrialID", "SpiderID", "OverallID"))


require(sqldf)

a1NotIna2 <- sqldf('SELECT * FROM BoxCombo2 EXCEPT SELECT * FROM BoxCombo3')

missing <- sqldf('SELECT BoxCombo.* FROM BoxCombo INNER JOIN a1NotIna2 ON BoxCombo.OverallID = a1NotIna2.OverallID')




#####################################################################################


#########Barplot Capture vs eating###################################################
#####################################################################################

CapVsEat <- data.frame(BoxCombo$FeedIndPos, BoxCombo$CaptureIndPos)
CapVsEat <-subset(BoxCombo, select = c("FeedIndPos", "CaptureIndPos", "Treatment") )
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

CapVsEatSize <-subset( TrialsFeeding, select = c("FeedIndPos", "CaptureIndPos", "Treatment") )

CapVsEatSize <-na.omit(CapVsEat) 
##comparing the proportion of eaters and captures by TREATMENT
##separate bars
ggplot(data=CapVsEat, aes(x=FeedIndPos, fill = CaptureIndPos)) +
		geom_bar(stat="bin", position="fill", colour = "black") + 
		scale_x_discrete(breaks=c("y", "n"), labels=c("Fed", "Did Not Feed")) +
		theme(axis.text=element_text(colour="black"), axis.title = element_blank()) +
		scale_fill_discrete(name = "Involved with\nprey capture?", breaks = c("n", "y"),
				labels = c("No", "Yes")) + ggtitle("Prey Capture Vs Feeding by prey size") +
		facet_wrap(~Treatment)

dev.off()



 #########################################################################################
 ########graph of number of individuals and total duration vs prey size and instar########
 

		
####removing evening feeds as no or little feeding observations
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

 ##removing trials with no feeding
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
		 xlab("Prey Size")  + scale_y_sqrt()
 
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

##########################################################################################
#stat Tests...might be a good idea to remove ones that only fed for 15mins

test<- subset(FeedingMorn, FeedFraction > 0)

t.test(EatCount$logNoFeed ~ EatCount$Treatment)  #only sig when remove all feeing < 30 mins
t.test(EatCount$feedDur ~ EatCount$Treatment)
t.test(FeedingMorn$FeedFraction ~ FeedingMorn$Treatment)
t.test(test$FeedFraction ~ test$Treatment)

##########################################################################################
##Feeding duration (rank?) vs weight rank

##need to combine small trials ......




####remove evening trials and moulted individuals
FeedingWeights <- subset(FeedingWeights, TimeOfDay == "morn")
FeedingWeights <- subset(FeedingWeights, Moulted. != "y")





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


##Histogram of differences .. not very informative
ggplot(Weights, aes(x = PokeDifference)) + geom_bar()

##paired t-test
t.test(Weights$PokeRating.1, Weights$PokeRating.2, alternative = "two.sided", paired = TRUE)


#####################################################################################
##Behaviour vs feeding and capture

"
SubWeights<- subset(Weights, select = -c(Treatment, Instar))

# changing y and n to 0 and 1 for aves taking into account when feeding or capture not observed
TrialsFeeding$IndCapNum<- ifelse(TrialsFeeding$CaptureIndPos=="y", 1,
		ifelse(TrialsFeeding$CaptureIndPos =="n", 0, NA))

TrialsFeeding$IndFeedNum<- ifelse(TrialsFeeding$FeedIndPos=="y", 1,
		ifelse(TrialsFeeding$FeedIndPos =="n", 0, NA))



##merging TrialsFeeding and Weights
setkey(TrialsFeeding, SpiderID)
setkey(SubWeights, SpiderID)

FeedingWeights <- merge(SubWeights, TrialsFeeding)#, IndBoxID, DateCol)

FeedingWeights$IndCapNum <- as.numeric(FeedingWeights$IndCapNum)
FeedingWeights$IndFeedNum <- as.numeric(FeedingWeights$IndFeedNum)

##removing evening trials...perhaps only necessary if using duration of feeding
#FeedingWeights<- subset(FeedingWeights, TimeOfDay == 'morn')


FeedBehv <- ddply(FeedingWeights, .(SpiderID, Treatment, Instar, AveBoldness, AvePokeRating, Poke.1 ), summarise, 
		N = length(!is.na(SpiderID)),
		AveFeed = mean(IndFeedNum, na.omit=TRUE),
		AveCap = mean(IndCapNum, na.omit= TRUE),
		TotFeedDur = sum(TotalTimeEating, na.omit = TRUE)
)

FeedBehv$Move<- factor (ifelse(FeedBehv$AveBoldness > 0 , "y", 
		ifelse(FeedBehv$AveBoldness == 0 , "n", NA))) 

FeedBehv$Feed<- factor(ifelse(FeedBehv$AveFeed == 0, "n", 
		ifelse(FeedBehv$AveFeed > 0, "y", NA)) )

FeedBehv$Cap<- factor(ifelse(FeedBehv$AveCap > 0, "y", 
		ifelse(FeedBehv$AveCap == 0 , "n", NA)) )
#changing order of factors
FeedBehv$Move <- factor(FeedBehv$Move, levels = c("y", "n") )
FeedBehv$Feed <- factor(FeedBehv$Feed, levels =  c("y", "n") )
FeedBehv$Cap <- factor(FeedBehv$Cap, levels = c("y", "n") )

###histogram of AveBoldness rating... maybe need to change classifactions
ggplot(FeedBehv, aes(x=AveBoldness)) + geom_histogram()

pdf("RuthEcuador2013/BoxFeedingTrials/Graphs/Behaviour.pdf")

ggplot(FeedBehv, aes(x= AveCap, y = AveBoldness)) + geom_jitter(position = position_jitter(w = 0.1, h = 0.1)) +
		geom_smooth(method = "lm", formula =y ~  poly(x, 1 , raw = TRUE), se = TRUE) +
		facet_wrap(~Instar) + ggtitle("Ave capture vs average boldness rating")

ggplot(FeedBehv, aes(x= AveCap, y = AvePokeRating)) + geom_jitter(position = position_jitter(w = 0.1, h = 0.1)) +
		geom_smooth(method = "lm", formula =y ~  poly(x, 1 , raw = TRUE), se = TRUE)+
		facet_wrap(~Instar) + ggtitle("Ave capture vs average poke rating")

ggplot(data=subset(FeedBehv, AveCap != "NA"), aes(x=Move, fill = Cap)) +
		geom_bar(stat="bin", position="fill", colour = "black") + 
		ggtitle("Move at all during boldness test vs particitpated in capture") +
		scale_x_discrete(breaks=c("y", "n"), labels=c("Moved", "Did Not Move")) +
		theme(axis.text=element_text(colour="black"), axis.title = element_blank()) +
		scale_fill_discrete(name = "Involved with\nprey capture?", breaks = c("n", "y"),
				labels = c("No", "Yes"))


ggplot(data=subset(FeedBehv, AveFeed != "NA"), aes(x=Move, fill = Feed)) +
		geom_bar(stat="bin", position="fill", colour = "black")  +
		ggtitle("Move at all during boldness test with eat at all") +
		scale_x_discrete(breaks=c("y", "n"), labels=c("Moved", "Did Not Move")) +
		theme(axis.text=element_text(colour="black"), axis.title = element_blank()) +
		scale_fill_discrete(name = "Ate Food?", breaks = c("n", "y"),
				labels = c("No", "Yes"))


dev.off()


ggplot(data=FeedBehv, aes(x=Poke.1, fill = as.factor(AveFeed))) +
		geom_bar(stat="bin", position="fill", colour = "black")

ggplot(data=FeedBehv, aes(x=Poke.1, fill = as.factor(AveCap))) +
		geom_bar(stat="bin", position="fill", colour = "black")



############################################################################
#Difference in weights.. need percentage change in weight?

# (1) Feeding time vs weight change

ggplot()
