#require(sqldf)
#a1NotIna2 <- sqldf('SELECT * FROM BoxCombo2 EXCEPT SELECT * FROM BoxCombo3')
#missing <- sqldf('SELECT BoxCombo.* FROM BoxCombo INNER JOIN a1NotIna2 ON BoxCombo.OverallID = a1NotIna2.OverallID')
 # Author: Ruth
###############################################################################
###AS I HAVE CHANGED THINGS MAKE SURE THAT THE CORRECT ITEMS ARE PICKED MORNING ETC
#TODO: check errors in leg length
library(data.table)
library(ggplot2)
library(plyr)
library(nlme)

Trials <- read.csv("RuthEcuador2013/BoxFeedingTrials/Trials.csv", na.strings = NA)
Feeding <-read.csv("RuthEcuador2013/BoxFeedingTrials/Feeding.csv", na.strings = NA)
Weights <-read.csv("RuthEcuador2013/BoxFeedingTrials/Weights.csv", na.strings = NA)

levels(Trials$Instar) <- gsub("1s", "Sub1", levels(Trials$Instar))
levels(Trials$Instar) <- gsub("2s", "Sub2", levels(Trials$Instar))

Weights$WeightDiff <- Weights$Weight.2 - Weights$Weight.1
Weights$Hunger <- Weights$HeadLen.mm/ Weights$Weight.1

#only keeping a few fields
Feeding<-subset(Feeding, select=c("TrialID", "OverallID", "SpiderID", "TotalTimeEating", 
				"IndCapture"))
Trials<- subset(Trials, select = c("TrialID", "Day", "TimeOfDay", "SheetNo", "DateTrial", 
				"BoxAtePrey", "BoxCapture"))

##combining all tables
FeedingWeights <- merge(Feeding, Weights, by = c("SpiderID"))
BoxCombo <- merge(FeedingWeights, Trials, by = c("TrialID"))

##########################  RANKING and FEEDING FRACTION #####################################
##If removing NA's then need to check that all boxes have 10 individuals with  no NA's
#feeding fraction
BoxCombo<-transform(BoxCombo, TotBoxEating = ave(TotalTimeEating, TrialID, 
				FUN = function(x) sum(x)))

BoxCombo$BoxFeedObs <- as.factor(ifelse(BoxCombo$TotBoxEating > 60, "y", "n")) #change to 15mins?30mins?

# setting Total time eating to NA if TotBoxEating is NA
BoxCombo$TotalTimeEating <- ifelse(BoxCombo$BoxFeedObs == "y", BoxCombo$TotalTimeEating, NA)

##removing boxes from the feeding analysis if tot eating < 1hour
BoxCombo$FeedFraction <- BoxCombo$TotalTimeEating/BoxCombo$TotBoxEating
 
# time eating
BoxCombo<-transform(BoxCombo, Rank.TimeEating = ave(TotalTimeEating, 
				TrialID, FUN = function(x) rank(x, ties.method = "average", na.last = "keep")))
# weight
BoxCombo <- transform(BoxCombo, Rank.Weights = ave(Weight.1, TrialID, 
				FUN = function(x) rank(x, ties.method = "average", na.last = "keep")))
# leg length
BoxCombo <- transform(BoxCombo, Rank.Legs = ave(LegLen.mm, TrialID, 
				FUN = function(x) rank(x, ties.method = "average", na.last = "keep")))
# hunger
BoxCombo <- transform(BoxCombo, Rank.Hunger = ave(Hunger, TrialID, 
				FUN = function(x) rank(x, ties.method = "average", na.last = "keep")))

################  Capture and eat including NAs  ######################################

BoxCombo$IndFeed <- as.factor(ifelse (BoxCombo$TotalTimeEating > 0, "y", "n"))

Capture <- data.frame (IndCapture = c("y", "n", "n"), BoxCapture = c("y", "y", "n"), 
		 CaptureIndPos = c("y", "n", NA))
BoxCombo <- merge(BoxCombo, Capture, by = (c("IndCapture", "BoxCapture")))


Feed <- data.table (IndFeed = c("y", "n", "n", NA), BoxFeedObs = c("y", "y", "n", "n"), 
		FeedIndPos = c("y", "n", NA, NA))
BoxCombo <- merge(BoxCombo, Feed, by= c("IndFeed", "BoxFeedObs"))

# changing y and n to 0 and 1 for aves taking into account when feeding or capture not observed
BoxCombo$IndCapNum<- ifelse(BoxCombo$CaptureIndPos=="y", 1,
		ifelse(BoxCombo$CaptureIndPos =="n", 0, NA))

BoxCombo$IndFeedNum<- ifelse(BoxCombo$FeedIndPos=="y", 1,
		ifelse(BoxCombo$FeedIndPos =="n", 0, NA))

#####################################################################################
######## Averages table combining different trials (small trials) ###################
#####################################################################################

BoxComboAve<- ddply(BoxCombo, .(SpiderID, Rank.Weights, Instar, Rank.Legs, Moulted., 
				AveBoldness, AvePokeRating, Treatment, Hunger), summarise, # need to discount trials where no feeding obs and eve
		N = length(!is.na(SpiderID)),
		IndEatDur.Mean = mean(TotalTimeEating, na.rm = TRUE),
		SumIndEat = sum(TotalTimeEating),
		RankEatDur.Mean = mean(Rank.TimeEating, na.rm = TRUE),
		AveFeed = mean(IndFeedNum, na.omit=TRUE),
		AveCap = mean(IndCapNum, na.omit= TRUE)
)
## checking that the averaging works
subset(as.data.frame(table(BoxComboAve$SpiderID)), Freq >1)


################### Histograms ######################################################
##Looking at the total time eating in box. Need to remove maybe all records time < 1hour?
BoxSubset<-subset(BoxCombo, TimeOfDay == "morn")
BoxEating <- aggregate(BoxSubset, by = list(BoxSubset$TrialID), FUN = mean)
ggplot(BoxEating, aes(x= x)) + geom_histogram(binwidth = 15)

#########  Barplot Capture vs eating  ###################################################
#####################################################################################

CapVsEat <-subset(BoxCombo, select = c("FeedIndPos", "CaptureIndPos", "Treatment", "Instar") )
CapVsEat <-na.omit(CapVsEat)

pdf("RuthEcuador2013/BoxFeedingTrials/Graphs/CaptureVsFeed.pdf")

##separate bars
ggplot(data=CapVsEat, aes(x=FeedIndPos, fill = CaptureIndPos)) +
		geom_bar(stat="bin", position="fill", colour = "black") + 
		scale_x_discrete(breaks=c("y", "n"), labels=c("Fed", "Did Not Feed")) +
		theme(axis.text=element_text(colour="black"), axis.title = element_blank()) +
		scale_fill_discrete(name = "Involved with\nprey capture?", breaks = c("n", "y"),
				labels = c("No", "Yes")) + ggtitle("Prey Capture Vs Feeding")

CapVsEatSize <-subset( TrialsFeeding, select = c("FeedIndPos", "CaptureIndPos", "Treatment") )

##comparing the proportion of eaters and captures by TREATMENT
##separate bars
ggplot(data=CapVsEat, aes(x=FeedIndPos, fill = CaptureIndPos)) +
		geom_bar(stat="bin", position="fill", colour = "black") + 
		scale_x_discrete(breaks=c("y", "n"), labels=c("Fed", "Did Not Feed")) +
		theme(axis.text=element_text(colour="black"), axis.title = element_blank()) +
		scale_fill_discrete(name = "Involved with\nprey capture?", breaks = c("n", "y"),
				labels = c("No", "Yes")) + ggtitle("Prey Capture Vs Feeding by prey size") +
		facet_wrap(~Treatment)

ggplot(data=CapVsEat, aes(x=FeedIndPos, fill = CaptureIndPos)) +
		geom_bar(stat="bin", position="fill", colour = "black") + 
		scale_x_discrete(breaks=c("y", "n"), labels=c("Fed", "Did Not Feed")) +
		theme(axis.text=element_text(colour="black"), axis.title = element_blank()) +
		scale_fill_discrete(name = "Involved with\nprey capture?", breaks = c("n", "y"),
				labels = c("No", "Yes")) + ggtitle("Prey Capture Vs Feeding by prey size") +
		facet_wrap(~Instar)


dev.off()

#########################################################################################
 ########graph of number of individuals and total duration vs prey size and instar########
 
####removing evening feeds as no or little feeding observations
##Counting the number of individuals eating in each trial

pdf("RuthEcuador2013/BoxFeedingTrials/Graphs/NoAndDurationFeeding.pdf", onefile = "TRUE") 

 ##graph total number of individuals feeding vs prey size
 ggplot(EatCount, aes(x=Treatment, y=noFeed)) + geom_boxplot() + 
		 stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
		 ggtitle("Log of Total number of spiders fed on prey against prey size") +
		 xlab("Prey Size") + ylab("Total number of spiders feeding on prey")  + 
		 scale_y_log10()

#Number feeding vs treatment 
ggplot(EatCount, aes(x=Treatment, y=noFeed)) + geom_boxplot() + 
		 stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
		 ggtitle("Total number of spiders fed on prey against prey size") +
		 xlab("Prey Size") + ylab("Total number of spiders feeding on prey by instar") +
		 facet_wrap(~Instar)  + scale_y_log10() 
 
 ##### total box time eating vs prey
 ggplot(EatCount, aes(x=Treatment, y=feedDur)) + geom_boxplot() + 
		 stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
		 ggtitle("Log of Total amount of time feeding on prey per box") + ylab("Total time feeding (mins)") +
		 xlab("Prey Size") + scale_y_log10()
 
#total box time eating vs treatment by instar
ggplot(EatCount, aes(x=Treatment, y=feedDur)) + geom_boxplot() + 
		 stat_summary(fun.y=mean, geom="point", shape=5, size=4) + facet_wrap(~Instar) + 
		 ggtitle("Log of Total amount of time feeding on prey per box by instar") + 
		 ylab("Total time feeding (mins)") + xlab("Prey Size") + scale_y_log10() 
 
 ################################  Feeding fraction  #####################################
 
 ####graph of individual feeding fractionvs prey size and instar
 ggplot((subset(BoxCombo, FeedFraction > 0)), aes(x=Treatment, y=FeedFraction)) + geom_boxplot() + 
		 stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
		 ggtitle("Fraction of time feeding by individual (no eaters removed)") + ylab("Fraction of time spent eating prey by each individual") +
		 xlab("Prey Size")  + scale_y_log10()
 
# feeding fraction by instar
ggplot((subset(BoxCombo, FeedFraction > 0)), aes(x=Treatment, y=FeedFraction)) + geom_boxplot() + 
		 stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
		 ggtitle("Fraction of time feeding by individual (zero eaters removed)") + ylab("Fraction of time spent eating prey by each individual") +
		 xlab("Prey Size") + facet_wrap(~Instar) + scale_y_log10()
dev.off()

##########################################################################################
#stat Tests...might be a good idea to remove ones that only fed for 15mins

test<- subset(FeedingMorn, FeedFraction > 0)

t.test(EatCount$logNoFeed ~ EatCount$Treatment)  #only sig when remove all feeding < 30 mins
t.test(EatCount$feedDur ~ EatCount$Treatment)
t.test(FeedingMorn$FeedFraction ~ FeedingMorn$Treatment)
t.test(test$FeedFraction ~ test$Treatment)

##########################################################################################
##Feeding duration (rank?) vs weight rank

##need to combine small trials ......
####remove evening trials and moulted individuals


pdf("RuthEcuador2013/BoxFeedingTrials/Graphs/InitalWeights.pdf", onefile = "TRUE")

# weight rank vs rank eating duration by treatment
ggplot(BoxComboAve, aes(x = Rank.Weights, y = RankEatDur.Mean) +
		geom_point() + geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE) +
		ggtitle("Weight ranked within box vs time eating ranked within box ") + ylab("Rank of Time Eating") +
		xlab("Weight rank within box") + facet_wrap(~Treatment)

# leg length rank vs rank of eating duration 
ggplot(BoxComboAve, aes(x = Rank.Legs, y = RankEatDur.Mean, colour = Treatment)) + 
		geom_point() + geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE) +
		ggtitle("Rank of leg eating vs rank of time eating") + ylab("Rank of time eating") + 
		xlab("Rank of leg length")

# hunger by sum of time eating
ggplot(BoxComboAve, aes(x= Hunger, y = SumIndEat, colour = Treatment)) + geom_point() +
		geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE) + 
		ggtitle("Total Time Eating against hunger level (head length / weight")

# hunger by sum of time eating by instar
ggplot(BoxComboAve, aes(x= Hunger, y = SumIndEat, colour = Treatment)) + geom_point() +
		geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE) + 
		ggtitle("Total Time Eating against hunger level (head length / weight") + 
		facet_wrap(~Instar, scales = "free_x")


#TODO: hunger rank

dev.off()

#####################   Regression testing  #########################################
fit <- lme(Rank.TimeEating ~ Rank.Weights, random=~1|IndBoxID, data=FeedingWeights)
summary(fit)
anova(fit)
plot(fit)
hist(resid(fit))

#####################################################################################
#### Behavior vs physiological things

# Poke rating vs inital weight by instar
ggplot(Weights, aes(x= AvePokeRating, y = Weight.1)) + geom_point() + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE) + 
		facet_wrap(~Instar, scales = "free_y")

# Boldness vs inital weight by instar
ggplot(Weights, aes(x= AveBoldness, y = Weight.1)) + geom_point() + 
		facet_wrap(~Instar, scales = "free_y") + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE)

# Boldness vs hunger by instar
ggplot(Weights, aes(x= AveBoldness, y = Hunger)) + geom_point() + 
		facet_wrap(~Instar, scales = "free_y") + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE)

# Boldness vs leg length by instar
ggplot(Weights, aes(x= AveBoldness, y = LegLen.mm)) + geom_point() + 
		facet_wrap(~Instar) + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE)


#######################################################################################
#######  Behaviours over time #########################

# Poke time1 vs time2 barchart although NA's are included ANNOYING!!
ggplot(data=Weights, aes(x=Poke.1, fill = as.factor(Poke.2))) +
		geom_bar(stat="bin", position="fill", colour = "black")

pdf("RuthEcuador2013/BoxFeedingTrials/Graphs/BehaviourOverTime.pdf")

# Poke1 vs Poke2
ggplot(data=Weights, aes(x= PokeRating.1, y = PokeRating.2)) + geom_jitter(position = position_jitter(w = 0.1, h = 0.1)) +
		geom_smooth(method = "lm", formula =y ~  poly(x, 2 , raw = TRUE), se = TRUE) +
		ggtitle("Poke rating of same spider at different times (jittered points)")

# Boldness1 vs Boldness2
ggplot(data = Weights, aes(x = BoldnessRank.1, y = BoldnessRank.2)) + geom_jitter(position = position_jitter(w = 0.1, h = 0.1)) +
		geom_smooth(method = "lm", formula =y ~  poly(x, 2 , raw = TRUE), se = TRUE) +
		ggtitle("Boldness rank of same spider at different times (jittered points)")

dev.off()

#Bar chart of cup drop1 vs cup drop2
ggplot(data= Weights, aes(x = as.factor(CupDrop.2), fill = as.factor(CupDrop.1))) + 
		geom_bar(stat="bin", position="fill", colour = "black")
# Bar chart of box drop1 vs box drop 2
ggplot(data= Weights, aes(x = as.factor(DropBox.2), fill = as.factor(BoxDrop.1))) + 
		geom_bar(stat="bin", position="fill", colour = "black")



#####################################################################################
##Behaviour vs feeding and capture


BoxComboAve$Move<- factor(ifelse(BoxComboAve$AveBoldness > 0 , "y", 
		ifelse(BoxComboAve$AveBoldness == 0 , "n", NA))) 
BoxComboAve$Feed<- factor(ifelse(BoxComboAve$AveFeed == 0, "n", 
		ifelse(BoxComboAve$AveFeed > 0, "y", NA)))
BoxComboAve$Cap<- factor(ifelse(BoxComboAve$AveCap > 0, "y", 
		ifelse(BoxComboAve$AveCap == 0 , "n", NA)))

#changing order of factors
BoxComboAve$Move <- factor(BoxComboAve$Move, levels = c("y", "n") )
BoxComboAve$Feed <- factor(BoxComboAve$Feed, levels =  c("y", "n") )
BoxComboAve$Cap <- factor(BoxComboAve$Cap, levels = c("y", "n") )

pdf("RuthEcuador2013/BoxFeedingTrials/Graphs/Behaviour.pdf")

# Capture vs boldness by instar
ggplot(BoxComboAve, aes(x= AveCap, y = AveBoldness)) + geom_jitter(position = position_jitter(w = 0.1, h = 0.1)) +
		geom_smooth(method = "lm", formula =y ~  poly(x, 1 , raw = TRUE), se = TRUE) +
		facet_wrap(~Instar) + ggtitle("Ave capture vs average boldness rating")
# Capture vs poke by instar
ggplot(BoxComboAve, aes(x= AveCap, y = AvePokeRating)) + geom_jitter(position = position_jitter(w = 0.1, h = 0.1)) +
		geom_smooth(method = "lm", formula =y ~  poly(x, 1 , raw = TRUE), se = TRUE)+
		facet_wrap(~Instar) + ggtitle("Ave capture vs average poke rating")
# Move at all vs capture
ggplot(subset(BoxComboAve, Cap != "NA"), aes(x=Move, fill = Cap)) +
		geom_bar(stat="bin", position="fill", colour = "black") + 
		ggtitle("Move at all during boldness test vs particitpated in capture") +
		scale_x_discrete(breaks=c("y", "n"), labels=c("Moved", "Did Not Move")) +
		theme(axis.text=element_text(colour="black"), axis.title = element_blank()) +
		scale_fill_discrete(name = "Involved with\nprey capture?", breaks = c("n", "y"),
				labels = c("No", "Yes"))


ggplot(subset(BoxComboAve, Feed != "NA"), aes(x=Move, fill = Feed)) +
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
