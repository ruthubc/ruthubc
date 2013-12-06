# Author: Ruth
###############################################################################

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
######## Averages table combining different trials on same box    ###################
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


#####################################################################################
######## Averages table combining boxes  ###################
#####################################################################################

## only include day time boxes

EatCount <- ddply(subset(BoxCombo, TimeOfDay == "morn"), .(TrialID, Treatment, Instar), summarise, 
		N = length(!is.na(IndFeed)),
		noFeed=length(SpiderID[IndFeed== "y"]),
		feedDur = sum(TotalTimeEating),
		logFeedDur = log(feedDur),
		logNoFeed = log(noFeed),
		meanFeedDur = mean(TotalTimeEating)
)

##removing trials with no
EatCount <- subset(EatCount, feedDur > 0)

