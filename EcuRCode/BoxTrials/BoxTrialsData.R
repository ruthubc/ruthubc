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
library(reshape)

Trials <- read.csv("RuthEcuador2013/BoxFeedingTrials/Trials.csv", na.strings = NA)
Feeding <-read.csv("RuthEcuador2013/BoxFeedingTrials/Feeding.csv", na.strings = NA)
Weights <-read.csv("RuthEcuador2013/BoxFeedingTrials/Weights.csv", na.strings = NA)

#levels(Trials$Instar) <- gsub("1s", "Sub1", levels(Trials$Instar))
#levels(Trials$Instar) <- gsub("2s", "Sub2", levels(Trials$Instar))

#only keeping a few fields
Feeding<-subset(Feeding, select=c("TrialID", "OverallID", "SpiderID", "TotalTimeEating", 
				"IndCapture"))
Trials<- subset(Trials, select = c("TrialID", "Day", "TimeOfDay", "SheetNo", "DateTrial", 
				"BoxAtePrey", "BoxCapture"))
Weights<-subset(Weights, select=c("IndBoxID", "SpiderID", "Instar", "Treatment", "DateCol",
				"Poke.1", "Weight.1", "LegLen.mm", "AbdmLen.mm", "BodyLen.mm", "HeadLen.mm", 
				"Weight.2", "Poke.2", "Replaced.", "Moulted.", "PokeRating.1", "PokeRating.2",
				"AvePokeRating", "Climb.1", "BoldnessRank.1", "BoldnessRank.2", "AveBoldness"))

Weights$WeightDiff <- Weights$Weight.2 - Weights$Weight.1
Weights$Hunger <- Weights$HeadLen.mm/ Weights$Weight.1
Weights$Cond<- 1/Weights$Hunger
Weights$LogCond <- log10(Weights$Cond)
Weights$WeightDiffPer <- Weights$WeightDiff/Weights$Weight.1
Weights$LogWeight1 <- log(Weights$Weight.1)


##combining all tables
FeedingWeights <- merge(Feeding, Weights, by = c("SpiderID"))
BoxCombo <- merge(FeedingWeights, Trials, by = c("TrialID"))

##########################  RANKING and FEEDING FRACTION #####################################
##If removing NA's then need to check that all boxes have 10 individuals with  no NA's
#feeding fraction
BoxCombo<-transform(BoxCombo, TotBoxEating = ave(TotalTimeEating, TrialID, 
				FUN = function(x) sum(x)))

BoxCombo<-transform(BoxCombo, MinBoxHunger = ave(Hunger, TrialID, 
				FUN = function(x) min(x)))

BoxCombo<-transform(BoxCombo, DiffBoxHunger = ave(Hunger, TrialID, 
				FUN = function(x) max (x) - min(x)))

BoxCombo$RelHun <- (BoxCombo$Hunger - BoxCombo$MinBoxHunger) / BoxCombo$DiffBoxHunger

BoxCombo$RelCond <- 1- BoxCombo$RelHun

BoxCombo$BoxFeedObs <- as.factor(ifelse(BoxCombo$TotBoxEating > 30, "y", "n")) #change to 15mins?30mins?

# setting Total time eating to NA if TotBoxEating is NA
BoxCombo$TotalTimeEating <- ifelse(BoxCombo$BoxFeedObs == "y", BoxCombo$TotalTimeEating, NA)

##removing boxes from the feeding analysis if tot eating < 1hour
BoxCombo$FeedFraction <- BoxCombo$TotalTimeEating/BoxCombo$TotBoxEating

BoxCombo$ASFeedFrac <- asin(sqrt(BoxCombo$FeedFraction))


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

BoxCombo$Rank.Cond <- 11- BoxCombo$Rank.Hunger

# simpsons diversity index >> incorrect should use Pielou J's
BoxCombo$nn1 <-BoxCombo$TotalTimeEating * (BoxCombo$TotalTimeEating -1)
BoxCombo<-transform(BoxCombo, nn1Tot = ave(nn1, TrialID, FUN = function(x) sum(x)))
BoxCombo$Simpsons <- 1- (BoxCombo$nn1Tot/ (BoxCombo$TotBoxEating * (BoxCombo$TotBoxEating -1)))

BoxCombo$nDivN <- ((BoxCombo$TotalTimeEating/BoxCombo$TotBoxEating) ^2)
BoxCombo<-transform(BoxCombo, nDivN = ave(nDivN, TrialID, FUN = function(x) (1-sum(x))))


## Pielou's J index of evenness... start with shannon-weiner index
BoxCombo$SW_ind<- ifelse(BoxCombo$FeedFraction == 0, 0,
		log(BoxCombo$FeedFraction) * (BoxCombo$FeedFraction))
		
BoxCombo<-transform(BoxCombo, N_Spi = ave(as.numeric(SpiderID), TrialID, FUN = function(x) length(x)))

BoxCombo<-transform(BoxCombo, SW_Tot = ave(SW_ind, TrialID, FUN = function(x) sum(x)))

BoxCombo$PJEven<- -1 * (BoxCombo$SW_Tot/ log(BoxCombo$N_Spi))
BoxCombo$AsinPJEven <- asin(sqrt(BoxCombo$PJEven))

##Log Transform Hunger
BoxCombo$TimeEatingLog <- log(BoxCombo$TotalTimeEating)
BoxCombo$TimeEatingLog1 <- log(BoxCombo$TotalTimeEating + 1)
BoxCombo$LogHunger<- log10(BoxCombo$Hunger)

# Create logical arguments for treatment and 
BoxCombo$LogicalTreat<- ifelse(BoxCombo$Treatment == "large", 1, 0 )


################  Capture and eat including NAs  ######################################

BoxCombo$IndFeed <- as.factor(ifelse (BoxCombo$TotalTimeEating == "NA", NA, 
				ifelse(BoxCombo$TotalTimeEating > 0, "y", "n")))

BoxCombo$LogicalIndFeed <- ifelse(BoxCombo$IndFeed == "y", "1", ifelse(BoxCombo$IndFeed == "n", "0", NA))

Capture <- data.frame (IndCapture = c("y", "n", "n"), BoxCapture = c("y", "y", "n"), 
		CaptureIndPos = c("y", "n", NA))
BoxCombo <- merge(BoxCombo, Capture, by = (c("IndCapture", "BoxCapture")))


Feed <- data.table (IndFeed = c("y", "n", "n", NA), BoxFeedObs = c("y", "y", "n", "n"), 
		FeedIndPos = c("y", "n", NA, NA))
BoxCombo <- merge(BoxCombo, Feed, by= c("IndFeed", "BoxFeedObs"))

####### Capture and Eat combined field, needed to test whether  
#codes
#C+E
#NC+E
#C+NE
#NC+NE

FdAndCp <- data.table (CaptureIndPos = c("y", "n", "y", "n", NA, NA, "y", "n", NA), 
		FeedIndPos = c("y", "y", "n", "n", "n", "y", NA, NA, NA),
		CapAndFeed = c("C+E", "NC+E", "C+NE", "NC+NE", NA, NA, NA, NA, NA))
BoxCombo <- merge(BoxCombo, FdAndCp, by = c("CaptureIndPos", "FeedIndPos"))



# changing y and n to 0 and 1 for aves taking into account when feeding or capture not observed
BoxCombo$IndCapNum<- ifelse(BoxCombo$CaptureIndPos=="y", 1,
		ifelse(BoxCombo$CaptureIndPos =="n", 0, NA))

BoxCombo$IndFeedNum<- ifelse(BoxCombo$FeedIndPos=="y", 1,
		ifelse(BoxCombo$FeedIndPos =="n", 0, NA))


### Removing the evening trials from box combo

BoxComboMorn <- subset(BoxCombo, BoxCombo$TimeOfDay == "morn")

#####################################################################################
######## Averages table combining different trials on same box    ###################
#####################################################################################

BoxComboAve<- ddply(BoxComboMorn, .(SpiderID, Rank.Hunger, RelHun,RelCond, Rank.Cond, LogHunger, LogCond, Instar, Rank.Legs, IndBoxID,  Moulted., 
				AveBoldness, AvePokeRating, Treatment, Hunger, WeightDiffPer ), summarise, # need to discount trials where no feeding obs and eve
		N = length(!is.na(SpiderID)),
		IndEatDur.Mean = mean(TotalTimeEating, na.rm = TRUE),
		SumIndEat = sum(TotalTimeEating, na.rm = TRUE),
		RankEatDur.Mean = mean(Rank.TimeEating, na.rm = TRUE),
		AveFeed = mean(IndFeedNum, na.rm=TRUE),
		AveCap = mean(IndCapNum, na.rm= TRUE)
)
## checking that the averaging works
subset(as.data.frame(table(BoxComboAve$SpiderID)), Freq >1)

x1<- c(1,2,3,4,NA)
sum(x1, na.omit = TRUE)

###################### Behaviour summary #####################

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



#####################################################################################
############### Averages table by trial ID  ########################################
#####################################################################################

## only include day time boxes

AveByTrial <- ddply(subset(BoxCombo, TimeOfDay == "morn"), .(TrialID, Treatment, Instar, PJEven, AsinPJEven, IndBoxID ), summarise, 
		N = sum(!is.na(IndFeed)),
		noFeed=sum(!is.na(SpiderID[IndFeed== "y"])),
		noCap = sum(!is.na(SpiderID[IndCapture== "y"])),
		feedDur = sum(TotalTimeEating, na.rm =TRUE),
		logFeedDur = log(feedDur),
		logNoFeed = log(noFeed),
		meanFeedDur = mean(TotalTimeEating, na.rm= TRUE)
		
)

subset(as.data.frame(table(AveByTrial$TrialID)), Freq >1)

##removing trials with no feeding dur
AveByTrial <- subset(AveByTrial, feedDur > 0)


BoxMornFeedOnly<- subset(BoxComboMorn, IndFeed == "y" & CaptureIndPos != "NA" )

#averaging by spider as can't have spider as a random in lmer
BoxFeedAve<- ddply(BoxMornFeedOnly, .(SpiderID, TrialID, Rank.Hunger, RelHun,RelCond, Rank.Cond, LogHunger, LogCond, Instar, IndBoxID, 
				Treatment, Hunger), summarise, 
		AveFeed = mean(IndFeedNum, na.rm=TRUE),
		AveCap = mean(IndCapNum, na.rm= TRUE)
)
		
#getting ratio of caps vs non-caps (arbartuary number field needed)	
BoxMornFeedOnly<-transform(BoxMornFeedOnly, CountFeed = ave(Weight.1, IndCapture, TrialID, 
				FUN = function(x) length(x)))


BoxFeedRatio<- ddply(BoxMornFeedOnly, .(Treatment, TrialID, Instar, IndCapture, IndBoxID), summarise, # need to discount trials where no feeding obs and eve
		NumFeed= mean(CountFeed, na.rm = TRUE)
)

BoxFeedRatio<-reshape(BoxFeedRatio, timevar = "IndCapture", idvar = c("TrialID", "Treatment", "Instar", "IndBoxID" ), direction = "wide")


BoxFeedRatio$NumFeed.n<-ifelse(is.na(BoxFeedRatio$NumFeed.n), 0, BoxFeedRatio$NumFeed.n)
BoxFeedRatio$NumFeed.y<-ifelse(is.na(BoxFeedRatio$NumFeed.y), 0, BoxFeedRatio$NumFeed.y)


BoxFeedRatio$Tot<-BoxFeedRatio$NumFeed.n + BoxFeedRatio$NumFeed.y
BoxFeedRatio$PerNoCap<- BoxFeedRatio$NumFeed.n/BoxFeedRatio$Tot
BoxFeedRatio$logCap.n <- log10(BoxFeedRatio$NumFeed.n+1)
