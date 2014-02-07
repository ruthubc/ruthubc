# TODO: Add comment
# 
# Author: Ruth
###############################################################################


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
Weights$WeightDiffPer <- Weights$WeightDiff/Weights$Weight.1
Weights$LogWeight1 <- log(Weights$Weight.1)


##combining all tables
FeedingWeights <- merge(Feeding, Weights, by = c("SpiderID"))
BoxCombo <- merge(FeedingWeights, Trials, by = c("TrialID"))

BoxCombo<-transform(BoxCombo, TotBoxEating = ave(TotalTimeEating, TrialID, 
				FUN = function(x) sum(x)))

BMorn <- subset(BoxCombo, TimeOfDay == "morn")

BMorn$TrialID <- factor(BMorn$TrialID)

nlevels(BMorn$TrialID)

table(BMorn$Treatment, BMorn$Instar)