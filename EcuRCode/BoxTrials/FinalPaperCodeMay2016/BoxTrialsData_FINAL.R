# Author: Ruth
###############################################################################
library(plyr)
library(data.table)
library(lmerTest)
library(ggplot2)
require(tigerstats)

Trials <- read.csv("RuthEcuador2013/BoxFeedingTrials/Trials.csv", na.strings = NA)
Feeding <-read.csv("RuthEcuador2013/BoxFeedingTrials/Feeding.csv", na.strings = NA)
Weights <-read.csv("RuthEcuador2013/BoxFeedingTrials/Weights.csv", na.strings = NA)

Feeding<-subset(Feeding, select=c("TrialID", "OverallID", "SpiderID", "TotalTimeEating", 
				"IndCapture"))
Trials<- subset(Trials, select = c("TrialID", "Day", "TimeOfDay", "DateTrial", 
				"BoxAtePrey", "BoxCapture"))
Weights<-subset(Weights, select=c("IndBoxID", "SpiderID", "Instar", "Treatment", "DateCol",
				"Weight.1", "HeadLen.mm"))


Weights$RatioCond <- Weights$Weight.1/Weights$HeadLen.mm
Weights$LogRatioCond <- log10(Weights$RatioCond)

### Residual Condition

Weights$logHead <- log10(Weights$HeadLen.mm)
Weights$logWt <- log10(Weights$Weight.1)

model <- lm(logWt ~ logHead + Instar, Weights ) # doesn't matter which way round this is
# If instar is included in this model the interaction in prey capture is only just not significant. 
#model <- lm(logWt ~ logHead + Instar + logHead:Instar, Weights ) # whichever one i use doesn't make a difference


selection <- which(!is.na(Weights$RatioCond))
Weights$residCond <- NA
Weights$residCond[selection] <- model$residuals

######### Scaling condition for instars so can plot together! ##########
## If need to scale see old data r file

########### Making Box Combo ##########

FeedingWeights <- merge(Feeding, Weights, by = c("SpiderID"))
BoxCombo <- merge(FeedingWeights, Trials, by = c("TrialID"))

BoxCombo <- ddply(BoxCombo, "TrialID", transform, TotBoxEating = sum(TotalTimeEating))

BoxCombo$BoxFeedObs <- as.factor(ifelse(BoxCombo$TotBoxEating > 30, "y", "n")) #change to 15mins?30mins?

# setting individual Total time eating to NA if TotBoxEating is NA
BoxCombo$TotalTimeEating <- ifelse(BoxCombo$BoxFeedObs == "y", BoxCombo$TotalTimeEating, NA)

BoxCombo$FeedFraction <- BoxCombo$TotalTimeEating/BoxCombo$TotBoxEating




## Pielou's J index of evenness... start with shannon-weiner index
BoxCombo$SW_ind<- ifelse(BoxCombo$FeedFraction == 0, 0,
		log(BoxCombo$FeedFraction) * (BoxCombo$FeedFraction))
		
BoxCombo<-transform(BoxCombo, N_Spi = ave(as.numeric(SpiderID), TrialID, FUN = function(x) length(x)))

BoxCombo<-transform(BoxCombo, SW_Tot = ave(SW_ind, TrialID, FUN = function(x) sum(x)))

BoxCombo$PJEven<- -1 * (BoxCombo$SW_Tot/ log(BoxCombo$N_Spi))
BoxCombo$AsinPJEven <- asin(sqrt(BoxCombo$PJEven))


################  Capture and eat including NAs  ######################################

BoxCombo$IndFeed <- as.factor(ifelse (BoxCombo$TotalTimeEating == "NA", NA, 
				ifelse(BoxCombo$TotalTimeEating > 0, "y", "n")))

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

BoxCombo$FeedIndPos <- as.factor(BoxCombo$FeedIndPos)

### New Table only with morning Only 

BoxComboMorn <- subset(BoxCombo, BoxCombo$TimeOfDay == "morn")



########### Summary Tables ############

AveByTrial <- ddply(BoxComboMorn, .(TrialID, Treatment, Instar, PJEven, AsinPJEven, IndBoxID ), summarise, 
		N = sum(!is.na(IndFeed))
)

AveByTrial <- subset(AveByTrial, N > 0)


######## Function for comparative anova tests

RedVsFull_fun <- function(Text, RedModel, FullModel) {
	
	print(Text)	
	
	print(anova(RedModel, FullModel)) #very significant interaction effect
	
	print("AIC difference")
	print(AIC(FullModel) - AIC(RedModel))
	
	print("")
	
	
}

########### Function to print AIC's of models ######################

modelAIC <- function(model_list) {
	
	for (m in 1:length(model_list)){
		model <- model_list[[m]]
		print(paste("model count:", m))
	
		print(model@call)
		print(AIC(model))
	
		print("")
	}
	
	
}


#resdCondMd <- lmer(Instar ~ Instar, data = subset(Weights, !is.na(residCond)))
#resdCondMdRed <- lm(residCond ~ 1, data = subset(Weights, !is.na(residCond)))

#anova(resdCondMd)

