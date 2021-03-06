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


mytheme <-theme_bw(base_size=10)  + theme(plot.title = element_text(vjust=2), plot.margin=unit(c(0.08, 0.08, 0.0, 0.08),"cm"), 
				axis.title.y = element_text(vjust=0.50),
				axis.line = element_line(colour = "grey6", linetype=1, size = 0.3), panel.border = element_blank(), 
				panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  theme(strip.background = element_rect(fill = 'white'))


setwd("G:/Dropbox/")

setwd("C:/Users/Ruth/Dropbox/")

#Trials <- read.csv("RuthEcuador2013/BoxFeedingTrials/Trials.csv", na.strings = NA)
Trials <- read.csv("RuthEcuador2013/BoxFeedingTrials/TrailsWithOrgNest.csv", na.strings = NA)
Feeding <-read.csv("RuthEcuador2013/BoxFeedingTrials/Feeding.csv", na.strings = NA)
Weights <-read.csv("RuthEcuador2013/BoxFeedingTrials/Weights.csv", na.strings = NA)

#levels(Trials$Instar) <- gsub("1s", "Sub1", levels(Trials$Instar))
#levels(Trials$Instar) <- gsub("2s", "Sub2", levels(Trials$Instar))

#only keeping a few fields
Feeding<-subset(Feeding, select=c("TrialID", "OverallID", "SpiderID", "TotalTimeEating", 
				"IndCapture"))
Trials<- subset(Trials, select = c("OrgNest", "TrialID", "Day", "TimeOfDay", "SheetNo", "DateTrial", 
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

######### Scaling condition for instars so can plot together! ##########

Sub1Weights <- subset(Weights, Instar =="Sub1", select = c("SpiderID", "LogCond"))
Sub1Weights$Cond.Scal <- scale(Sub1Weights$LogCond,  center = TRUE, scale = TRUE)[,1]
Sub2Weights <- subset(Weights, Instar =="Sub2", select = c("SpiderID", "LogCond"))
Sub2Weights$Cond.Scal <- scale(Sub2Weights$LogCond,  center = TRUE, scale = TRUE)[,1]
Scaled <- rbind(Sub2Weights, Sub1Weights)
Scaled$LogCond <- NULL
Weights <- merge(Scaled, Weights, by = c("SpiderID"))


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

BoxComboAve<- ddply(BoxComboMorn, .(SpiderID, Rank.Hunger, RelHun,RelCond, Rank.Cond, LogHunger, LogCond, Instar, Rank.Legs, IndBoxID,  OrgNest, Moulted., 
				AveBoldness, AvePokeRating, Treatment, Hunger, WeightDiffPer, Cond.Scal ), summarise, # need to discount trials where no feeding obs and eve
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

AveByTrial <- ddply(subset(BoxCombo, TimeOfDay == "morn"), .(TrialID, OrgNest, Treatment, Instar, PJEven, AsinPJEven, IndBoxID ), summarise, 
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
BoxFeedAve<- ddply(BoxMornFeedOnly, .(SpiderID, OrgNest, TrialID, Rank.Hunger, RelHun,RelCond, Rank.Cond, LogHunger, LogCond, Instar, IndBoxID, 
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


BoxMornFeedOrCap<- subset(BoxComboMorn, (IndFeed == "y" & CaptureIndPos != "NA") | (IndFeed !="NA" & CaptureIndPos=="y") )

SubAveByTrial <-subset(AveByTrial, TrialID != "T3") #taking out T3 as NA's etc.



BoxMornFeedOrCap$CapAndFeed <- as.factor(BoxMornFeedOrCap$CapAndFeed)

BoxMornFeedOrCap$CapAndFeed2 <- factor(BoxMornFeedOrCap$CapAndFeed, levels = c("C+NE", "NC+E", "C+E")) # changing the order for the graph


AveFdOrCap <- ddply(BoxComboMorn, .(TrialID, Treatment, IndBoxID, Instar), summarise, 
		CapNoEat =sum(!is.na(SpiderID[CapAndFeed== "C+NE"])),
		NoCapEat = sum(!is.na(SpiderID[CapAndFeed== "NC+E"])),
		CapEat = sum(!is.na(SpiderID[CapAndFeed== "C+E"])),
		NoCapNoEat = sum(!is.na(SpiderID[CapAndFeed== "NC+NE"]))


)

AveFdOrCap$PropCheat <- AveFdOrCap$NoCapEat/ (AveFdOrCap$NoCapEat+ AveFdOrCap$CapEat)

AveFdOrCap$LogPropCheat <- asin(sqrt(AveFdOrCap$PropCheat))

ggplot(AveFdOrCap, aes(x = LogPropCheat, fill = Treatment)) + geom_histogram()

table(BoxComboMorn$CapAndFeed)

FdCapByTrial<- melt(AveFdOrCap, id = c("TrialID", "Treatment", "IndBoxID", "Instar"))

FdCap_labeller <- function(var, value){
	value <- as.character(value)
	if (var == "variable" ){
		value[value == "CapNoEat"] <- "Capture Not Eat"
		value[value == "NoCapEat"] <- "No Capture Eat"
		value[value == "CapEat"] <- "Capture And Eat"
		value[value == "NoCapNoEat"] <- "No Capture No Eat"
	}
	return(value)
}

#FdCapByTrial$name <- FdCap_labeller('variable', FdCapByTrial$variable)


FdCap_Capture <- function(var, value){
	value <- as.character(value)
	if (var == "variable" ){
		value[value == "CapNoEat"] <- "Captured"
		value[value == "NoCapEat"] <- "Did Not Capture"
		value[value == "CapEat"] <- "Captured"
		value[value == "NoCapNoEat"] <- "Did Not Capture"
	}
	return(value)
}


FdCap_Eat <- function(var, value){
	value <- as.character(value)
	if (var == "variable" ){
		value[value == "CapNoEat"] <- "Did Not Feed"
		value[value == "NoCapEat"] <- "Fed"
		value[value == "CapEat"] <- "Fed"
		value[value == "NoCapNoEat"] <- "Did Not Feed"
	}
	return(value)
}


FdCapByTrial$Cap <- FdCap_Capture('variable', FdCapByTrial$variable)
FdCapByTrial$Eat <- FdCap_Eat('variable', FdCapByTrial$variable)


################ Sample sizes #########


aggregate(IndBoxID ~ Treatment, AveByTrial, function(x) length(unique(x)))

aggregate(TrialID ~ Treatment, AveByTrial, function(x) length(unique(x)))

aggregate(IndBoxID ~ Treatment, subset(AveByTrial, noCap >0), function(x) length(unique(x)))

aggregate(TrialID ~ Treatment, subset(AveByTrial, noCap >0), function(x) length(unique(x)))


### Residual Condition for Histograme

Weights$logHead <- log10(Weights$HeadLen.mm)
Weights$logWt <- log10(Weights$Weight.1)

Weights<- subset(Weights, !is.na(logWt)& !is.na(logHead))

#calculating the residual index
#ggplot(Weights, aes(logHead, logWt)) + geom_point() + geom_smooth(method=lm)# + facet_wrap(~Instar,  scales = "free")
#ggplot(spiders, aes(lnLeg, lnWt)) + geom_point() + geom_smooth(method=lm, fullrange = TRUE) # log or nat log doesn't make a difference

model <- lm(logWt ~ logHead + Instar, Weights ) # doesn't matter which way round this is
# If instar is included in this model the interaction in prey capture is only just not significant. 
#model <- lm(logWt ~ logHead + Instar + logHead:Instar, spiders ) # whichever one i use doesn't make a difference

#visreg(model, xvar = "logHead", by = "Instar" )


#summary(model)
Weights$condResiduals <- resid(model) 




