# TODO: Add comment
# 
# Author: user
###############################################################################



library('plyr')
library("ggplot2")

spiderData <- read.csv("RuthEcuador2013/NestSize/CombinedNestVsWeight.csv")

#removing eggs, parastised individuals and the outlier nest 44.3ex01 as the adults were particularly small
spiders <- subset(spiderData, Instar != "FALSE" & NestID != "44.3ex01"  & Instar !="egg" & 
				Instar != "pj" & Instar != "PST" & Instar != "juv3")

##replacing ad males with sub bodies as sub males
spiders$Instar<-as.character(spiders$Instar) ; spiders$Instar[spiders$AdMaleSubBd == "y"] <- "SubMale"
spiders$Instar <-as.factor(spiders$Instar)

#Calculating spider hunger
spiders$condition <- spiders$Weight.mg/spiders$HeadLength.mm

#removing empty levels and ordering for graph
spiders$Instar <- factor(spiders$Instar, levels= c("Juv4", "Sub1", 
				"Sub2", "Adult", "SubMale", "AdMale"))

#changing to simplier name
spiders$type <- spiders$Approx..Single.

#log transforming
spiders$logCtFm <- log10(spiders$CountFemales)
spiders$logLeg<- log10(spiders$LegLen.mm)
spiders$logCond <- log10(spiders$condition)


# checking the normality with histograms
ggplot(spiders, aes(logCtFm)) + geom_histogram() + facet_wrap(~Instar) # not very normal but I don't think it would be

ggplot(spiders, aes(logLeg)) + geom_histogram() + facet_wrap(~Instar) # normal, awesome (but also normal before transformation)

ggplot(spiders, aes(logCond)) + geom_histogram() + facet_wrap(~Instar) # normal, awesome


## scaling to plot on one graph from stackoverflow.com/questions/15836452/data-table-aggregations-that-return-vectors-such-as-scale
spidersSub <- subset(spiders, select = c(Instar, logCond))

spiders <- as.data.frame(ddply(spiders,"Instar",transform, Cond.Scal=zScore(logCond)))

ggplot(spiders, aes(Cond.Scal)) + geom_histogram() + facet_wrap(~Instar) # checking that the condition worked

spiders <- as.data.frame(ddply(spiders,"Instar",transform, Leg.Scal=zScore(logLeg)))

ggplot(spiders, aes(Leg.Scal)) + geom_histogram() + facet_wrap(~Instar) # checking that the condition worked


SpiNestAveAll<- ddply(spiders, .(NestID, type, Instar, logCtFm, CountFemales), summarise, # need to discount trials where no feeding obs and eve
		N = length(!is.na(Weight.mg)),
		meanLeg = mean(LegLen.mm, na.rm = TRUE),
		sdLeg = sd(LegLen.mm, na.rm = TRUE),
		CVLeg= sdLeg / meanLeg,
		cvByNLeg = (1+(1/(4*N))) * CVLeg,
		logcvByNLeg = log10(cvByNLeg),
		meanCond = mean(condition, na.rm = TRUE),
		sdCond = sd(condition, na.rm = TRUE),
		CVCond = sdCond / meanCond,
		cvByNCond  = (1+(1/(4*N))) * CVCond,
		logcvByNCond = log10(cvByNCond ),
		meanLeg.Scal = mean(Leg.Scal),
		meanCond.Scal = mean(Cond.Scal)



)


SpiNestAveMul <- subset(SpiNestAveAll, type == "multiple")
# remove 
spidersMul <- subset(spiders, type == "multiple") #removing single females
spidersMul$NestID <- factor(spidersMul$NestID)
