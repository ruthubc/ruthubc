# TODO: Add comment
# 
# Author: user
###############################################################################


library('plyr')
library("ggplot2")

spiders <- read.csv("RuthEcuador2013/NestSize/CombinedNestVsWeight.csv")

spiders <- subset(spiders, AdMaleSubBd == "")


#changing to simplier name
spiders$type <- spiders$Approx..Single.


spiders <- subset(spiders, select = c(NestID, type, FemalesHaveEggsOrJuvs, Instar, Weight.mg, LegLen.mm, HeadLength.mm,  CountFemales ))

spiders$ID<-seq.int(nrow(spiders))

#removing eggs, parastised individuals and the outlier nest 44.3ex01 as the adults were particularly small
spiders <- subset(spiders, Instar != "FALSE" & NestID != "44.3ex01"  & Instar !="egg" & 
				Instar != "pj" & Instar != "PST" & Instar != "juv3")

##replacing ad males with sub bodies as sub males
spiders$Instar<-as.character(spiders$Instar) ; spiders$Instar[spiders$AdMaleSubBd == "y"] <- "SubMale"
spiders$Instar <-as.factor(spiders$Instar)

#Calculating spider hunger
#spiders$condition <- spiders$Weight.mg/spiders$HeadLength.mm

spiders$condition <- spiders$Weight.mg/spiders$LegLen.mm
spiders$condCb <- spiders$Weight.mg/(spiders$LegLen.mm^3)

#spiders$condCb <- spiders$Weight.mg/(spiders$HeadLength.mm^3)

# Histograms checking condition distribution
#ggplot(spiders, aes(spiders$condition)) + geom_histogram() + facet_wrap(~Instar)
#ggplot(spiders, aes(spiders$condCb)) + geom_histogram() + facet_wrap(~Instar) # checking the distribution ,  not normal, needs transforming

# transforming condition
spiders$conditionSq <- spiders$condition ^ 0.5
spiders$condCbSq <- spiders$condCb ^ 0.5

# checking the distribution again
#ggplot(spiders, aes(spiders$conditionSq)) + geom_histogram() + facet_wrap(~Instar) # dist much more normal
#ggplot(spiders, aes(spiders$condCbSq)) + geom_histogram() + facet_wrap(~Instar) # dist much more normal

#removing empty levels and ordering for graph
spiders$Instar <- factor(spiders$Instar, levels= c("Juv4", "Sub1", 
				"Sub2", "Adult", "SubMale", "AdMale"))


#log transforming
spiders$logWt <- log10(spiders$Weight.mg)
spiders$logCtFm <- log10(spiders$CountFemales)
spiders$logLeg<- log10(spiders$LegLen.mm)
spiders$logCond <- log10(spiders$condition)


# checking the normality with histograms
#ggplot(spiders, aes(logCtFm)) + geom_histogram() + facet_wrap(~Instar) # not very normal but I don't think it would be
#ggplot(spiders, aes(logLeg)) + geom_histogram() + facet_wrap(~Instar) # normal, awesome (but also normal before transformation)
#ggplot(spiders, aes(logCond)) + geom_histogram() + facet_wrap(~Instar) # normal, awesome


#SpiNestAveAll<- ddply(spiders, .(NestID, type, Instar, logCtFm, CountFemales), summarise,
#		N = length(!is.na(Weight.mg)),
#		meanLeg = mean(LegLen.mm, na.rm = TRUE),
#		sdLeg = sd(LegLen.mm, na.rm = TRUE),
#		CVLeg= sdLeg / meanLeg,
#		cvByNLeg = (1+(1/(4*N))) * CVLeg,
#		logcvByNLeg = log10(cvByNLeg),
#		meanCond = mean(condition, na.rm = TRUE),
#		sdCond = sd(condition, na.rm = TRUE),
#		CVCond = sdCond / meanCond,
#		cvByNCond  = (1+(1/(4*N))) * CVCond,
#		logcvByNCond = log10(cvByNCond ),
#		meanLeg.Scal = mean(Leg.Scal),
#		meanCond.Scal = mean(Cond.Scal)#


#)


#SpiNestAveMul <- subset(SpiNestAveAll, type == "multiple")
# remove 
spidersMul <- subset(spiders, type == "multiple") #removing single females
spidersMul$NestID <- factor(spidersMul$NestID)

#spidersMul <- subset(spiders, CountFemales > 14)

levels(as.factor(spidersMul$CountFemales))
levels(spidersMul$Instar)

### Creating subset tables of each instar

spiMulAd <- subset(spidersMul, Instar == "Adult")
spiMulS1 <- subset(spidersMul, Instar == "Sub1")
spiMulS2 <- subset(spidersMul, Instar == "Sub2")
spiMulJv <- subset(spidersMul, Instar == "Juv4")
spiMulMaleSb <- subset(spidersMul, Instar == "SubMale")
spiMulMaleAd <- subset(spidersMul, Instar == "AdMale")
