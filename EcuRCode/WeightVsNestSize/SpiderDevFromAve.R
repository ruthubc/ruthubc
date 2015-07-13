# TODO: Add comment
# 
# Author: Ruth
###############################################################################
library(ggplot2)
library(lmerTest) # this puts pvalue in lmer


SpiAveMerge<- ddply(spiders, .(NestID, Instar), summarise, # need to discount trials where no feeding obs and eve
		N = length(!is.na(Weight.mg)),
		WeightMean = mean(Weight.mg, na.rm = TRUE),
		LogWeightMean = mean(logWeight, na.rm = TRUE),
		LegMean = mean(LegLen.mm, na.rm = TRUE),
		LogLegMean = mean(logLeg, na.rm = TRUE),
		HungerMean = mean(hunger, na.rm=TRUE),
		LogHungerMean = mean(logHung, na.rm=TRUE)
		
		

)

#ggplot(data = spidersMul, aes(logWeight)) + geom_histogram() + facet_wrap(~Instar)



spidersMul <- merge(spidersMul, SpiAveMerge, by = c("NestID", "Instar") )

spidersMul$WeightMean[spidersMul$N == 1] <- NA
spidersMul$LogWeightMean[spidersMul$N == 1] <- NA
spidersMul$LegDiffFromMean[spidersMul$N == 1] <- NA
spidersMul$LogLegDiffFromMean[spidersMul$N == 1] <- NA


spidersMul$LogWeightDiffFromMean <- abs(spidersMul$logWeight - spidersMul$LogWeightMean)

spidersMul$WeightDiffFromMean <- abs(spidersMul$Weight.mg - spidersMul$WeightMean)

spidersMul$LegDiffFromMean <- abs(spidersMul$LegLen.mm - spidersMul$LegMean)

spidersMul$LogLegDiffFromMean <- abs(spidersMul$logLeg - spidersMul$LogLegMean)

spidersMul$HungerDiffFromMean <- abs(spidersMul$hunger - spidersMul$HungerMean)

spidersMul$LogHungerDiffFromMean <- abs(spidersMul$LogHungerMean - spidersMul$logHung)

spidersMul$SqRtOfHungDiff <- (spidersMul$HungerDiffFromMean)^0.5

spidersMul$SqRtOfLegDiff <- (spidersMul$LegDiffFromMean)^0.5

## Histograms to check distribution. 
#I am going with the log one for the moment as it appears that the distributions are the most similar among instars
ggplot(data = spidersMul, aes(WeightDiffFromMean)) + geom_histogram() + facet_wrap(~Instar)

ggplot(data = spidersMul, aes(LegDiffFromMean^0.5)) + geom_histogram() + facet_wrap(~Instar)

ggplot(data = spidersMul, aes(LogLegDiffFromMean)) + geom_histogram() + facet_wrap(~Instar)

ggplot(data = spidersMul, aes(LogOfHungDiff)) + geom_histogram() + facet_wrap(~Instar)

ggplot(data = spidersMul, aes(LogHungerDiffFromMean)) + geom_histogram()# + facet_wrap(~Instar)

ggplot(data = spidersMul, aes(SqRtOfHungDiff)) + geom_histogram() + facet_wrap(~Instar)

ggplot(data = spidersMul, aes(x = logCtFm , y = SqRtOfHungDiff)) + facet_wrap(~Instar) + geom_point() + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE)


SpiDiffAve<- ddply(spidersMul, .(NestID, Instar, N, logCtFm), summarise, # need to discount trials where no feeding obs and eve
		NewN = length(!is.na(LogWeightDiffFromMean)),
		MeanLogWtDiff = mean(LogWeightDiffFromMean, na.rm = TRUE),
		SELogWtDiff = sd(LogWeightDiffFromMean, na.rm = TRUE)/ sqrt(NewN),
		MeanLogLegDiff = mean(LogLegDiffFromMean, na.rm = TRUE),
		SELogLegDiff = sd(LogLegDiffFromMean, na.rm = TRUE)/ sqrt(NewN)
)



check <- subset(spidersMul, NestID == "16.2EX01" & Instar == "AdMale")
## graphs diff from mean vs nest size
ggplot(data = SpiDiffAve, aes(x = logCtFm , y = MeanLogLegDiff )) + facet_wrap(~Instar) + geom_point() + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE)

ggplot(data = SpiDiffAve, aes(x = logCtFm , y = MeanLogWtDiff )) + facet_wrap(~Instar) + geom_point() + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE)


### Stats #########
#### Leg Length variance 

legDiffLm<- lmer(SqRtOfLegDiff ~ I(logCtFm^2) + logCtFm + Instar+ Instar:logCtFm + 
				I(logCtFm^2):Instar + (1|NestID), data = spidersMul, REML = FALSE)

summary(legDiffLm)

modelPlot(legDiffLm)

anova(legDiffLm)

## Hunger variance 

hungDiffLm<- lmer(SqRtOfHungDiff ~ I(logCtFm^2) + logCtFm + Instar+ Instar:logCtFm + 
				I(logCtFm^2):Instar + (1|NestID), data = spidersMul, REML = FALSE)

summary(hungDiffLm)

modelPlot(hungDiffLm)

anova(hungDiffLm)