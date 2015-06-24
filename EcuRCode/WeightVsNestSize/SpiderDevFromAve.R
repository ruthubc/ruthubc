# TODO: Add comment
# 
# Author: Ruth
###############################################################################
library(ggplot2)

SpiAveMerge<- ddply(spiders, .(NestID, Instar), summarise, # need to discount trials where no feeding obs and eve
		N = length(!is.na(Weight.mg)),
		WeightMean = mean(Weight.mg, na.rm = TRUE),
		LogWeightMean = mean(logWeight, na.rm = TRUE),
		LegMean = mean(LegLen.mm),
		LogLegMean = mean(logLeg)
		

)

#ggplot(data = spidersMul, aes(logWeight)) + geom_histogram() + facet_wrap(~Instar)



spidersMul <- merge(spidersMul, SpiAveMerge, by = c("NestID", "Instar") )

spidersMul$WeightMean[spidersMul$N == 1] <- NA
spidersMul$LogWeightMean[spidersMul$N == 1] <- NA
spidersMul$LegDiffFromMean[spidersMul$N == 1] <- NA
spidersMul$LogLegDiffFromMean[spidersMul$N == 1] <- NA


spidersMul$LogWeightDiffFromMean <- spidersMul$logWeight - spidersMul$LogWeightMean

spidersMul$WeightDiffFromMean <- spidersMul$Weight.mg - spidersMul$WeightMean

spidersMul$LegDiffFromMean <- spidersMul$LegLen.mm - spidersMul$LegMean

spidersMul$LogLegDiffFromMean <- spidersMul$logLeg - spidersMul$LogLegMean

## Histograms to check distribution. 
#I am going with the log one for the moment as it appears that the distributions are the most similar among instars
ggplot(data = spidersMul, aes(WeightDiffFromMean)) + geom_histogram() + facet_wrap(~Instar)

ggplot(data = spidersMul, aes(LegDiffFromMean)) + geom_histogram() + facet_wrap(~Instar)

ggplot(data = spidersMul, aes(LogLegDiffFromMean)) + geom_histogram() + facet_wrap(~Instar)


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