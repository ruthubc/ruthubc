# TODO: Add comment
# 
# Author: Ruth
###############################################################################

library(ggplot2)
library(plyr)
library(data.table)

Census <- read.csv("RuthSync/EggManipulation/RuthDataFiles/CensusJMP.csv", na.strings = NA)

Census$LogRatio <- log10(Census$ratioJuvsAds+0.1)  # can't do this as there are some zeros.

hist(Census$ratioJuvsAds, breaks = 30)
hist(Census$LogRatio, breaks = 30)
hist(Census$Log10AdsSubs, breaks = 30)


pd <- position_dodge(0.2)

ggplot(data = Census, aes(x = IndCensus, y = LogRatio, colour = Treatment )) + geom_point(position = pd) +
		geom_line(position = pd, aes(group= IndColID))

ggplot(data = Census, aes(x = IndCensus, y = log10colSize, colour = Treatment )) + geom_point(position = pd) +
		geom_line(position = pd, aes(group= IndColID))


#Census$AdSubs <- Census$AdFemales + Census$Sub2 + Census$Sub1
#Census$JuvsTot <- Census$Juv3.4 + Census$Juv1.2
#Census$RatioJA <- Census$JuvsTot/ Census$AdSubs # can't log10 transform as some zeros
# option is % adultssubs - % juvs

Census$IndNestID <- paste(Census$Colony.TrapID, Census$Nest_Number, sep = "_")

Census$LogRatio <- log10(Census$ratioJuvsAds+0.1)  # can't do this as there are some zeros.

hist(Census$ratioJuvsAds, breaks = 30)
hist(Census$LogRatio, breaks = 30)


CensusAve<- ddply(Census, .(IndCensus, Treatment), summarise,
		N = length(!is.na(IndCensus)),
		Ratio.Mean = mean(LogRatio, na.rm = TRUE),
		Ratio.sd   = sd(LogRatio),
		Ratio.se   = Ratio.sd / sqrt(N)


)

#graph ratio juvs to adults
ggplot(data = CensusAve, aes(x = IndCensus, y = Ratio.Mean, colour = Treatment )) + geom_point() + geom_line() +
		geom_errorbar(aes(ymin=Ratio.Mean - Ratio.se, ymax=Ratio.Mean + Ratio.se),width=.1)




censusOne <- Census[(Census$IndCensus == 1), ]

censusDT <- as.data.table(Census)

censusOne <- as.data.table(censusOne)

#censusOther <- Census[!(Census$Census == 1), ]


censusDT <- data.table(IndNestID = Census$IndColID, CensusNo = Census$Census, AdsSubs =  Census$Log10AdsSubs, Treatment = Census$Treatment)

censusOne <- data.table(IndNestID = censusOne$IndColID, AdsSubsOne =  censusOne$Log10AdsSubs)

censusOne[, count := length(unique(AdsSubsOne)), by=IndNestID]

CensusBoth<- merge(censusDT, censusOne, by = c("IndNestID"))

#CensusBoth$PerDiff <- 
		
#censusDT[, min := min(CensusNo), by=IndNestID]

minCensus <- censusDT [,.SD[which.min(CensusNo)],by=c('IndNestID')]

CensusBoth$PerChange <- (CensusBoth$AdsSubs - CensusBoth$AdsSubsOne) / CensusBoth$AdsSubsOne

ggplot(data = CensusBoth, aes(x = CensusNo, y = PerChange, colour = Treatment )) + geom_point(position = pd) +
		geom_line(position = pd, aes(group= IndNestID))
