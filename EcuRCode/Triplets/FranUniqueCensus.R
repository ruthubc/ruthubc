# TODO: Add comment
# 
# Author: Ruth
###############################################################################

library(ggplot2)
library(plyr)
library(data.table)

Census <- read.csv("RuthSync/EggManipulation/RuthDataFiles/UniqueCensusData.csv", na.strings = NA)

Census$AdSubs <- Census$AdFemales + Census$Sub2 + Census$Sub1

Census$JuvsTot <- Census$Juv3.4 + Census$Juv1.2

Census$RatioJA <- Census$JuvsTot/ Census$AdSubs # can't log10 transform as some zeros
# option is % adultssubs - % juvs

Census$IndNestID <- paste(Census$Colony.TrapID, Census$Nest_Number, sep = "_")

hist(log10(Census$RatioJA), breaks = 30)


CensusAve<- ddply(Census, .(Colony.TrapID, Census, Treatment), summarise,
		N = length(!is.na(Colony.TrapID)),
		Ratio.Mean = mean(RatioJA, na.rm = TRUE),
		AdSubs.Mean = mean(AdSubs, na.rm = TRUE)

)


ggplot(data = CensusAve, aes(x = Census, y = Ratio.Mean, colour = Treatment )) + geom_point() + geom_line()

censusOne <- Census[(Census$Census == 1), ]

#censusOther <- Census[!(Census$Census == 1), ]


censusDT <- data.table(IndNestID = Census$IndNestID, CensusNo = Census$Census, AdsSubs =  Census$AdSubs)

censusOne <- data.table(IndNestID = censusOne$IndNestID, AdsSubsOne =  censusOne$AdSub)

censusOne[, count := length(unique(AdsSubsOne)), by=IndNestID]

CensusBoth<- merge(censusDT, censusOne, by = c("IndNestID"))

#CensusBoth$PerDiff <- 
		
censusDT[, min := min(CensusNo), by=IndNestID]

minCensus <- censusDT [,.SD[which.min(CensusNo)],by=c('IndNestID')]
