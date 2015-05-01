# TODO: Add comment
# 
# Author: Ruth
###############################################################################

library(ggplot2)
library(plyr)

Census <- read.csv("RuthSync/EggManipulation/RuthDataFiles/UniqueCensusData.csv", na.strings = NA)

Census$AdSubs <- Census$AdFemales + Census$Sub2 + Census$Sub1

Census$JuvsTot <- Census$Juv3.4 + Census$Juv1.2

Census$RatioJA <- Census$JuvsTot/ Census$AdSubs # can't log10 transform as some zeros
# option is % adultssubs - % juvs


hist(log10(Census$RatioJA), breaks = 30)


CensusAve<- ddply(Census, .(Colony.TrapID, Census, Treatment), summarise,
		N = length(!is.na(Colony.TrapID)),
		Ratio.Mean = mean(RatioJA, na.rm = TRUE),
		AdSubs.Mean = mean(AdSubs, na.rm = TRUE)

)


ggplot(data = CensusAve, aes(x = Census, y = Ratio.Mean, colour = Treatment )) + geom_point() + geom_line()