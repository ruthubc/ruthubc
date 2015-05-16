# TODO: Add comment
# 
# Author: Ruth
###############################################################################


library(ggplot2)
library(plyr)
library(data.table)

Biomass <- read.csv("RuthSync/EggManipulation/RuthDataFiles/BiomassData.csv", na.strings = NA)

Biomass$JuvTot<- Biomass$Juv34 + Biomass$Juv12

## check that the censuss are the correct ones
BiomassNestCen <- ddply(Biomass, .(ColonyID, Treatment, OverallID, Census, JuvTot), summarise,
		N = length(!is.na(OverallID)),
		dryBioTot = sum(Dry_biomass_Y, na.rm = TRUE)
		
)

BiomassNestCen$foodRatio <-  BiomassNestCen$dryBioTot/BiomassNestCen$JuvTot

hist(BiomassNestCen$dryBioTot, breaks = 50)
hist(BiomassNestCen$logDryBio, breaks = 50)
hist(BiomassNestCen$recDryBio, breaks = 50)



BiomassOverall <- ddply(Biomass, .(ColonyID, Treatment), summarise,
		N = length(!is.na(dryBioTot)),
		Ratio.Mean = mean(Dry_biomass_lnY, na.rm = TRUE),
		Ratio.sd   = sd(LogRatio),
		Ratio.se   = Ratio.sd / sqrt(N)


)
