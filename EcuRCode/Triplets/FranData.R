# TODO: Add comment
# 
# Author: user
###############################################################################

library(plyr)
library(data.table)

Census <- read.csv("RuthSync/EggManipulation/RuthDataFiles/CensusJMP.csv", na.strings = NA)

CensusNum <- data.frame(ColID =Census$ColonyID, Date = Census$Date, CensusTripNo = Census$CensusTripNo, 
		IndCensus = Census$IndCensus)

CensusNum <- unique(CensusNum)

CensusNum$Date <- substr(as.character(CensusNum$Date), 0, 10)

CensusNum$DateFmt <- as.Date(CensusNum$Date , "%d/%m/%Y") # for some reason this doesn't show in the statet view?? But it works

Weights <- read.csv("RuthSync/EggManipulation/RuthDataFiles/JuvWeights.csv", na.strings = NA)

Weights$DateFmt <- as.Date(Weights$Date , "%d/%m/%Y")

BiomassOri <- read.csv("RuthSync/EggManipulation/RuthDataFiles/BiomassData.csv", na.strings = NA)


BiomassMerge <- merge(BiomassOri, CensusNum, by = c("ColID", "CensusTripNo"))

Biomass <- ddply(BiomassMerge, .(ColID, Treatment, IndCensus), summarise,
		dryBioTot = sum(DryBiomass, na.rm = TRUE)

)

## ************Next job add the number of juvs to the biomass table*********

NumJuvs <- data.frame(ColID = Census$ColID, Treatment = Census$Treatment, IndCensus =  Census$IndCensus,
		TotJuvs = Census$TotJuvs)

Biomass <- merge(Biomass, NumJuvs, by =  c("ColID", "Treatment", "IndCensus"))  # not working!

##*********** Next step : make graph of biomass per juv!****

# ******** Another next step : find when the eggs where switched AND
# Update dates to date fields ! ***********
