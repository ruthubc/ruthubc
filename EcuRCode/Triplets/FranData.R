# 
# Author: user
###############################################################################

library(plyr)
library(data.table)

#TODO: ***** I might want to smooth the census data to take account of the errors in the censuses! ###

Census <- read.csv("RuthSync/EggManipulation/RuthDataFiles/CensusJMP.csv", na.strings = NA)

#TODO: Need to get this for each indivdiual nest and age class. Some sort of a loop function? Or scale by the overall census average?

IndNests <- levels(Census$IndColID)

IndNests[1]

Census[Census$IndColID == IndNests[1],]

print(fnSpline(0.01, Census$IndCensus, Census$AdFemale))



CensusNum <- data.frame(ColID =Census$ColID, Date = Census$Date, CensusTripNo = Census$CensusTripNo, 
		IndCensus = Census$IndCensus)

CensusNum <- unique(CensusNum)

CensusNum$Date <- substr(as.character(CensusNum$Date), 0, 10)

CensusNum$DateFmt <- as.Date(CensusNum$Date , "%d/%m/%Y") # for some reason this doesn't show in the statet view?? But it works

Weights <- read.csv("RuthSync/EggManipulation/RuthDataFiles/JuvWeights.csv", na.strings = NA)

Weights$DateFmt <- as.Date(Weights$Date , "%d/%m/%Y")


#TODO: check what units the mass is in
BiomassOri <- read.csv("RuthSync/EggManipulation/RuthDataFiles/BiomassData.csv", na.strings = NA)

BiomassMerge <- merge(BiomassOri, CensusNum, by = c("ColID", "CensusTripNo"))

Biomass <- ddply(BiomassMerge, .(ColID, Treatment, IndCensus), summarise,
		dryBioTot = sum(DryBiomass, na.rm = TRUE)

)



NumJuvs <- data.frame(ColID = Census$ColID, Treatment = Census$Treatment, IndCensus =  Census$IndCensus,
		TotJuvs = Census$TotJuvs)

Biomass <- merge(Biomass, NumJuvs, by =  c("ColID", "Treatment", "IndCensus"))

Biomass$dryBioTot <- Biomass$dryBioTot *1000

Biomass$jvfd <- Biomass$dryBioTot/Biomass$TotJuvs 



##*********** Next step : make graph of biomass per juv!****

# ******** Another next step : find when the eggs where switched AND
# Update dates to date fields ! ***********

##** Also make some graphs involving nest size such as no inds per next size etc.
