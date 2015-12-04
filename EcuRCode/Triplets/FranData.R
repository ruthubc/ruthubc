# 
# Author: user
###############################################################################

library(plyr)
library(data.table)
library(ggplot2)

#TODO: more important: change the time to day's since egg sac switching (and check is correct)

#TODO: ***** I might want to smooth the census data to take account of the errors in the censuses! ###

Census <- read.csv("RuthSync/EggManipulation/RuthDataFiles/CensusJMP.csv", na.strings = NA)

#TODO: Need to get this for each individual nest and age class. Some sort of a loop function? Or scale by the overall census average?

IndNests <- levels(Census$IndColID)

IndNests[1]

Census[Census$IndColID == IndNests[1],]

print(fnSpline(0.01, Census$IndCensus, Census$AdFemale))



CensusNum <- data.frame(ColID =Census$ColID, Date = Census$Date, CensusTripNo = Census$CensusTripNo, 
		IndCensus = Census$IndCensus, NumAds = Census$AdFemales, TotSubs = Census$TotSubs, Treatment = Census$Treatment)

CensusNum <- unique(CensusNum)

CensusNum$Date <- substr(as.character(CensusNum$Date), 0, 10)

CensusNum$DateFmt <- as.Date(CensusNum$Date , "%d/%m/%Y") # for some reason this doesn't show in the statet view?? But it works

Weights <- read.csv("RuthSync/EggManipulation/RuthDataFiles/JuvWeights.csv", na.strings = NA)

Weights$DateFmt <- as.Date(Weights$Date , "%d/%m/%Y")


#TODO: check what units the mass is in
BiomassOri <- read.csv("RuthSync/EggManipulation/RuthDataFiles/BiomassData.csv", na.strings = NA)

BiomassMerge <- merge(BiomassOri, CensusNum, by = c("ColID", "CensusTripNo", "Treatment"))

BiomassMerge$ID<-seq.int(nrow(BiomassMerge))

Biomass <- ddply(BiomassMerge, .(ColID, Treatment, IndCensus, NumAds, TotSubs, Date.y), summarise, # num ads is just females
		N = length(ID),
		dryBioTotMean = mean(DryBiomass, na.rm = TRUE), 
		dryBioTotSum = sum(DryBiomass, na.rm = TRUE)


)

Biomass$SubsAds <- Biomass$NumAds + Biomass$TotSubs

NumJuvs <- data.frame(ColID = Census$ColID, Treatment = Census$Treatment, IndCensus =  Census$IndCensus,
		TotJuvs = Census$TotJuvs)

Biomass <- merge(Biomass, NumJuvs, by =  c("ColID", "Treatment", "IndCensus"))

Biomass$dryBioTot <- Biomass$dryBioTot *1000

Biomass$jvfd <- Biomass$dryBioTot/Biomass$TotJuvs 


EggSwitch <- read.csv("RuthSync/EggManipulation/RuthDataFiles/EggSwitchingDates.csv", na.strings = NA)

EggSwitch$EggDateFmt  <- as.character(EggSwitch$DateEggSwitch)

EggSwitch$EggDateFmt <- as.Date(EggSwitch$EggDateFmt , "%d/%m/%Y") # for some reason this doesn't show in the statet view?? But it works

EggSwitch = EggSwitch[-c(2, 3) ]

EggSwitch <- unique(EggSwitch)

CenSwitch <- merge(CensusNum, EggSwitch, by = "ColID")

CenSwitch$DateDiff <- difftime(CenSwitch$DateFmt, CenSwitch$EggDateFmt, units = "days")

write.csv(CenSwitch, file = "RuthSync/EggManipulation/RuthDataFiles/CensusSwitchDiff.csv")


######### Weights

Weights <- read.csv("RuthSync/EggManipulation/RuthDataFiles/JuvWeights.csv", na.strings = NA)

Weights$Instar <- as.factor(Weights$Instar)

Weights$MeasNo <- as.factor(Weights$MeasNo)

Weights <- Weights[(Weights$Instar != "Sub1"),] # we don't want to include the weight of sub1's

Weights <- Weights[(Weights$Instar != "2"),] # removing juv 2's as well

Weights$Instar <- as.numeric(Weights$Instar)

Weights$WgtDateFmt <- as.Date(Weights$Date, "%d/%m/%Y" )

Weights <- merge(Weights, EggSwitch, by = "ColID")

Weights$DateDiff <- difftime(Weights$WgtDateFmt, Weights$EggDateFmt, units = "days")

unique(c(Weights$ColID, Weights$DateDiff))

WeightsTime <- data.frame(Weights$ColID, Weights$MeasNo, Weights$Date, Weights$WgtDateFmt, Weights$EggDateFmt, Weights$DateDiff)

WeightsTime <- unique(WeightsTime)


write.csv(WeightsTime, file = "RuthSync/EggManipulation/RuthDataFiles/WeightsDifference.csv")

Weights$DiffDays <- as.numeric(Weights$DateDiff)

ggplot(Weights, aes(x = DiffDays, y = log(LegLen), colour = Treatment)) + stat_summary(fun.y=mean, geom="line") + facet_grid(.~Instar)



##*********** Next step : make graph of biomass per juv!****

# ******** Another next step : find when the eggs where switched AND
# Update dates to date fields ! ***********

##** Also make some graphs involving nest size such as no inds per next size etc.
