# TODO: Add comment
# 
# Author: user
###############################################################################

library(ggplot2)
library(plyr)
library(data.table)

Census <- read.csv("RuthSync/EggManipulation/RuthDataFiles/CensusJMP.csv", na.strings = NA)

Census$LogRatio <- log10(Census$ratioJuvsAds+0.1)  # can't do this as there are some zeros.
Census$LogAdults <- log10(Census$TotAds)
#Census$AdsSubs <- Census$AdFemales + Census

hist(Census$TotJuvs, breaks = 30)


censusOne <- Census[(Census$IndCensus == 1), ]

censusDT <- as.data.table(Census)

censusOne <- as.data.table(censusOne)

#censusOther <- Census[!(Census$Census == 1), ]


censusDT <- data.table(IndNestID = Census$IndColID, CensusNo = Census$IndCensus, AdsSubs =  Census$Log10AdsSubs, Treatment = Census$Treatment, 
		Ads = Census$AdFemales)

censusOneDT <- data.table(IndNestID = censusOne$IndColID, AdsSubsOne =  censusOne$Log10AdsSubs, OneAds = censusOne$AdFemales)

#censusOne[, count := length(unique(AdsSubsOne)), by=IndNestID]

CensusBoth<- merge(censusDT, censusOneDT, by = c("IndNestID"))


#censusDT[, min := min(CensusNo), by=IndNestID]

#minCensus <- censusDT [,.SD[which.min(CensusNo)],by=c('IndNestID')]

CensusBoth$PerChange <- (CensusBoth$AdsSubs - CensusBoth$AdsSubsOne) / CensusBoth$AdsSubsOne

CensusBoth$ChgAds <- (CensusBoth$Ads - CensusBoth$OneAds) / CensusBoth$OneAds



ggplot(data = CensusBoth, aes(x = CensusNo, y = PerChange, colour = Treatment )) + geom_point(position = pd) +
		geom_line(position = pd, aes(group= IndNestID))

hist(CensusBoth$PerChange, breaks = 30)
hist(log10(CensusBoth$ChgAds),breaks = 30)

CensusChange<- ddply(CensusBoth, .(CensusNo, Treatment), summarise,
		N = length(!is.na(IndNestID)),
		AdSub.Mean = mean(PerChange, na.rm = TRUE),
		AdSub.sd   = sd(PerChange),
		AdSub.se   = AdSub.sd / sqrt(N),
		Ads.Mean = mean(ChgAds),
		Ads.se = sd(ChgAds)/sqrt(N)
)

# Mean and se of percentage change of ads +subs
ggplot(data = CensusChange, aes(x = CensusNo, y = AdSub.Mean, colour = Treatment )) + geom_point() + geom_line() +
		geom_errorbar(aes(ymin=AdSub.Mean - AdSub.se, ymax=AdSub.Mean + AdSub.se),width=.1) + 
		geom_text(aes(label=N),hjust=-0.3, vjust=-0.3)

# Mean of percentage change of adults only
ggplot(data = CensusChange, aes(x = CensusNo, y = Ads.Mean, colour = Treatment )) + geom_point() + geom_line() +
		geom_errorbar(aes(ymin=Ads.Mean - Ads.se, ymax=Ads.Mean + Ads.se),width=.1) + 
		geom_text(aes(label=N),hjust=-0.3, vjust=-0.3)






########## Difference between control and too many

CensusControl <- Census[(Census$Treatment == "Control"), ]
CensusTooMany <- Census[(Census$Treatment == "TooMany"), ]

ControlDT <- data.table(IndNestID = CensusControl$ColonyID, CensusNo = CensusControl$IndCensus, AdsControl =  CensusControl$TotAds)

TooManyDT <- data.table(IndNestID = CensusTooMany$ColonyID, CensusNo = CensusTooMany$IndCensus, AdsTooMany =  CensusTooMany$TotAds)

CensusTreatment<- merge(ControlDT, TooManyDT, by = c("IndNestID", "CensusNo"))

CensusTreatment$AdChg <- (CensusTreatment$AdsTooMany - CensusTreatment$AdsControl)/CensusTreatment$AdsControl

CensusTreatment$LogAdChg <- log10(CensusTreatment$AdChg)

hist(log10(CensusTreatment$AdChg), breaks = 40)

TreatAve<- ddply(CensusTreatment, .(CensusNo), summarise,
		N = length(!is.na(IndNestID)),
		AdChg.Mean = mean(AdChg, na.rm = TRUE),
		AdChg.sd   = sd(AdChg),
		AdChg.se   = AdChg.sd / sqrt(N)
)


ggplot(data = TreatAve, aes(x = CensusNo, y = AdChg.Mean )) + geom_point() + geom_line() +
		geom_errorbar(aes(ymin=AdChg.Mean - AdChg.se, ymax=AdChg.Mean + AdChg.se),width=.1) + 
		geom_text(aes(label=N),hjust=-0.3, vjust=-0.3)


