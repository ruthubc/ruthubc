
# Author: Ruth
###############################################################################

library(ggplot2)
library(plyr)
library(data.table)
library()

options(scipen = 100)  # disables scientific notation when displaying numbers

### Biomass overall

#Biomass <- format(Biomass, scientific = FALSE)

Biomass <- subset(Biomass, NumAds > 0)

Biomass$logFm <- log10(Biomass$NumAds)

Biomass$totBioTrans <- Biomass$dryBioTot^(1/3)  # cubed root is the only one that maked it look vaguly noraml

Biomass$FdPerAd <- Biomass$dryBioTot / Biomass$NumAds

Biomass$FdPerAdTrans <- Biomass$FdPerAd^(1/3)

Biomass$FdPerSpi <- Biomass$dryBioTot / Biomass$SubsAds

Biomass$FdPerSpiTrans <- Biomass$FdPerSpi^(1/3)


hist(Biomass$FdPerSpi^(1/3), breaks = 100)

hist(Biomass$FdPerAd^(1/3), breaks = 100)

hist(Biomass$totBioTrans, breaks = 100)
hist(log10(Biomass$dryBioTot^(1/3)+1), breaks = 200)

ggplot(data = Biomass, aes(x = log10(NumAds), y = totBioTrans)) + geom_point() + geom_smooth()

ggplot(data = subset(Biomass, FdPerAdTrans <  0.1), aes(x = log10(NumAds), y = FdPerAdTrans)) + geom_point() + geom_smooth(method = "lm")

ggplot(data = subset(Biomass), aes(x = log10(NumAds), y = FdPerSpi^(1/3))) + geom_point() + geom_smooth(method = "lm")


########## biomass per juv
 mBiomass$logJvfd <- asin(sqrt(Biomass$jvfd))  # this is NOT parametric 


hist((asin(sqrt(Biomass$jvfd))^2), breaks = 50)

ggplot(Biomass, aes(x=Treatment, y=logJvfd)) + geom_boxplot()


BiomassOverall <- ddply(Biomass, .(ColonyID, Treatment), summarise,
		N = length(!is.na(dryBioTot)),
		Ratio.Mean = mean(Dry_biomass_lnY, na.rm = TRUE),
		Ratio.sd   = sd(LogRatio),
		Ratio.se   = Ratio.sd / sqrt(N)


)
