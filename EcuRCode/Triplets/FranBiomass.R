
# Author: Ruth
###############################################################################

library(ggplot2)
library(plyr)
library(data.table)


### Biomass overall

Biomass <- format(Biomass, scientific = FALSE)

Biomass$totBioTrans <- Biomass$dryBioTot^(1/3)  # cubed root is the only one that maked it look vaguly noraml

Biomass$FdPerAd <- Biomass$dryBioTot / Biomass$NumAds

Biomass$FdPerAdTrans <- format(Biomass$FdPerAd, scientific = FALSE)

hist(Biomass$FdPerAd^(1/3), breaks = 100)

hist(Biomass$totBioTrans, breaks = 100)
hist(log10(Biomass$dryBioTot^(1/3)+1), breaks = 200)

ggplot(data = Biomass, aes(x = log10(NumAds), y = totBioTrans)) + geom_point() + geom_smooth()

ggplot(data = Biomass, aes(x = log10(NumAds), y = Biomass$FdPerAd^(1/3))) + geom_point() + geom_smooth()



########## biomass per juv
Biomass$logJvfd <- asin(sqrt(Biomass$jvfd))  # this is NOT parametric 


hist((asin(sqrt(Biomass$jvfd))^2), breaks = 50)

ggplot(Biomass, aes(x=Treatment, y=logJvfd)) + geom_boxplot()


BiomassOverall <- ddply(Biomass, .(ColonyID, Treatment), summarise,
		N = length(!is.na(dryBioTot)),
		Ratio.Mean = mean(Dry_biomass_lnY, na.rm = TRUE),
		Ratio.sd   = sd(LogRatio),
		Ratio.se   = Ratio.sd / sqrt(N)


)
