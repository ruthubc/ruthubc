
# Author: Ruth
###############################################################################

library(ggplot2)
library(plyr)
library(data.table)

Biomass$logJvfd <- log10(Biomass$jvfd+1)  # this is NOT parametric 


hist(log10(Biomass$jvfd + 0.1), breaks = 50)

ggplot(Biomass, aes(x=Treatment, y=logJvfd)) + geom_boxplot()




BiomassOverall <- ddply(Biomass, .(ColonyID, Treatment), summarise,
		N = length(!is.na(dryBioTot)),
		Ratio.Mean = mean(Dry_biomass_lnY, na.rm = TRUE),
		Ratio.sd   = sd(LogRatio),
		Ratio.se   = Ratio.sd / sqrt(N)


)
