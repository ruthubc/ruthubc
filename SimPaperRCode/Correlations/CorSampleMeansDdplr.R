# TODO: Add comment
# 
# Author: user
###############################################################################

library(plyr)


Corrs<-read.csv("kinshipEvolution/Correlations/LagMeansTransSplineSamples2014.csv")


AveByParms <- ddply(Corrs,.(R, C, Beta), summarise,
		KPvsCoopCorr = mean(KPvsCoopMax),
		KPvsCoopLag = mean(abs(KPvsCoopLag)),
		KPvsGSCorr = mean(KPvsGSMax),
		KPvsGSLag = mean(abs(KPvsGSLag)),
		KPvsRelCorr = mean(KPvsRelMax),
		KPvsRelLag = mean(abs(KPvsRelLag)),
		GSvsRelCorr = mean(GSvsRelMax),
		GSvsRelLag = mean(abs(GSvsRelLag)),
		GSvsCoopCorr = mean(GSvsCoopMax),
		GSvsCoopLag = mean(abs(GSvsCoopLag)),
		CoopvsRelCorr = mean(CoopvsResMax),
		CoopvsRelLag = mean(abs(CoopvsResLag))

)
