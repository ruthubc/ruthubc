# Graph of Lag Vs Max Correlation
# Author: Ruth
###############################################################################

Corrls<-read.csv("kinshipEvolution/Correlations/LagMeansTransSplineSamples2014.csv")


AveByParms <- ddply(Corrls,.(R, C, Beta), summarise,
		KPvsCoopMax = mean(KPvsCoopMax),
		