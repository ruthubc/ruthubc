# Graph of Lag Vs Max Correlation
# Author: Ruth
###############################################################################
library(plyr)

Corrs<-read.csv("kinshipEvolution/Correlations/LagMeansTransSplineSamples2014.csv")


AveByParms <- ddply(Corrs,.(R, C, Beta), summarise,
		KPvsCoopMax = mean(KPvsCoopMax),
		KPvsCoopLag = mean(abs(KPvsCoopLag)),
		KPvsGSMax = mean(KPvsGSMax),
		KPvsGSLag = mean(abs(KPvsGSLag)),
		KPvsRelMax = mean(KPvsRelMax),
		KPvsRelLag = mean(abs(KPvsRelLag)),
		GSvsRelMax = mean(GSvsRelMax),
		GSvsRelLag = mean(abs(GSvsRelLag)),
		GSvsCoopMax = mean(GSvsCoopMax),
		GSvsCoopLag = mean(abs(GSvsCoopLag)),
		CoopvsResMax = mean(CoopvsResMax),
		CoopvsResLag = mean(abs(CoopvsResLag))

)


# want 6 graphs

pdf("kinshipEvolution/Correlations/LagVsCor.pdf", onefile = TRUE)

layout(matrix(c(1:6), 2, 3, byrow = TRUE))

for (i in c(4,6,8, 10, 12, 14)){
	
	yParm<-i #column number 
	ylabel<-colnames(AveByParms)[yParm]
	print (ylabel)

	plot(AveByParms[,i+1], AveByParms[,i], main = ylabel, xlab = "MaxCorrelation", ylab = "Lag at Max Correlation")
	abline(lm(AveByParms[,i]~AveByParms[,i+1]))
}


dev.off()		