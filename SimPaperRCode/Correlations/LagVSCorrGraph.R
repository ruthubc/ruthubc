# Graph of Lag Vs Max Correlation
# Author: Ruth
###############################################################################
library(plyr)




# want 6 graphs

pdf("kinshipEvolution/Correlations/LagVsCorNew.pdf", onefile = TRUE)

layout(matrix(c(1:6), 2, 3, byrow = TRUE))

for (i in c(4,6,8, 10, 12, 14)){
	
	yParm<-i #column number 
	ylabel<-colnames(AveByParms)[yParm]
	print (ylabel)

	plot(AveByParms[,i+1], AveByParms[,i], main = ylabel, xlab = "MaxCorrelation", ylab = "Lag at Max Correlation")
	abline(lm(AveByParms[,i]~AveByParms[,i+1]))
}


dev.off()		