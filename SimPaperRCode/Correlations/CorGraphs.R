# TODO: Add comment
# 
# Author: Ruth
# Makes graphs of maximum lag and maximum correlation by variable
###############################################################################

source("Ruth/SplineFunction.R")

lambda <-0.01

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

corrs<-AveByParms


cols<-c("navy", "tomato1","purple", "green4", "orange") # colours for graph
pnts<-c(15, 16, 2, 18, 17) #point type
lines<-c(2,3,4,5,6,7,8,9) #line type for graphs


CC<-as.numeric(levels(as.factor(corrs$C)))
Beta<-as.numeric(levels(as.factor(corrs$Beta)))

pdf("kinshipEvolution/Correlations/CorrGraphsSpline2014.pdf", onefile=TRUE)

layout(matrix(c(1:6), 2, 3, byrow = TRUE))

for (i in c(12, 13, 14, 15, 10, 11)){
	
	if (i %% 2==0) {
		yaxislab <-"maximum correlation"
		ylimits <- c(-1, 1)
	} else{dev
		yaxislab <- "Lag at maximum correlation"
		ylimits<-c(-5000, 5000)
	}
	
print(i)
	
yParm<-i #column number 
ylabel<-colnames(corrs)[yParm]

#fileName<- paste("C:/Users/Ruth/Desktop/CorrGraphs/", ylabel, ".pdf", sep="")

#print(fileName)


##
#making empty plot with all C's so axis don't change

for (k in 1:length(CC)){
	
	paste(ylabel, " C=", CC[k])

plot(corrs$R, corrs[, yParm], col=0, xlab = "R", ylab = yaxislab, 
		ylim= ylimits, main=paste(ylabel, " C=", CC[k]))

CVal <- CC[k]



	for(j in 1:length(Beta)) {
		
	
	
		corrs.Sub<-subset(corrs, C==CVal & corrs$Beta==Beta[j])
		
		points(corrs.Sub$R, corrs.Sub[, yParm], pch=pnts[j], col=cols[j], cex=1.2)
	
		my.spline<-fnSpline(lambda, corrs.Sub$R, corrs.Sub[, yParm] )
	
		smooth<-spline(my.spline) # adds extra points to smooth the line so it is not angular  
	
		lines(smooth, lty=lines[j], lwd=1, col = cols[j])
	
	} 
}

#dev.off()
}

beta<-c("Beta=0.0", "Beta=0.2", "Beta=0.4", "Beta=0.6", "Beta=0.8")


legend(x=.05, y=4500, beta, cex=.9, col=cols, pch=pnts, 
		lty=lines, box.col="transparent", bg="blue", bty="n",
		y.intersp=1) #kin pref x=1.0, y=0.3; ave coop x=1.0, y=0.4; relatedness 

dev.off()

meansAbs<-colMeans(abs(corrs))

#write.table(means, "G:/means.csv", sep=",", row.names = TRUE)

