# TODO: Add comment
# 
# Author: Ruth
###############################################################################

source("Ruth/SplineFunction.R")

lambda <-0.001

corrs<-read.csv("LagMeansTransNotSmMax5000.csv")


cols<-c("navy", "tomato1","purple", "green4", "orange") # colours for graph
pnts<-c(15, 16, 2, 18, 17) #point type
lines<-c(2,3,4,5,6,7,8,9) #line type for graphs


CC<-as.numeric(levels(as.factor(corrs$C)))
Beta<-as.numeric(levels(as.factor(corrs$Beta)))

pdf("C:/Users/Ruth/Desktop/CorrGraphs/means.pdf", onefile=TRUE)

for (i in 4:15){
	
print(i)
	
yParm<-i #column number 
ylabel<-colnames(corrs)[yParm]

fileName<- paste("C:/Users/Ruth/Desktop/CorrGraphs/", ylabel, ".pdf", sep="")

print(fileName)

#pdf(fileName)



layout(matrix(c(1:3), 1, 3, byrow = TRUE))
#making empty plot with all C's so axis don't change

for (i in 1:length(CC)){

plot(corrs$R, corrs[, yParm], col=0, xlab = "R", ylab=ylabel)

CVal <- CC[i]



	for(i in 1:length(Beta)) {
	
	
		corrs.Sub<-subset(corrs, C==CVal & corrs$Beta==Beta[i])
		
		points(corrs.Sub$R, corrs.Sub[, yParm], pch=pnts[i], col=cols[i], cex=1.2)
	
		my.spline<-fnSpline(lambda, corrs.Sub$R, corrs.Sub[, yParm] )
	
		smooth<-spline(my.spline) # adds extra points to smooth the line so it is not angular  
	
		lines(smooth, lty=lines[i], lwd=1, col = cols[i])
	
	} 
}

#dev.off()
}

dev.off()



beta<-c("Beta=0.0", "Beta=0.2", "Beta=0.4", "Beta=0.6", "Beta=0.8")


legend(x=.05, y=.4, beta, cex=.9, col=cols, pch=pnts, 
		lty=lines, box.col="transparent", bg="blue", bty="n",
		y.intersp=1) #kin pref x=1.0, y=0.3; ave coop x=1.0, y=0.4; relatedness 


meansAbs<-colMeans(abs(corrs))

write.table(means, "G:/means.csv", sep=",", row.names = TRUE)

