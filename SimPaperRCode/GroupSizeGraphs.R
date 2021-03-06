# TODO: Add comment
# 
# Author: Ruth
###############################################################################


source("G:/PhDWork/RCode/SimPaperCode/SplineFunction.R")

lambda<-0.001


#imports the data
averages <- read.csv("G:/Dropbox/kinshipEvolution/DataAnalysis/averages.csv")

layout(matrix(c(1:3), 1, 3, byrow = TRUE))# layout of graphs

CC<-as.numeric(levels(as.factor(averages$C))) #because we are making it by C

RR<-as.numeric(levels(as.factor(averages$R))) 

BB<-as.numeric(levels(as.factor(averages$Beta)))

title<-c("GCC=50", "GCC=16.7", "GCC=10")


for(j in 1:3){	
	
# makes empty plot 
	ave.Sub2<-subset(averages, C==CC[j])
	plot(ave.Sub2$R, ave.Sub2$avgGrSize, main=title[j], xlab="rate of growth (r)", ylab="average group size", col=0)#, col=0)
	
	for(i in 1:5) {
		
		
		
		#subsetting the data so we have what we want
		ave.Sub<-subset(averages, C==CC[j] & Beta==BB[i])
		
		points(ave.Sub$R, ave.Sub$avgGrSize, pch=i, col=i, cex=1.2)  
		#pch is dot type, cex point size
		
		my.spline<-fnSpline(lambda, cbind(ave.Sub$R, ave.Sub$avgGrSize))
		
		smooth<-spline(my.spline) # adds extra points to smooth the line so it is not angular  
		
		lines(smooth, lty=i, lwd=1, col = i)
		#lty is line type goes 1-6
	}  
	
	
}
