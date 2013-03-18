# TODO: Add comment
# 
# Author: Ruth
###############################################################################


library(tseries) # loading the package time series
source("Ruth/SplineFunction.R")

lam<-0.001



fileNames<-read.csv("kinshipEvolution/DataAnalysis/fileNames.csv", quote="", col.names="filenames")

DF<- data.frame(R = numeric (0), Beta = numeric(0), C = numeric(0),  KPvsCoopMax = numeric(0), KPvsCoopLag = numeric(0), KPvsGSMax = numeric(0), 
		KPvsGSLag = numeric(0),  KPvsRelMax = numeric(0), KPvsRelLag= numeric(0), GSvsRelMax= numeric(0), GSvsRelLag= numeric(0),
		GSvsCoopMax= numeric(0), GSvsCoopLag= numeric(0), CoopvsResMax = numeric(0), CoopvsResLag = numeric(0), KinADF, relADF, gsADF, coopADF)


for (i in 1:nrow(fileNames)){
	
	file<-read.delim(as.character(fileNames[i,]))
	
	file <- file[which (file$tick >=10000),]
	
	file$avgCoop <- asin(file$avgCoop)
	file$rel<-asin(file$rel)
	file$kinPref<-asin(file$kinPref)
	file$avgGrSize<-log(file$avgGrSize)
	
	print (i)
	
	R <- file[1, 4]
	
	Beta <- file[1, 5]
	
	C <- file[1, 6]
	###########spline
	
	
		#for (j in c(8,10,12,13)){ # spline smoothing the time series
		
		#	file[,j]<- (fnSpline(lam, file$tick, file[,j]))$y
		#}
	
	
	
	a<-ccf(file$kinPref, file$avgCoop, lag.max=5000, plot = FALSE)
	
	b<-ccf(file$kinPref, file$avgGrSize, lag.max=5000, plot = FALSE)
	
	c<-ccf(file$kinPref, file$rel, lag.max=5000, plot = FALSE)
	
	d<-ccf(file$avgGrSize, file$rel, lag.max=5000, plot = FALSE)
	
	e<-ccf(file$avgGrSize, file$avgCoop, lag.max=5000, plot = FALSE)
	
	f<-ccf(file$avgCoop, file$rel, lag.max=5000, plot = FALSE)
	
	adf.test(as.ts(file$kinPref))

	
	list<-c(R, Beta, C,  
			as.numeric(a$acf[which.max(array(abs(a$acf)))]), as.numeric(a$lag[which.max(array(abs(a$acf)))]), 
			as.numeric(b$acf[which.max(array(abs(b$acf)))]), as.numeric(b$lag[which.max(array(abs(b$acf)))]), 
			as.numeric(c$acf[which.max(array(abs(c$acf)))]), as.numeric(c$lag[which.max(array(abs(c$acf)))]), 
			as.numeric(d$acf[which.max(array(abs(d$acf)))]), as.numeric(d$lag[which.max(array(abs(d$acf)))]),  
			as.numeric(e$acf[which.max(array(abs(e$acf)))]), as.numeric(e$lag[which.max(array(abs(e$acf)))]), 
			as.numeric(f$acf[which.max(array(abs(f$acf)))]), as.numeric(f$lag[which.max(array(abs(f$acf)))]),
			adf.test(as.ts(file$kinPref))$p.value, adf.test(as.ts(file$rel))$p.value, 
			adf.test(as.ts(file$avgGrSize))$p.value, adf.test(as.ts(file$avgCoop))$p.value)
	
	 DF[i,]<-list # adding row

}

write.table(DF, "LagMeansTransNotSmMax5000.csv", sep=",", row.names = FALSE)


colMeans(DF)


