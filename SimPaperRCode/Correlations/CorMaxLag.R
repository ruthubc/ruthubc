# TODO: Add comment
# 
# Author: Ruth
# Outputs to file the correlation at maximum lag for each run
###############################################################################

library(TSA)
library(tseries) # loading the package time series
source("Ruth/SplineFunction.R")

lam<-0.001



fileNames<-read.csv("kinshipEvolution/DataAnalysis/fileNames.csv", quote="", col.names="filenames")

DF<- data.frame(sample = numeric(0), R = numeric (0), Beta = numeric(0), C = numeric(0),  KPvsCoopMax = numeric(0), KPvsCoopLag = numeric(0), KPvsGSMax = numeric(0), 
		KPvsGSLag = numeric(0),  KPvsRelMax = numeric(0), KPvsRelLag= numeric(0), GSvsRelMax= numeric(0), GSvsRelLag= numeric(0),
		GSvsCoopMax= numeric(0), GSvsCoopLag= numeric(0), CoopvsResMax = numeric(0), CoopvsResLag = numeric(0), KinADF = numeric(0) , 
		relADF =numeric(0), gsADF =numeric(0), coopADF =numeric(0))


for (i in 1:nrow(fileNames)){
	
	largeFile<-read.delim(as.character(fileNames[i,]))
	
	largeFile <- largeFile[which (largeFile$tick >=10000),]
	
	largeFile$avgCoop <- asin(sqrt(largeFile$avgCoop))
	largeFile$rel<-asin(sqrt(largeFile$rel))
	largeFile$kinPref<-asin(sqrt(largeFile$kinPref))
	largeFile$avgGrSize<-log(largeFile$avgGrSize)
	
	print ("large file number:")
	print (i)
	
	R <- largeFile[1, 4]
	
	Beta <- largeFile[1, 5]
	
	C <- largeFile[1, 6]
	###########spline
	
	
		for (j in c(8,10,12,13)){ # spline smoothing the time series
		
			largeFile[,j]<- (fnSpline(lam, largeFile$tick, largeFile[,j]))$y
		}
	
	### Splitting the files up into 5000 rows i.e. 8 different files
	
	numSample = as.integer(nrow(largeFile)/8) #calculates the number to be in the 8 samples
	
	for (k in 1:8) {
		
		startRow = (((k-1) * numSample) +1)
		endRow =  (k * numSample)
		file <-largeFile[startRow: endRow,]
		print ("sample number:")
		print (k)
		print ("number of rows in file")
		print (nrow(file)) # number of rows in dataframe
		
	
	
	a<-ccf(file$kinPref, file$avgCoop, lag.max=5000, plot = FALSE)
	
	b<-ccf(file$kinPref, file$avgGrSize, lag.max=5000, plot = FALSE)
	
	c<-ccf(file$kinPref, file$rel, lag.max=5000, plot = FALSE)
	
	d<-ccf(file$avgGrSize, file$rel, lag.max=5000, plot = FALSE)
	
	e<-ccf(file$avgGrSize, file$avgCoop, lag.max=5000, plot = FALSE)
	
	f<-ccf(file$avgCoop, file$rel, lag.max=5000, plot = FALSE)
	
	adf.test(as.ts(file$kinPref))

	
	list<-c(k, R, Beta, C,  
			as.numeric(a$acf[which.max(array(abs(a$acf)))]), as.numeric(a$lag[which.max(array(abs(a$acf)))]), 
			as.numeric(b$acf[which.max(array(abs(b$acf)))]), as.numeric(b$lag[which.max(array(abs(b$acf)))]), 
			as.numeric(c$acf[which.max(array(abs(c$acf)))]), as.numeric(c$lag[which.max(array(abs(c$acf)))]), 
			as.numeric(d$acf[which.max(array(abs(d$acf)))]), as.numeric(d$lag[which.max(array(abs(d$acf)))]),  
			as.numeric(e$acf[which.max(array(abs(e$acf)))]), as.numeric(e$lag[which.max(array(abs(e$acf)))]), 
			as.numeric(f$acf[which.max(array(abs(f$acf)))]), as.numeric(f$lag[which.max(array(abs(f$acf)))]),
			adf.test(as.ts(file$kinPref))$p.value, adf.test(as.ts(file$rel))$p.value, 
			adf.test(as.ts(file$avgGrSize))$p.value, adf.test(as.ts(file$avgCoop))$p.value)
	
	nextRow = nrow(DF) +1
	DF[(nextRow),]<-list # adding row

	}


}



write.table(DF, "kinshipEvolution/Correlations/LagMeansTransSplineSamples2014q.csv", sep=",", row.names = FALSE)


####means from graphs
corrs<-read.csv("kinshipEvolution/Correlations/LagMeansTransSpline.csv")

#corrs = corrs[-3,]

means<-colMeans(corrs) # calculates the column means
meansAbs<-colMeans(abs(corrs))

x<-data.frame(means, meansAbs)

write.csv(x, "kinshipEvolution/Correlations/SplineMeans.csv")

numSample = as.integer(nrow(file)/8)


