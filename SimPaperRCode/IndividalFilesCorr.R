# TODO: Add comment
# 
# Author: Ruth
###############################################################################

source("G:/PhDWork/RCode/SimPaperCode/SplineFunction.R")

lam<-0.001

library(tseries) # loading the package time series

fileNames<-read.csv("G:/Dropbox/kinshipEvolution/DataAnalysis/fileNames.csv", quote="")


file<-read.delim(as.character(fileNames[58,]))

file <- file[which (file$tick >=10000),]



for (j in c(8,10,12,13, 11)){ # spline smoothing the time series
	
	print(j)
	file[,j]<- (fnSpline(lam, file$tick, file[,j]))$y
}

# graph of average cooperative vs group size faced by an average individual
#plot(file$avgGrSize, file$avgCoop, cex=.1)


ccf(file$kinPref, file$avgCoop, lag.max= 10000, plot = TRUE)

#ccf(file$kinPref, file$avgGrSize, lag.max=1000)

#ccf(file$kinPref, file$rel, lag.max=0, plot = FALSE)

#ccf(file$rel, file$avgGrSize, lag.max=0, plot= FALSE)

#ccf(file$avgGrSize, file$rel, lag.max=10000)

#ccf(file$avgCoop, file$avgGrSize, lag.max=10000)
				
#ccf(file$avgCoop, file$rel, lag.max=0, plot = FALSE)

cross<-ccf(file$avgCoop, file$avgGrSize, lag.max=10000, plot = FALSE)
cross2 <-ccf(file$avgGrSize, file$avgCoop, lag.max=10000, plot = FALSE)


#cross$lag[which.max(array(abs(cross$acf)))]

#cross$acf[which.max(array(abs(cross$acf)))]


cross$acf[which.max(array(abs(cross$acf)))]
cross$lag[which.max(array(abs(cross$acf)))]

cross2$acf[which.max(array(abs(cross2$acf)))]
cross2$lag[which.max(array(abs(cross2$acf)))]

acf(file$avgGrSize)

