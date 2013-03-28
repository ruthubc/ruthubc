# TODO: Add comment
# http://www.statmethods.net/advstats/timeseries.html
# Author: Ruth
###############################################################################

source("Ruth/SplineFunction.R")
#library(TSA)
library(tseries)

lam<-0.001


fileNames<-read.csv("kinshipEvolution/DataAnalysis/fileNames.csv", quote="")


file<-read.delim(as.character(fileNames[21,]))

file$avgCoop <- asin(sqrt(file$avgCoop))
file$rel<-asin(sqrt(file$rel))
file$kinPref<-asin(sqrt(file$kinPref))
file$avgGrSize<-log(file$avgGrSize)

file <- file[which (file$tick >=10000),]






for (j in c(8,10,12,13, 11)){ # spline smoothing the time series
	
	print(j)
	file[,j]<- (fnSpline(lam, file$tick, file[,j]))$y
}

# graph of average cooperative vs group size faced by an average individual
#plot(file$avgGrSize, file$avgCoop, cex=.1)

 ###testing for stationaryity
 
series<-ts(file[,13])
series2<-ts(file[,12])
adf.test(series)$p.value
pp.test(series)

time(series)

prewhiten(series2, series, lag = 5000)
ccf(series, series2, lag=5000)
X11()
ccf(file$kinPref, file$avgCoop, lag.max=2000, plot = TRUE)

#ccf(file$kinPref, file$avgGrSize, lag.max=1000)

#ccf(file$kinPref, file$rel, lag.max=0, plot = FALSE)

#ccf(file$rel, file$avgGrSize, lag.max=0, plot= FALSE)

ccf(file$avgGrSize, file$rel, lag.max=10000)

#ccf(file$avgCoop, file$avgGrSize, lag.max=10000)
	ccf(file$avgCoop, file$avgGrSize, lag.max=5000, plot = F)			
#

cross<- ccf(file$avgCoop, file$rel, lag.max=5000, plot = FALSE)

as.numeric(cross$acf[which.max(array(abs(cross$acf)))])
as.numeric(cross$lag[which.max(array(abs(cross$acf)))])

X11()

ccf(file$avgCoop, file$rel, lag.max=5000, plot = T)

ccf(file$avgCoop, file$avgGrSize, lag.max=5000, plot = T)


ccf(file$avgGrSize, file$avgCoop, lag.max=10000, plot = FALSE)


#cross$lag[which.max(array(abs(cross$acf)))]

#cross$acf[which.max(array(abs(cross$acf)))]


cross$acf[which.max(array(abs(cross$acf)))]
cross$lag[which.max(array(abs(cross$acf)))]

cross2$acf[which.max(array(abs(cross2$acf)))]
cross2$lag[which.max(array(abs(cross2$acf)))]

acf(file$avgGrSize)

