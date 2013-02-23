# TODO: Add comment
# 
# Author: Ruth
###############################################################################

source("G:/PhDWork/RCode/SimPaperCode/SplineFunction.R")

lam<-0.001

library(tseries) # loading the package time series

fileNames<-read.csv("G:/Dropbox/kinshipEvolution/DataAnalysis/fileNames.csv", quote="")

file<-read.delim(as.character(fileNames[45,]))

file <- file[which (file$tick >=10000),]

spline<-fnSpline(lam, file$tick, file$avgCoop)

plot(as.ts(file$kinPref))
plot(spline)

file$avgCoop<-spline$y


#ccf(file$kinPref, file$avgCoop, plot = TRUE)

#ccf(file$kinPref, file$avgGrSize, lag.max=1000)

#ccf(file$kinPref, file$rel, lag.max=0, plot = FALSE)

ccf(file$avgGrSize, file$rel, lag.max=10000, type = "covariance")

#ccf(file$avgGrSize, file$avgCoop, lag.max=5000)
				
#ccf(file$avgCoop, file$rel, lag.max=0, plot = FALSE)
