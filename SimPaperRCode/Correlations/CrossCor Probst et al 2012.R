# http://cran.r-project.org/web/packages/TSA/TSA.pdf
# http://icesjms.oxfordjournals.org/content/69/4/670/suppl/DC1
# Author: Ruth
###############################################################################


source("G:/PhDWork/RCode/SimPaperCode/SplineFunction.R")

library(tseries) # loading the package time series
library(TSA)
fileNames<-read.csv("G:/Dropbox/kinshipEvolution/DataAnalysis/fileNames.csv", quote="")


file<-read.delim(as.character(fileNames[58,]))

file <- file[which (file$tick >=10000),]

ts.s<-(as.ts(asin(file$avgCoop)))
ts.p<-(as.ts(asin(file$rel)))

plot(ts.p)

plot(diff(ts.p))

summary(lm(ts.p)~time(ts.p))
summary(lm(diff(ts.p)~time(diff(ts.p))))

acf(ts.p, lag=500)

acf(diff(ts.p), lag=10000)

pacf(ts.p, lag=10000)

pacf(diff(ts.p), lag=10000)

arima.ts.p=arima(diff(ts.p), xreg=time(ts.p))

arima.ts.p

acf(arima.ts.p$residuals)

prewhiten((ts.p),(ts.s),x.model=arima.ts.p, lag = 5000)

ccf(ts.p, ts.s, lag=5000)

