# TODO: Add comment
# 
# Author: Ruth
###############################################################################


##trying to test for correlation or counter-correlatoin between things
library(tseries) # loading the package time series



file <- read.delim("D:/Dropbox/kinshipEvolution/NewSimulations2010-2011/r0.10c0.02b0.6/r0.1c0.02b0.6_RPT.pf.csv.series")

x=2000 # number of runs to remove before testing

gpTime<-data.frame(file$avgGrSize, file$rel)[which(file$tick>x),]

tsGrSize=ts(gpTime$file.avgGrSize, start=0)

tsCoop=ts(gpTime$file.rel, start=0)


ccf(tsGrSize, tsCoop, lag.max = 20000, type = c("correlation", "covariance"), plot = T)

gpTime$file.avgGrSize









spec<-spectrum(gpTime)

1/(min(spec$freq))

1/(max(spec$freq))

freq<-spec$freq
str(freq)


lag.plot(gpTime)#really not sure what this does
acf(gpTime) # autocorrelation graph

n<-nrow(file)
j<-n-x



hist(gpTime)

mean(gpTime)

#testing series for autocorrelation

Box.test(gpTime) # def significant

