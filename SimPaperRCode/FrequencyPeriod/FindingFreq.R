# Attempting to find period and frequency of time series
# 
# Author: Ruth
##############################################################################

library(tseries)
library(TTR)
library(TSA)


fileNames<-read.csv("kinshipEvolution/DataAnalysis/fileNames.csv", quote="", col.names="filenames")

largeFile<-read.delim(as.character(fileNames[78,]))

largeFile <- largeFile[which (largeFile$tick >=49400),]

largeFile$avgCoop <- asin(sqrt(largeFile$avgCoop))
largeFile$rel<-asin(sqrt(largeFile$rel))
largeFile$kinPref<-asin(sqrt(largeFile$kinPref))
largeFile$avgGrSize<-log(largeFile$avgGrSize)

spl<-fnSpline(0.0001, largeFile$tick, largeFile$avgGrSize)

timeSer=ts(spl)

timeSer=ts(largeFile$avgGrSize)

plot.ts(timeSer)
find.freq(timeSer)


timeSMA <- SMA(timeSer, n = 10)
plot.ts(timeSMA)

gpcomponents <- decompose(timeSer)

ped <- periodogram(timeSer, log = 'yes', plot = FALSE)

max(ped$freq)

1/ped$spec[ped$freq == max(ped$freq)]

## from https://onlinecourses.science.psu.edu/stat510/node/71

x<- timeSer

len <- (length(timeSer))

FF<- abs(fft(x)/sqrt(len))^2

P = (4/len)*FF[1:((len/2)+1)] # Only need the first (n/2)+1 values of the FFT result.
f = (0:(len/2))/len # this creates harmonic frequencies from 0 to .5 in steps of 1/128.
plot(f, P, type="l") # This plots the periodogram; type = “l” creates a line plot.  Note: l is lowercase L, not number 1.


spec <-spectrum(timeSer, spans = 2)

#spec <- spec.pgram(timeSer, spans = 10) # spans is the smoothing function.

1/spec$freq[spec$spec ==max(spec$spec)]