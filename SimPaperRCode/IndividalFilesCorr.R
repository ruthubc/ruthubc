# TODO: Add comment
# 
# Author: Ruth
###############################################################################


library(tseries) # loading the package time series

fileNames<-read.csv("G:/Dropbox/kinshipEvolution/DataAnalysis/fileNames.csv", quote="")

file<-read.delim(as.character(fileNames[3,]))

file <- file[which (file$tick >=5000),]


#ccf(file$kinPref, file$avgCoop, plot = TRUE)




ccf(file$kinPref, file$avgGrSize, lag.max=1000)

ccf(file$kinPref, file$rel, lag.max=0, plot = FALSE)

ccf(file$avgGrSize, file$rel, lag.max=0, plot = FALSE)

ccf(file$avgGrSize, file$avgCoop, lag.max=0, plot = FALSE)

ccf(file$avgCoop, file$rel, lag.max=0, plot = FALSE)
