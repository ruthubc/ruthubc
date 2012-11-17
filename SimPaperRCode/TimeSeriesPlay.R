###trying to test for correlation or counter-correlatoin between things
# 
# Author: Ruth
#gpTime<-data.frame(file$kinPref, file$avgGrSize)[which(file$tick>x),]
###############################################################################

library(tseries) # loading the package time series

crsCrl<-function(A, B){
	
	
	tsA=ts(data.frame(A)[which(file$tick>x),], start=0)
	tsB=ts(data.frame(B)[which(file$tick>x),], start=0)
	ccfOut<-ccf(tsA, tsB, lag.max = 0, type =  "correlation", plot = F)
	
	return(as.numeric(ccfOut$acf))
	
}

x=10000 # number of runs to remove before testing

fileNames<-read.csv("D:/Dropbox/kinshipEvolution/DataAnalysis/fileNames.csv", quote="")

kinGr<-c()


for (i in 50:60){
	
	
	
file<-read.delim(as.character(fileNames[1,]))


cc<-crsCrl(file$kinPref, file$avgGrSize)



kinGr<-c(kinGr, cc)


}

crsCrl(file$kinPref, file$avgCoop)
crsCrl(file$kinPref, file$rel)

crsCrl(file$avgGrSize, file$avgCoop)
crsCrl(file$avgGrSize, file$rel)

crsCrl(file$rel, file$avgCoop)



##plotting CCF function
A<- file$avgCoop
B<- file$kinPref
tsA=ts(data.frame(A)[which(file$tick>x),], start=0)
tsB=ts(data.frame(B)[which(file$tick>x),], start=0)
ccf(tsA, tsB, lag.max = 2000, type =  "correlation", plot = T)








##########################################


##testing for white noise in R
Box.test(tsA, lag=5000, type = "Ljung")


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

