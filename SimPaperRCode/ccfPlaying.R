# TODO: Add comment
# 
# Author: Ruth
###############################################################################


#clean up
rm(list=ls())

#The 'ccf' function allows a simple way to get cross-correlations.
#Oddly, 'x' refers to the presumed endogenous variable and 'y' refers to the presumed exogenous variable.
#This is opposite of what is usually expected.
#Here is a Monte Carlo simulation describing how this works.
a<-rnorm(1000)
b<-rep(NA,1000)
b[1]<-0
for (i in 2:1000) b[i]<-.5*a[i-1]+rnorm(1)

#By the truth, lags of a should predict values of b.
#To get what we normally want from a CCF, x is your endogenous variable and y is your exogenous
ccf(x=a,y=b,lag.max=1000)
ccf(x=b,y=a,lag.max=50)

###ADDITIONAL ILLUSTRATION###
#Let's look at some cross-correlations of Greek tourism and terrorist attacks.
#We'll compare 'ccf' to Pearson correlation computation.

#Load data
data <- read.csv('http://monogan.myweb.uga.edu/teaching/ts/italy.csv')

#View data
plot(data$GRSHARE, type='l')
plot(data$ATTKGR, type="l")

#CCF of unfiltered variables
ccf.output<-ccf(y=data$ATTKGR, x=data$GRSHARE, 12); ccf.output
tour<-ts(data$GRSHARE)
terr<-ts(data$ATTKGR)
l.terr<-lag(terr,-1)
l2.terr<-lag(terr,-2)
l.tour<-lag(tour,-1)
l2.tour<-lag(tour,-2)
data.2<-na.omit(as.data.frame(ts.union(tour,terr,l.terr,l2.terr,l.tour,l2.tour)))

#compare
cor(data.2$tour,data.2$l.terr);ccf.output[1]
cor(data.2$tour,data.2$l2.terr);ccf.output[2]
cor(data.2$terr,data.2$l.tour);ccf.output[-1]
cor(data.2$terr,data.2$l2.tour);ccf.output[-2]

