# TODO: Add comment
# 
# Author: Ruth
# http://www.statmethods.net/management/subset.html
###############################################################################


averages <- read.csv("G:/Dropbox/kinshipEvolution/DataAnalysis/averages.csv")

subAves<-subset(averages, (C==0.02))

plot(subAves$grSizeAvgInd, subAves$avgCoop)

subAves<-subset(averages, (Beta==0.4))

points(subAves$grSizeAvgInd, subAves$avgCoop, pch=3)

cop<-(1:10)/10

n<-cop/0.02

lines(n, cop)

abline(v=10)

v<-c(5, 5,5,5,5,5,5,5,5,5)

lines(v, cop)
