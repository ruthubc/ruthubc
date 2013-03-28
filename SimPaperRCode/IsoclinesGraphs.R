# TODO: Add comment
# 
# Author: Ruth
# http://www.statmethods.net/management/subset.html
###############################################################################


averages <- read.csv("kinshipEvolution/DataAnalysis/averages10000.csv")

subAves<-subset(averages, (C==0.06))

subAves<-subAves[order(subAves$grSizeAvgInd),]

X11()

plot(subAves$grSizeAvgInd, subAves$avgCoop, type = "b" , pch=16 )

subAves<-subset(averages, (Beta==0.4))

subAves<-subAves[order(subAves$grSizeAvgInd),]

points(subAves$grSizeAvgInd, subAves$avgCoop, pch=3, col='red')

lines(subAves$grSizeAvgInd, subAves$avgCoop, col = 'red')

cop<-(1:10)/10

n<-cop/0.02

lines(n, cop)

abline(v=10)

v<-c(5, 5,5,5,5,5,5,5,5,5)

lines(v, cop)
