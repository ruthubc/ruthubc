# TODO: Add comment
# 
# Author: Ruth
###############################################################################


averages <- read.csv("D:/Dropbox/kinshipEvolution/DataAnalysis/averages.csv")

subAve<-subset(averages, C==0.02)

plot(subAve$avgCoop, subAve$grSizeAvgInd)


plot(averages$stableGrSize, averages$avgGrSize)

plot(averages$optGrSize, averages$avgGrSize)

