# TODO: Add comment
# 
# Author: Ruth
###############################################################################


BoxData <- read.csv("RuthEcuador2013/BoxFeedingTrials/CombinedBoxData.csv")

levels(BoxData$Nest)


BoxData$WeightDiff <- BoxData$Weight.2 - BoxData$Weight.1

