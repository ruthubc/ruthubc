# TODO: Add comment
# 
# Author: Ruth
###############################################################################


BoxData <- read.csv("RuthEcuador2013/BoxFeedingTrials/CombinedBoxData.csv")

levels(BoxData$Nest)


BoxData$WeightDiff <- BoxData$Weight.2 - BoxData$Weight.1

ggplot(BoxData, aes(x=TotalTimeFeeding, y = IndCapture)) + geom_point(shape= 16)

ggplot(SumsLegN , aes(x=lnArea, y = cvByN)) + geom_point(shape = 16) + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + 
		ggtitle(paste("Coefficient of variation of leg length when nest min of ", MinNoSpis, " spiders", sep = ""))+
		xlab("Log Approx. Nest Area") + ylab("CV of Len Length") + facet_wrap(~ Instar, scales = "free_y")



boxplot(BoxData$IndCapture)
