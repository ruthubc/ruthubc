# TODO: Add comment
# 
# Author: Ruth
###############################################################################

library(data.table)

BoxData <- read.csv("RuthEcuador2013/BoxFeedingTrials/CombinedBoxData.csv")

levels(BoxData$Nest)




BoxData$WeightDiff <- BoxData$Weight.2 - BoxData$Weight.1

ggplot(BoxData, aes(x=TotalTimeFeeding, y = IndCapture)) + geom_point(shape= 16)

ggplot(SumsLegN , aes(x=lnArea, y = cvByN)) + geom_point(shape = 16) + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + 
		ggtitle(paste("Coefficient of variation of leg length when nest min of ", MinNoSpis, " spiders", sep = ""))+
		xlab("Log Approx. Nest Area") + ylab("CV of Len Length") + facet_wrap(~ Instar, scales = "free_y")



boxplot(BoxData$IndCapture)

##test for data table merging one to many works!

x <- data.table(one=1:6, two=c('a','b','c', 'a','b','c'))

y <- data.table(three = 1:3, two = c('a','b','c'))

setkey(x, two)

setkey(y, two)

merge(x, y)
