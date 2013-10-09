# TODO: Add comment
# 
# Author: Ruth
###############################################################################


# Spider size or weight vs nest size

library(plyr)
library(ggplot2)

spiderData <- read.csv("RuthEcuador2013/CombinedNestVsWeight.csv")

spiders <- subset(spiderData, Instar != "FALSE")

#removes empty levels
spiders$Instar <- factor(spiders$Instar)

table(spiders$Instar)

levels(spiders$Instar)

levels(spiders$NestID)


SSummarise <- ddply(spiders, .(NestID, Instar), summarise,
		N = length(!is.na(Weight.mg)),
		mean = mean(Weight.mg, na.rm = TRUE),
		sd = sd(Weight.mg, na.rm = TRUE),
		coefvar= sd / mean		
		)
		
		
plot(mean ~ NestID, data = SSummarise,
		type = "l")

nlevels(spiders$Instar)



#using ggplot to make like graph of nests 
ggplot(data = SSummarise, aes(x = N, y = mean, colour = Instar)) + geom_line()


