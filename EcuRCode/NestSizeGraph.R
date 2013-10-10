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


SSummariseWeight <- ddply(spiders, .(lnArea, Instar), summarise,
		N = length(!is.na(Weight.mg)),
		mean = mean(Weight.mg, na.rm = TRUE),
		sd = sd(Weight.mg, na.rm = TRUE),
		coefvar= sd / mean		
		)
		
		
plot(mean ~ lnArea, data = SSummarise,
		type = "l")

nlevels(spiders$Instar)



#using ggplot to make like graph of nests http://docs.ggplot2.org/0.9.3.1/stat_smooth.html
ggplot(data = SSummariseWeight, aes(x = lnArea, y = mean, colour = Instar)) + geom_point() + 
		ggtitle("Mean Weight vs approx. nest size") + stat_smooth(method="lm", se=FALSE)				) #formula = y ~ poly(x, 2), http://docs.ggplot2.org/0.9.3.1/stat_smooth.html
		
ggplot(data = SSummariseWeight, aes(x = lnArea, y = coefvar, colour = Instar)) + geom_point() + 
			ggtitle("Coef of Variation vs approx. nest size") + stat_smooth(method="lm", se=FALSE,
					formula = y~ poly(x, 2))

SSummariseLeg <- ddply(spiders, .(lnArea, Instar), summarise,
		N = length(!is.na(LegLen.mm)),
		mean = mean(LegLen.mm, na.rm = TRUE),
		sd = sd(LegLen.mm, na.rm = TRUE),
		coefvar= sd / mean		
)

ggplot(data = SSummariseLeg, aes(x = lnArea, y = mean, colour = Instar)) + geom_point() + 
		ggtitle("Mean Leg Length vs approx. nest size") + stat_smooth(method="lm", se=FALSE,
				formula = y~ poly(x, 2))

ggplot(data = SSummariseLeg, aes(x = lnArea, y = coefvar, colour = Instar)) + geom_point() + 
		ggtitle("Co of Var Leg Length vs approx. nest size") + stat_smooth(method="lm", se=FALSE,
				formula = y~ poly(x, 2))


SSummariseHead <- ddply(spiders, .(lnArea, Instar), summarise,
		N = length(!is.na(HeadLength.mm)),
		mean = mean(HeadLength.mm, na.rm = TRUE),
		sd = sd(HeadLength.mm, na.rm = TRUE),
		coefvar= sd / mean		
)

ggplot(data = SSummariseHead, aes(x = lnArea, y = mean, colour = Instar)) + geom_point()
