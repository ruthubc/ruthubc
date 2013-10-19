# TODO: Add comment
# 
# Author: Ruth
###############################################################################


# Spider size or weight vs nest size

library(plyr)
library(ggplot2)
require(lattice)

spiderData <- read.csv("RuthEcuador2013/CombinedNestVsWeight.csv")

spiders <- subset(spiderData, Instar != "FALSE")

#removes empty levels
spiders$Instar <- factor(spiders$Instar)

table(spiders$Instar)

levels(spiders$Instar)

Nests<-levels(spiders$NestID)

levels(spiders$NestID)[1]



hist(spiders$Weight.mg, breaks = 20)


histogram( ~ Weight.mg | Instar , data=spiders, type = "count", equal.widths = FALSE,
		layout=c(3,2) , scales= list(y=list(relation="free"), x=list(relation="free")), 
		breaks = 15 )

histogram( ~ LegLen.mm | Instar , data=spiders, type = "count", equal.widths = FALSE,
		layout=c(3,2) , scales= list(y=list(relation="free"), x=list(relation="free")), 
		breaks = 15 )

histogram( ~ HeadLength.mm | Instar , data=spiders, type = "count", equal.widths = FALSE,
		layout=c(3,2) , scales= list(y=list(relation="free"), x=list(relation="free")), 
		breaks = 15 )

Nest<-Nests[1]

subset(spiders, NestID == Nest)

ggplot(subset(spiders, NestID == Nests[5]) , aes(x=Weight.mg, fill = Instar)) + geom_bar()



SumizeWeightInstarOnly <- ddply(spiders, .(Instar), summarise,
		N = length(!is.na(Weight.mg)),
		mean = mean(Weight.mg, na.rm = TRUE),
		sd = sd(Weight.mg, na.rm = TRUE),
		coefvar= sd / mean		
)

SSummariseWeight <- ddply(spiders, .(lnArea, Instar), summarise,
		N = length(!is.na(Weight.mg)),
		mean = mean(Weight.mg, na.rm = TRUE),
		sd = sd(Weight.mg, na.rm = TRUE),
		coefvar= sd / mean		
		)
		
SumsWeightN <- subset(SSummariseWeight, N>9)


histogram( ~ coefvar | Instar , data=SumsWeightN, type = "count", equal.widths = FALSE,
		layout=c(3,2) , scales= list(y=list(relation="free"), x=list(relation="free")), 
		breaks = 8 )	

		
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
