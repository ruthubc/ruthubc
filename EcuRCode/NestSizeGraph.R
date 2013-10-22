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


Nests<-levels(spiders$NestID)

### histograms to check distribution and outliers etc.


histogram( ~ Weight.mg | Instar , data=spiders, type = "count", equal.widths = FALSE,
		layout=c(3,2) , scales= list(y=list(relation="free"), x=list(relation="free")), 
	)

histogram( ~ LegLen.mm | Instar , data=spiders, type = "count", equal.widths = FALSE,
		layout=c(3,2) , scales= list(y=list(relation="free"), x=list(relation="free")), 
		breaks = 15 )

histogram( ~ HeadLength.mm | Instar , data=spiders, type = "count", equal.widths = FALSE,
		layout=c(3,2) , scales= list(y=list(relation="free"), x=list(relation="free")), 
		breaks = 15 )




# making individual histograms for each nest
pdf("RuthEcuador2013/Graphs/IndNestGraphs.pdf", onefile = TRUE)

for (i in 1:length(Nests)){
	
print(ggplot(subset(spiders, NestID == Nests[i]) , aes(x=LegLen.mm, fill = Instar)) + 
				geom_bar(colour="black") + ggtitle(paste(Nests[i], "LegLen.mm", sep = " ")))
	

print(ggplot(subset(spiders, NestID == Nests[i]) , aes(x=HeadLength.mm, fill = Instar)) + 
				geom_bar(colour="black") + ggtitle(paste(Nests[i], "HeadLen.mm", sep = " ")))

}

dev.off()



SumizeWeightInstarOnly <- ddply(spiders, .(Instar), summarise,
		N = length(!is.na(Weight.mg)),
		mean = mean(Weight.mg, na.rm = TRUE),
		sd = sd(Weight.mg, na.rm = TRUE),
		coefvar= sd / mean		
)

SSummariseWeight <- ddply(spiders, .(lnArea, Instar), summarise,
		N = length(!is.na(Weight.mg)),
		mean = mean(Weight.mg, na.rm = TRUE),
		median = median(Weight.mg, na.rm = TRUE),
		sd = sd(Weight.mg, na.rm = TRUE),
		coefvar= sd / mean,
		IQR = IQR(Weight.mg, na.rm = TRUE),
		IQRDivMed =  IQR/median,
		Diff = IQRDivMed - coefvar

		)
		
SumsWeightN <- subset(SSummariseWeight, N>15 & lnArea > 2.05)


ggplot(SumsWeightN, aes(x=lnArea, y = IQRDivMed)) + geom_point(shape = 16) + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 3, raw = TRUE), se = TRUE)


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


###single females against multi-female nests

Adults <- subset(spiders, Instar == "Adult" & FemalesHaveEggsOrJuvs != "n" & 
				FemalesHaveEggsOrJuvs != "unk" )

pdf("RuthEcuador2013/Graphs/SingleMultipeNests.pdf", onefile= TRUE)

boxplot(Weight.mg ~ Approx..Single., data = Adults, 
		main = "Weight of adult females in single and multiple nests", ylab = "Weight/mg" )


boxplot(LegLen.mm ~ Approx..Single., data = Adults, 
		main = "Leg length of adults in single and multiple nests", ylab = "Leg Length/mm" )


boxplot(HeadLength.mm ~ Approx..Single., data = Adults, 
		main = "Head length of adults in single and multiple nests", ylab = "Head Length/mm" )

boxplot(AbdmLen.mm ~ Approx..Single., data = Adults, 
		main = "Abdomen length of adults in single and multiple nests", ylab = "Abdomen Length/mm" )

dev.off()

t.test(Adults$Weight.mg ~ Adults$Approx..Single. )
t.test(Adults$LegLen.mm ~ Adults$Approx..Single. )
t.test(Adults$HeadLength.mm ~ Adults$Approx..Single. )
t.test(Adults$AbdmLen.mm ~ Adults$Approx..Single. )


###size vs weight graph


ggplot(subset(spiders, Instar == "Adult"), aes(x=HeadLength.mm, y = Weight.mg)) + 
		 geom_point(shape=16) + ggtitle("title") + geom_smooth()
 

 ## Weight vs time difference

 boxplot(Weight.mg ~ DateDiff, data = subset(spiders, Instar == "Adult"))
