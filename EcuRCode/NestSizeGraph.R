
# 
# Author: Ruth
###############################################################################


# Spider size or weight vs nest size

library(plyr)
library(ggplot2)
require(reshape2)
library(nlme)
library(lme)

spiderData <- read.csv("RuthEcuador2013/NestSize/CombinedNestVsWeight.csv")

#removing eggs, parastised individuals and the outlier nest 44.3ex01 as the adults were particularly small

spiders <- subset(spiderData, Instar != "FALSE" & NestID != "44.3ex01"  & Instar !="egg" & 
				Instar != "pj" & Instar != "PST" & Instar != "juv3")

spiders$hunger <- spiders$Weight.mg/spiders$HeadLength.mm

#removes empty levels
spiders$Instar <- factor(spiders$Instar, levels= c("Adult", "Sub2", 
				"Sub1", "Juv4", "AdMale", "SubMale"))

Nests<-levels(spiders$NestID)

##########################################################################################
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

####histograms for each individual nest
pdf("RuthEcuador2013/NestSize/Graphs/IndNestGraphs.pdf", onefile = TRUE)

for (i in 1:length(Nests)){
	
print(ggplot(subset(spiders, NestID == Nests[i]) , aes(x=LegLen.mm, fill = Instar)) + 
				geom_bar(colour="black") + ggtitle(paste(Nests[i], "LegLen.mm", sep = " ")))
	

print(ggplot(subset(spiders, NestID == Nests[i]) , aes(x=HeadLength.mm, fill = Instar)) + 
				geom_bar(colour="black") + ggtitle(paste(Nests[i], "HeadLen.mm", sep = " ")))

}

dev.off()



## Summary of weight

SSummariseWeight <- ddply(spiders, .(NestID, lnArea, Instar), summarise,
		N = length(!is.na(Weight.mg)),
		mean = mean(Weight.mg, na.rm = TRUE),
		median = median(Weight.mg, na.rm = TRUE),
		sd = sd(Weight.mg, na.rm = TRUE),
		CV= sd / mean,
		IQR = IQR(Weight.mg, na.rm = TRUE),
		max = max(Weight.mg, na.rm=TRUE),
		min = min(Weight.mg, na.rm=TRUE),
		cvByN = (1+(1/(4*N))) * CV

		)
		
MinNoSpis <- 5
		
SumsWeightN <- subset(SSummariseWeight, N>MinNoSpis)

Instar<-levels(SumsWeightN$Instar)[c(2, 9, 8, 5, 1)]


###exporting graphs of mean weight against nest size, an individual graph for each 

pdf("RuthEcuador2013/NestSize/Graphs/MeanWeightVsNestArea.pdf", onefile = "TRUE")

#length(Instar)
#for(i in 1:2 ){
	
	MyInstar <- Instar[i]
	
ggplot(subset(SumsWeightN, Instar ==MyInstar) , aes(x=lnArea, y = mean)) + geom_point(shape = 16) + 
			geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE) + 
			ggtitle(paste("Mean of weight ", Instar[i], " if nest has ", MinNoSpis, " or more ", Instar[i], "'s", sep = ""))+
			xlab("Log Approx. Nest Area") + ylab("Mean Weight(mg)")
			
	
#}


##multiple graphs on one page
ggplot(SumsWeightN , aes(x=lnArea, y = mean)) + geom_point(shape = 16) + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + 
		ggtitle(paste("Mean of weight when nest has ", MinNoSpis, " or more", sep = ""))+
		xlab("Log Approx. Nest Area") + ylab("Mean Weight(mg)") + facet_wrap(~ Instar, scales = "free_y")



dev.off()

ggplot(SumsWeightN , aes(x=lnArea, y = min)) + geom_point(shape = 16) + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + 
		ggtitle(paste("Mean of weight when nest has ", MinNoSpis, " or more", sep = ""))+
		xlab("Log Approx. Nest Area") + ylab("Mean Weight(mg)") + facet_wrap(~ Instar, scales = "free_y")

lmMinWeight <- lm(min ~ lnArea, data = subset(SumsWeightN,Instar == "Adult" ))
anova(lmMinWeight)

### exporting graph of standardized variance 

pdf("RuthEcuador2013/NestSize/Graphs/CVWeightVsNestArea.pdf", onefile = "TRUE")

for(i in 1: length(Instar)){
	
	MyInstar <- Instar[i]
	
	print(ggplot(subset(SumsWeightN, Instar ==MyInstar) , aes(x=lnArea, y = cvByN)) + geom_point(shape = 16) + 
					geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + 
					ggtitle(paste("Coefficient of variation of weight ", Instar[i], " if nest has ", MinNoSpis, " or more ", Instar[i], "'s", sep = ""))+
					xlab("Log Approx. Nest Area") + ylab("C of V of Weight"))
	
	
}


ggplot(SumsWeightN , aes(x=lnArea, y = cvByN)) + geom_point(shape = 16) + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + 
		ggtitle(paste("Coefficient of variation of weight when nest has min ", MinNoSpis, " spiders", sep = ""))+
		xlab("Log Approx. Nest Area") + ylab("Mean Weight") + facet_wrap(~ Instar, scales = "free_y")


dev.off()

ggplot(SumsWeightN , aes(x=lnArea, y = cvByN)) + geom_point(shape = 16) + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + 
		ggtitle(paste("Coefficient of variation of weight when nest has min ", MinNoSpis, " spiders", sep = ""))+
		xlab("Log Approx. Nest Area") + ylab("Mean Weight") + facet_wrap(~ Instar, scales = "free_y")



### summarise leg length by nest area
SSummariseLeg <- ddply(spiders, .(NestID, lnArea, Instar), summarise,
		N = length(!is.na(LegLen.mm)),
		mean = mean(LegLen.mm, na.rm = TRUE),
		median = median(LegLen.mm, na.rm = TRUE),
		sd = sd(LegLen.mm, na.rm = TRUE),
		CV= sd / mean,
		IQR = IQR(LegLen.mm, na.rm = TRUE),
		IQRDivMed =  IQR/median,
		max = max(LegLen.mm, na.rm=TRUE), 
		min = min(LegLen.mm, na.rm = TRUE),
		cvByN = (1+(1/(4*N))) * CV
)

NMin <- 2
SumsLegN <- subset(SSummariseLeg, N > NMin)



pdf("RuthEcuador2013/NestSize/Graphs/MeanLegLengthVsNestArea.pdf", onefile = "TRUE")

for(i in 1: length(Instar)){
	
	MyInstar <- Instar[i]
	
	print(ggplot(subset(SumsLegN, Instar ==MyInstar) , aes(x=lnArea, y = mean)) + geom_point(shape = 16) + 
					geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + 
					ggtitle(paste("Mean Leg Length ", Instar[i], " if nest has ", MinNoSpis, " or more ", Instar[i], "'s", sep = ""))+
					xlab("Log Approx. Nest Area") + ylab("Mean Leg Length (mm)"))
	
	
}


##All graphs on one page

pdf("RuthEcuador2013/NestSize/Graphs/MaxLegLengthVsNestSize.pdf")


ggplot(SumsLegN , aes(x=lnArea, y = max)) + geom_point(shape = 16) + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + 
		ggtitle(paste("Max length of leg with min ", NMin, " spiders", sep = ""))+
		xlab("Log Approx. Nest Area") + ylab("Max Leg Length (mm)") + facet_wrap(~ Instar, scales = "free_y")



dev.off()

ggplot(SumsLegN , aes(x=lnArea, y = mean)) + geom_point(shape = 16) + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + 
		ggtitle(paste("Mean length of leg with min ", NMin, " spiders", sep = ""))+
		xlab("Log Approx. Nest Area") + ylab("Mean Leg Length (m3)") + facet_wrap(~ Instar, scales = "free_y")

lmMaxLeg <- lm(max ~ lnArea, data = subset(SumsLegN,Instar == "Adult" ))
anova(lmMaxLeg)


pdf("RuthEcuador2013/NestSize/Graphs/CVLegLengthVsNestArea.pdf", onefile = "TRUE")

for(i in 1: length(Instar)){
	
	MyInstar <- Instar[i]
	
	print(ggplot(subset(SumsLegN, Instar ==MyInstar) , aes(x=lnArea, y = cvByN)) + geom_point(shape = 16) + 
					geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + 
					ggtitle(paste("CV of Leg Length ", Instar[i], " if nest has ", MinNoSpis, " or more ", Instar[i], "'s", sep = ""))+
					xlab("Log Approx. Nest Area") + ylab("CV of Leg Length (mm)"))
	
	
}


##All graphs on one page

ggplot(SumsLegN , aes(x=lnArea, y = cvByN)) + geom_point(shape = 16) + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + 
		ggtitle(paste("Coefficient of variation of leg length when nest min of ", MinNoSpis, " spiders", sep = ""))+
		xlab("Log Approx. Nest Area") + ylab("CV of Len Length") + facet_wrap(~ Instar, scales = "free_y")


dev.off()


 #### Max Leg Lenght vs Nest Area

pdf("RuthEcuador2013/NestSize/Graphs/MaxLegLengthVsNestArea.pdf", onefile = "TRUE")

for(i in 1: length(Instar)){
	
	MyInstar <- Instar[i]
	
	print(ggplot(subset(SumsLegN, Instar ==MyInstar) , aes(x=lnArea, y = max)) + geom_point(shape = 16) + 
					geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + 
					ggtitle(paste("Max Leg Length ", Instar[i], " if nest has ", MinNoSpis, " or more ", Instar[i], "'s", sep = ""))+
					xlab("Log Approx. Nest Area") + ylab("Max Leg Length (mm)"))
	
	
}

dev.off()



### Hunger levels


SSummariseHunger <- ddply(spiders, .(NestID, lnArea, Instar), summarise,
		N = length(!is.na(hunger)),
		mean = mean(hunger, na.rm = TRUE),
		median = median(hunger, na.rm = TRUE),
		sd = sd(hunger, na.rm = TRUE),
		CV= sd / mean,
		IQR = IQR(hunger, na.rm = TRUE),
		IQRDivMed =  IQR/median,
		max = max(hunger, na.rm=TRUE), 
		min = min(hunger, na.rm = TRUE),
		cvByN = (1+4/N) * CV
)

NMin <- 2
SumsHungerN <- subset(SSummariseHunger, N > NMin)



pdf("RuthEcuador2013/NestSize/Graphs/MeanHungerVsNestArea.pdf", onefile = "TRUE")

for(i in 1: length(Instar)){
	
	MyInstar <- Instar[i]
	
	print(ggplot(subset(SumsHungerN, Instar ==MyInstar) , aes(x=lnArea, y = mean)) + geom_point(shape = 16) + 
					geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + 
					ggtitle(paste("Mean Hunger ", Instar[i], " if nest has ", MinNoSpis, " or more ", Instar[i], "'s", sep = ""))+
					xlab("Log Approx. Nest Area") + ylab("Mean hunger"))
	
	
}

dev.off()


pdf("RuthEcuador2013/NestSize/Graphs/CVHungerVsNestArea.pdf", onefile = "TRUE")

for(i in 1: length(Instar)){
	
	MyInstar <- Instar[i]
	
	print(ggplot(subset(SumsHungerN, Instar ==MyInstar) , aes(x=lnArea, y = cvByN)) + geom_point(shape = 16) + 
					geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + 
					ggtitle(paste("CV of hunger ", Instar[i], " if nest has ", MinNoSpis, " or more ", Instar[i], "'s", sep = ""))+
					xlab("Log Approx. Nest Area") + ylab("CV of hunger)"))
	
	
}

dev.off()



#######################################################################################
###single females against multi-female nests

Adults <- subset(spiders, Instar == "Adult" & FemalesHaveEggsOrJuvs != "n" & 
				FemalesHaveEggsOrJuvs != "unk" )

pdf("RuthEcuador2013/NestSize/Graphs/SingleMultipeNests.pdf", onefile= TRUE)

boxplot(Weight.mg ~ Approx..Single., data = Adults, 
		main = "Weight of adult females in single and multiple nests", ylab = "Weight/mg" )


boxplot(LegLen.mm ~ Approx..Single., data = Adults, 
		main = "Leg length of adults in single and multiple nests", ylab = "Leg Length/mm" )


boxplot(HeadLength.mm ~ Approx..Single., data = Adults, 
		main = "Head length of adults in single and multiple nests", ylab = "Head Length/mm" )

boxplot(AbdmLen.mm ~ Approx..Single., data = Adults, 
		main = "Abdomen length of adults in single and multiple nests", ylab = "Abdomen Length/mm" )


dev.off()

##stats test of weight etc against nest type single or multiple

t.test(Adults$Weight.mg ~ Adults$Approx..Single. )
t.test(Adults$LegLen.mm ~ Adults$Approx..Single. )
t.test(Adults$HeadLength.mm ~ Adults$Approx..Single. )
t.test(Adults$AbdmLen.mm ~ Adults$Approx..Single. )


###head size vs weight graph
ggplot(subset(spiders, Instar == "Adult"), aes(x=HeadLength.mm, y = Weight.mg)) + 
		 geom_point(shape=16) + ggtitle("title") + geom_smooth()
 

 ## Weight vs time difference__ not really much of a difference
 boxplot(Weight.mg ~ DateDiff, data = subset(spiders, Instar == "Adult"))
 

 
 
 ##Different Instars weight against each other but lots of individual graphs so not nice!!
 
 SumN <- subset(SSummariseWeight, N>0)
 
 SumN$NestID <- factor(SumN$NestID)
 
pdf("RuthEcuador2013/NestSize/Graphs/DiffWeightBtwnInstarsVsNestArea.pdf", onefile= TRUE)
 
for(i in 1: length(Instar)){ 

	MyInstar <- Instar[i]
	
	for(j in 1: length(Instar)){ 
		
		MyInstar2<- Instar[j]
		
		if(MyInstar !=  MyInstar2){
 
 	TranSum <- subset(SumN, Instar == MyInstar2 | Instar == MyInstar, 
		 select = c(NestID, Instar, mean, lnArea) )
 
	TranSum$Instar <- factor(TranSum$Instar)
	
	InstarCols<- dcast(TranSum, NestID +  lnArea ~ Instar, value.var= "mean",  drop = T) #transpose data

	InstarCols<-na.omit(InstarCols)

	InstarCols$Diff<-(InstarCols[,3] - InstarCols[,4])

	print(ggplot(data = InstarCols, aes(x = lnArea, y = Diff)) + geom_point() +
		 stat_smooth(method="lm", se=TRUE, formula = y~ poly(x, 1)) +
		 ggtitle(paste("Difference in Mean Weight Between ", MyInstar2,  " and ", Instar[i], "s", sep = ""))+
		 xlab("Log Approx. Nest Area") + ylab("Difference in mean weight"))
}else{}

}}

dev.off()


##Diff in leg length between different instars



SumN <- subset(SSummariseLeg, N>0)

SumN$NestID <- factor(SumN$NestID)

InstarCols<- dcast(subset(SumN, select = c(NestID, Instar, mean, lnArea)), 
		NestID +  lnArea ~ Instar, value.var= "mean",  drop = T) #transpose data


SpiderDiffs <- ddply(InstarCols, .(NestID, lnArea), summarise,
		AdultVsSub2 = Adult - Sub2,
		AdultVsSub1 = Adult - Sub2,
		AdultVsJuv4 = Adult - Juv4,
		AdultVsAdMale = Adult - AdMale,
		Sub2VsSub1 = Sub2 - Sub1,
		Sub2VsJuv4 = Sub2 - Juv4,
		Sub1VsJuv4 = Sub1 - Juv4,
		Sub2VsAdMale = Sub2 - AdMale,
		AdMaleVsSubMale = AdMale - SubMale
		

)

#unstacks the data
SpiderDiffs <- melt(SpiderDiffs, id.vars=c("NestID","lnArea"))#dcast(SpiderDiffs, NestID + lnArea + Instar)

pdf("RuthEcuador2013/NestSize/Graphs/LegLengthDiffBtwnInstarVsNestArea.pdf")

ggplot(data = SpiderDiffs, aes(x = lnArea, y = value)) + geom_point() +
				stat_smooth(method="lm", se=TRUE, formula = y~ poly(x, 2)) +
				facet_wrap(~ variable, scales = "free_y") + xlab("Log of Nest Area") +
				ylab("Difference in Leg Length (mm)") + xlim(2.4, 4.35)+ 
				ggtitle("Difference in Leg Length Between Instars")
		
		
dev.off()
		
		


InstarCols<-na.omit(InstarCols)

InstarCols$Diff<-(InstarCols[,3] - InstarCols[,4])


cols<- colnames(InstarCols)

paste(cols[3], "Vs", cols[4], sep = "")

pdf("RuthEcuador2013/NestSize/Graphs/DiffLegLenBtwnInstarsVsNestArea.pdf", onefile= TRUE)

for(i in 1: length(Instar)){ 
	
	MyInstar <- Instar[i]
	
	for(j in 1: length(Instar)){ 
		
		MyInstar2<- Instar[j]
		
		if(MyInstar !=  MyInstar2){
			
			TranSum <- subset(SumN, Instar == MyInstar2 | Instar == MyInstar, 
					select = c(NestID, Instar, mean, lnArea) )
			
			TranSum$Instar <- factor(TranSum$Instar)
			
			InstarCols<- dcast(TranSum, NestID +  lnArea ~ Instar, value.var= "mean",  drop = T) #transpose data
			
			InstarCols<-na.omit(InstarCols)
			
			InstarCols$Diff<-(InstarCols[,3] - InstarCols[,4])
			
			print(ggplot(data = InstarCols, aes(x = lnArea, y = Diff)) + geom_point() +
							stat_smooth(method="lm", se=TRUE, formula = y~ poly(x, 1)) +
							ggtitle(paste("Difference in Mean Weight Between ", MyInstar2,  " and ", Instar[i], "s", sep = ""))+
							xlab("Log Approx. Nest Area") + ylab("Difference in mean weight"))
		}else{}
		
	}}

dev.off()


################################################################################
######Statistical tests

spiders$WeightSqr<- (spiders$Weight.mg)^2


test <- lme(lnArea ~ WeightSqr, random = ~1|NestID ,  
		data = subset(spiders, Instar = "Adult"), na.action = "na.omit")

summary(test)

anova(test)

min(spiders$WeightSqr, na.rm = TRUE)
min(spiders$Weight.mg, na.rm = TRUE)
