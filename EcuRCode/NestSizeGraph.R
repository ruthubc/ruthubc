# TODO: Add comment
# 
# Author: Ruth
###############################################################################


# Spider size or weight vs nest size

library(plyr)
library(ggplot2)
require(lattice)
require(reshape2)
require(gridExtra)

spiderData <- read.csv("RuthEcuador2013/CombinedNestVsWeight.csv")

#removing eggs and the outlier nest 44.3ex01 as the adults were particularly small

spiders <- subset(spiderData, Instar != "FALSE" & NestID != "44.3ex01"  & Instar !="egg" & 
				Instar != "pj" & Instar != "PST" & Instar != "juv3")

spiders$hunger <- spiders$Weight.mg/spiders$HeadLength.mm

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



## Summary of weight

SSummariseWeight <- ddply(spiders, .(NestID, lnArea, Instar), summarise,
		N = length(!is.na(Weight.mg)),
		mean = mean(Weight.mg, na.rm = TRUE),
		median = median(Weight.mg, na.rm = TRUE),
		sd = sd(Weight.mg, na.rm = TRUE),
		CV= sd / mean,
		IQR = IQR(Weight.mg, na.rm = TRUE),
		max = max(Weight.mg, na.rm=TRUE),
		cvByN = (1+4/N) * CV

		)
		
MinNoSpis <- 2
		
SumsWeightN <- subset(SSummariseWeight, N>MinNoSpis)

Instar<-levels(SumsWeightN$Instar)[c(2, 9, 8, 5, 1)]


###exporting graphs of mean weight against nest size, an individual graph for each 

pdf("RuthEcuador2013/Graphs/MeanWeightVsNestArea.pdf", onefile = "TRUE")

mylist <-c()
#length(Instar)
for(i in 1:2 ){
	
	MyInstar <- Instar[i]
	
	gph<- ggplot(subset(SumsWeightN, Instar ==MyInstar) , aes(x=lnArea, y = mean)) + geom_point(shape = 16) + 
			geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE) + 
			ggtitle(paste("Mean of weight ", Instar[i], " if nest has ", MinNoSpis, " or more ", Instar[i], "'s", sep = ""))+
			xlab("Log Approx. Nest Area") + ylab("Mean Weight(mg)")
	
	mylist <- c(mylist, gph)
			
	
}


##multiple graphs on one page
gph<- ggplot(SumsWeightN , aes(x=lnArea, y = mean)) + geom_point(shape = 16) + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE) + 
		ggtitle(paste("Mean of weight ", Instar[i], " if nest has ", MinNoSpis, " or more ", Instar[i], "'s", sep = ""))+
		xlab("Log Approx. Nest Area") + ylab("Mean Weight(mg)") + facet_wrap(~ Instar, scales = "free_y")

gph

grid.arrange(arrangeGrob(arrangeGrob(mylist), ncol=1))

dev.off()

### exporting graph of standardized variance 

pdf("RuthEcuador2013/Graphs/CoefVarWeightVsNestArea.pdf", onefile = "TRUE")

for(i in 1: length(Instar)){
	
	MyInstar <- Instar[i]
	
	print(ggplot(subset(SumsWeightN, Instar ==MyInstar) , aes(x=lnArea, y = cvByN)) + geom_point(shape = 16) + 
					geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + 
					ggtitle(paste("Coefficient of variation of weight ", Instar[i], " if nest has ", MinNoSpis, " or more ", Instar[i], "'s", sep = ""))+
					xlab("Log Approx. Nest Area") + ylab("C of V of Weight(mg)"))
	
	
}

dev.off()




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
		cvByN = (1+4/N) * CV
)

NMin <- 2
SumsLegN <- subset(SSummariseLeg, N > NMin)



pdf("RuthEcuador2013/Graphs/MeanLegLengthVsNestArea.pdf", onefile = "TRUE")

for(i in 1: length(Instar)){
	
	MyInstar <- Instar[i]
	
	print(ggplot(subset(SumsLegN, Instar ==MyInstar) , aes(x=lnArea, y = mean)) + geom_point(shape = 16) + 
					geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + 
					ggtitle(paste("Mean Leg Length ", Instar[i], " if nest has ", MinNoSpis, " or more ", Instar[i], "'s", sep = ""))+
					xlab("Log Approx. Nest Area") + ylab("Mean Leg Length (mm)"))
	
	
}

dev.off()


pdf("RuthEcuador2013/Graphs/CVLegLengthVsNestArea.pdf", onefile = "TRUE")

for(i in 1: length(Instar)){
	
	MyInstar <- Instar[i]
	
	print(ggplot(subset(SumsLegN, Instar ==MyInstar) , aes(x=lnArea, y = cvByN)) + geom_point(shape = 16) + 
					geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + 
					ggtitle(paste("CV of Leg Length ", Instar[i], " if nest has ", MinNoSpis, " or more ", Instar[i], "'s", sep = ""))+
					xlab("Log Approx. Nest Area") + ylab("CV of Leg Length (mm)"))
	
	
}

dev.off()


 #### Max Leg Lenght vs Nest Area

pdf("RuthEcuador2013/Graphs/MaxLegLengthVsNestArea.pdf", onefile = "TRUE")

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



pdf("RuthEcuador2013/Graphs/MeanHungerVsNestArea.pdf", onefile = "TRUE")

for(i in 1: length(Instar)){
	
	MyInstar <- Instar[i]
	
	print(ggplot(subset(SumsHungerN, Instar ==MyInstar) , aes(x=lnArea, y = mean)) + geom_point(shape = 16) + 
					geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + 
					ggtitle(paste("Mean Hunger ", Instar[i], " if nest has ", MinNoSpis, " or more ", Instar[i], "'s", sep = ""))+
					xlab("Log Approx. Nest Area") + ylab("Mean hunger"))
	
	
}

dev.off()


pdf("RuthEcuador2013/Graphs/CVHungerVsNestArea.pdf", onefile = "TRUE")

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
 

 
 
 ##Different Instars weight against each other
 
 SumN <- subset(SSummariseWeight, N>0)
 
 SumN$NestID <- factor(SumN$NestID)
 
pdf("RuthEcuador2013/Graphs/DiffWeightBtwnInstarsVsNestArea.pdf", onefile= TRUE)
 
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



SumN <- subset(SSummariseWeight, N>0)

SumN$NestID <- factor(SumN$NestID)

InstarCols<- dcast(subset(SumN, select = c(NestID, Instar, mean, lnArea)), 
		NestID +  lnArea ~ Instar, value.var= "mean",  drop = T) #transpose data


SpiderDiffs <- ddply(InstarCols, .(NestID, lnArea), summarise,
		AdultVsSub2 = Adult - Sub2,
		AdultVsSub1 = Adult - Sub2,
		AdultVsJuv4 = Adult - Juv4,
		AdultVsMale = Adult - AdMale,
		Sub2VsSub1 = Sub2 - Sub1,
		Sub2VsJuv4 = Sub2 - Juv4,
		Sub1VsJuv4 = Sub1 - Juv4,
		Sub2VsAdMale = Sub2 - AdMale

)

#unstacks the data
SpiderDiffs <- melt(SpiderDiffs, id.vars=c("NestID","lnArea"))#dcast(SpiderDiffs, NestID + lnArea + Instar)



ggplot(data = SpiderDiffs, aes(x = lnArea, y = value)) + geom_point() +
				stat_smooth(method="lm", se=TRUE, formula = y~ poly(x, 1)) +
				facet_wrap(~ variable, scales = "free_y")


InstarCols<-na.omit(InstarCols)

InstarCols$Diff<-(InstarCols[,3] - InstarCols[,4])


cols<- colnames(InstarCols)

paste(cols[3], "Vs", cols[4], sep = "")

pdf("RuthEcuador2013/Graphs/DiffLegLenBtwnInstarsVsNestArea.pdf", onefile= TRUE)

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
