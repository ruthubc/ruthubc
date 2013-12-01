
# 
# Author: Ruth
###############################################################################
#TODO: single female nests.. FemaleHaveEggsOrJuvs not coming through--correct in access

# Spider size or weight vs nest size

library(plyr)
library(ggplot2)
require(reshape2)
library(nlme)
library(gridExtra)
#library(lme) wrong packages

spiderData <- read.csv("RuthEcuador2013/NestSize/CombinedNestVsWeight.csv")

#removing eggs, parastised individuals and the outlier nest 44.3ex01 as the adults were particularly small

spiders <- subset(spiderData, Instar != "FALSE" & NestID != "44.3ex01"  & Instar !="egg" & 
				Instar != "pj" & Instar != "PST" & Instar != "juv3")

spiders$hunger <- spiders$Weight.mg/spiders$HeadLength.mm

#removes empty levels
spiders$Instar <- factor(spiders$Instar, levels= c("Adult", "Sub2", 
				"Sub1", "Juv4", "AdMale", "SubMale"))

####log transforming Female count, weight and leg length
spiders$logWeight <- log10(spiders$Weight.mg)
spiders$logCtFm <- log10(spiders$CountFemales)
spiders$logLeg<- log10(spiders$LegLen.mm)
spiders$logHead <-log10(spiders$HeadLength.mm)
spiders$logAbdm<- log10(spiders$AbdmLen.mm)

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

#GRAPH TO EXPORT
#############################################################################
## Summary of weight

SSummariseWeight <- ddply(spiders, .(NestID, logCtFm, Instar), summarise,
		N = length(!is.na(Weight.mg)),
		mean = mean(logWeight, na.rm = TRUE),
		median = median(logWeight, na.rm = TRUE),
		sd = sd(logWeight, na.rm = TRUE),
		CV= sd / mean,
		IQR = IQR(logWeight, na.rm = TRUE),
		max = max(logWeight, na.rm=TRUE),
		min = min(logWeight, na.rm=TRUE),
		cvByN = (1+(1/(4*N))) * CV

		)
		
MinNoSpis <- 2
		
SumsWeightN <- subset(SSummariseWeight, N>MinNoSpis)

Instar<-levels(SumsWeightN$Instar)[c(2, 9, 8, 5, 1)]


###exporting graphs of mean weight against nest size, an individual graph for each 

pdf("RuthEcuador2013/NestSize/Graphs/MeanWeightVsNestSize.pdf", onefile = "TRUE")


##multiple graphs on one page
ggplot(SumsWeightN , aes(x=logCtFm, y = mean)) + geom_point(shape = 16) + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + 
		ggtitle(paste("Mean of weight when nest has ", MinNoSpis, " or more", sep = ""))+
		xlab("Log Number of females in nest") + ylab("Log Mean Weight(mg)") + 
		facet_wrap(~ Instar, scales = "free_y", ncol = 4)



dev.off()

#GRAPH TO EXPORT
####################################################################################
### exporting graph of standardized variance 

pdf("RuthEcuador2013/NestSize/Graphs/CVWeightVsNestSize.pdf", onefile = "TRUE")

ggplot(SumsWeightN , aes(x=logWeight, y = cvByN)) + geom_point(shape = 16) + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + 
		ggtitle(paste("Coefficient of variation of weight when nest has min ", MinNoSpis, " spiders", sep = ""))+
		xlab("Log Number of Females") + ylab("Log Mean Weight") + 
		facet_wrap(~ Instar, scales = "free_y", ncol=4)


dev.off()

###########################################################################################

#GRAPH TO EXPORT ------ LEG LENGTH

### summarise leg length by nest area
SSummariseLeg <- ddply(spiders, .(NestID, logCtFm, Instar), summarise,
		N = length(!is.na(LegLen.mm)),
		mean = mean(logLeg, na.rm = TRUE),
		median = median(logLeg, na.rm = TRUE),
		sd = sd(logLeg, na.rm = TRUE),
		CV= sd / mean,
		IQR = IQR(logLeg, na.rm = TRUE),
		IQRDivMed =  IQR/median,
		max = max(logLeg, na.rm=TRUE), 
		min = min(logLeg, na.rm = TRUE),
		cvByN = (1+(1/(4*N))) * CV
)

NMin <- 2
SumsLegN <- subset(SSummariseLeg, N > NMin)

pdf("RuthEcuador2013/NestSize/Graphs/MeanLegLengthVsNestSize.pdf", onefile = "TRUE")

ggplot(SumsLegN , aes(x=logCtFm, y = mean)) + geom_point(shape = 16) + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + 
		ggtitle(paste("Mean leg length with min ", NMin, " spiders", sep = ""))+
		xlab("Log number of females") + ylab("Log leg length") + 
		facet_wrap(~ Instar, scales = "free_y", ncol = 4)

dev.off()

lmMaxLeg <- lm(max ~ logCtFm, data = subset(SumsLegN,Instar == "Adult" ))
anova(lmMaxLeg)

##GRAPH TO EXPORT---- CV OF LEG
###################################################################################
pdf("RuthEcuador2013/NestSize/Graphs/CVLegLengthVsNestSize.pdf", onefile = "TRUE")

ggplot(SumsLegN , aes(x=logCtFm, y = cvByN)) + geom_point(shape = 16) + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + 
		ggtitle(paste("Coefficient of variation of leg length (min ", MinNoSpis, " spiders counted)", sep = ""))+
		xlab("Log Approx. Nest Area") + ylab("CV of Len Length") + facet_wrap(~ Instar, scales = "free_y")

dev.off()



### Hunger levels
###################################################################################
SSummariseHunger <- ddply(spiders, .(NestID, logCtFm, Instar), summarise,
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

pdf("RuthEcuador2013/NestSize/Graphs/MeanHungerVsNestSize.pdf", onefile = "TRUE")

for(i in 1: length(Instar)){
	
	MyInstar <- Instar[i]
	
	print(ggplot(subset(SumsHungerN, Instar ==MyInstar) , aes(x=logCtFm, y = mean)) + geom_point(shape = 16) + 
					geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + 
					ggtitle(paste("Mean Hunger ", Instar[i], " if nest has ", MinNoSpis, " or more ", Instar[i], "'s", sep = ""))+
					xlab("Log Approx. Nest Area") + ylab("Mean hunger"))
	
	
}

dev.off()


pdf("RuthEcuador2013/NestSize/Graphs/CVHungerVsNestSize.pdf", onefile = "TRUE")

for(i in 1: length(Instar)){
	
	MyInstar <- Instar[i]
	
	print(ggplot(subset(SumsHungerN, Instar ==MyInstar) , aes(x=logCtFm, y = cvByN)) + geom_point(shape = 16) + 
					geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + 
					ggtitle(paste("CV of hunger ", Instar[i], " if nest has ", MinNoSpis, " or more ", Instar[i], "'s", sep = ""))+
					xlab("Log Approx. Nest Area") + ylab("CV of hunger)"))
	
	
}

dev.off()


#GRAPH TO EXPORT
#######################################################################################
###single females against multi-female nests

Adults <- subset(spiders, Instar == "Adult" & FemalesHaveEggsOrJuvs != "n" & 
				FemalesHaveEggsOrJuvs != "unk" )

pdf("RuthEcuador2013/NestSize/Graphs/SingleMultipeNests.pdf", onefile= TRUE)

p1 = ggplot(Adults, aes(x=Approx..Single, y=logWeight)) + geom_boxplot() +
		ggtitle("Weight") + ylab("Log Weight")

p2 = ggplot(Adults, aes(x=Approx..Single, y=logLeg)) + geom_boxplot() +
		ggtitle("Leg Length") + ylab("Log Leg Length")

p3 = ggplot(Adults, aes(x=Approx..Single, y=logHead)) + geom_boxplot() +
		ggtitle("Head Length") + ylab("Log Head Length")

p4 = ggplot(Adults, aes(x=Approx..Single, y=logAbdm)) + geom_boxplot() +
		ggtitle("Abdomen Length") + ylab("Log Abdomen Length")

grid.arrange(p1, p2, p3, p4, ncol = 2, main = "Multiple vs (approx) single nests")

dev.off()

####################################################################################
## Only including 44.4 nest in the analysis (large nest is 44.4EX03

AdSub <- subset(Adults, grepl("44.4EXM", spiders$NestID) | NestID == "44.4EX03")

pdf("RuthEcuador2013/NestSize/Graphs/SingleMultipeNests44.4ONLY.pdf", onefile= TRUE)

p1 = ggplot(AdSub, aes(x=Approx..Single, y=logWeight)) + geom_boxplot() +
		ggtitle("Weight") + ylab("Log Weight")

p2 = ggplot(AdSub, aes(x=Approx..Single, y=logLeg)) + geom_boxplot() +
		ggtitle("Leg Length") + ylab("Log Leg Length")

p3 = ggplot(AdSub, aes(x=Approx..Single, y=logHead)) + geom_boxplot() +
		ggtitle("Head Length") + ylab("Log Head Length")

p4 = ggplot(AdSub, aes(x=Approx..Single, y=logAbdm)) + geom_boxplot() +
		ggtitle("Abdomen Length") + ylab("Log Abdomen Length")

grid.arrange(p1, p2, p3, p4, ncol = 2, main = "Multiple vs (approx) single nests 44.4 only")

dev.off()

############################################################################################
##stats test of weight etc against nest type single or multiple

t.test(Adults$Weight.mg ~ Adults$Approx..Single. )
t.test(Adults$LegLen.mm ~ Adults$Approx..Single. )
t.test(Adults$HeadLength.mm ~ Adults$Approx..Single. )
t.test(Adults$AbdmLen.mm ~ Adults$Approx..Single. )

############################################################################################
###head size vs weight graph
ggplot(subset(spiders, Instar == "Adult"), aes(x=HeadLength.mm, y = Weight.mg)) + 
		 geom_point(shape=16) + ggtitle("title") + geom_smooth()
 

 ## Weight vs time difference__ not really much of a difference
 boxplot(Weight.mg ~ DateDiff, data = subset(spiders, Instar == "Adult"))
 
 
 ##Different Instars weight against each other but lots of individual graphs so not nice!!
 
 SumN <- subset(SSummariseWeight, N>0)
 
 SumN$NestID <- factor(SumN$NestID)
 
pdf("RuthEcuador2013/NestSize/Graphs/DiffWeightBtwnInstarsVsNestSize.pdf", onefile= TRUE)
 
for(i in 1: length(Instar)){ 

	MyInstar <- Instar[i]
	
	for(j in 1: length(Instar)){ 
		
		MyInstar2<- Instar[j]
		
		if(MyInstar !=  MyInstar2){
 
 	TranSum <- subset(SumN, Instar == MyInstar2 | Instar == MyInstar, 
		 select = c(NestID, Instar, mean, logCtFm) )
 
	TranSum$Instar <- factor(TranSum$Instar)
	
	InstarCols<- dcast(TranSum, NestID +  logCtFm ~ Instar, value.var= "mean",  drop = T) #transpose data

	InstarCols<-na.omit(InstarCols)

	InstarCols$Diff<-(InstarCols[,3] - InstarCols[,4])

	print(ggplot(data = InstarCols, aes(x = logCtFm, y = Diff)) + geom_point() +
		 stat_smooth(method="lm", se=TRUE, formula = y~ poly(x, 1)) +
		 ggtitle(paste("Difference in Mean Weight Between ", MyInstar2,  " and ", Instar[i], "s", sep = ""))+
		 xlab("Log Approx. Nest Area") + ylab("Difference in mean weight"))
}else{}

}}

dev.off()


#GRAPH TO EXPORT ---- Diff in leg length between different instars
########################################################################################

SumN <- subset(SSummariseLeg, N>0)

SumN$NestID <- factor(SumN$NestID)

InstarCols<- dcast(subset(SumN, select = c(NestID, Instar, mean, logCtFm)), 
		NestID +  logCtFm ~ Instar, value.var= "mean",  drop = T) #transpose data

SpiderDiffs <- ddply(InstarCols, .(NestID, logCtFm), summarise,
		AdultVsSub2 = Adult - Sub2,
		Sub2VsSub1 = Sub2 - Sub1,
		Sub1VsJuv4 = Sub1 - Juv4,
		Sub2VsAdMale = Sub2 - AdMale,
		Sub1VsSubMale = Sub1 - Sub2,
		AdMaleVsSubMale = AdMale - SubMale
		

)

##########################################################################################
#unstacks the data
SpiderDiffs <- melt(SpiderDiffs, id.vars=c("NestID","logCtFm"))#dcast(SpiderDiffs, NestID + logCtFm + Instar)

pdf("RuthEcuador2013/NestSize/Graphs/LegLengthDiffBtwnInstarVsNestSize.pdf")

ggplot(data = SpiderDiffs, aes(x = logCtFm, y = value)) + geom_point() +
				stat_smooth(method="lm", se=TRUE, formula = y~ poly(x, 2)) +
				facet_wrap(~ variable, scales = "free_y") + xlab("Log of Nest Area") +
				ylab("Difference in log Leg Length") + 
				ggtitle("Difference in Leg Length Between Instars") # + xlim(2.4, 4.35)
dev.off()



#GRAPH TO EXPORT ---- Diff in weight between different instars
########################################################################################

SumN <- subset(SSummariseWeight, N>0)

SumN$NestID <- factor(SumN$NestID)

InstarCols<- dcast(subset(SumN, select = c(NestID, Instar, mean, logCtFm)), 
		NestID +  logCtFm ~ Instar, value.var= "mean",  drop = T) #transpose data

SpiderDiffs <- ddply(InstarCols, .(NestID, logCtFm), summarise,
		AdultVsSub2 = Adult - Sub2,
		Sub2VsSub1 = Sub2 - Sub1,
		Sub1VsJuv4 = Sub1 - Juv4,
		Sub2VsAdMale = Sub2 - AdMale,
		Sub1VsSubMale = Sub1 - Sub2,
		AdMaleVsSubMale = AdMale - SubMale
)

#unstacks the data
SpiderDiffs <- melt(SpiderDiffs, id.vars=c("NestID","logCtFm"))#dcast(SpiderDiffs, NestID + logCtFm + Instar)

pdf("RuthEcuador2013/NestSize/Graphs/WeightDiffBtwnInstarVsNestSize.pdf")

ggplot(data = SpiderDiffs, aes(x = logCtFm, y = value)) + geom_point() +
		stat_smooth(method="lm", se=TRUE, formula = y~ poly(x, 2)) +
		facet_wrap(~ variable, scales = "free_y") + xlab("Log of Nest Area") +
		ylab("Difference in log weight") + 
		ggtitle("Difference in Weight Between Instars") # + xlim(2.4, 4.35)
dev.off()



		

##### not entirely sure what this is for.. perhapas a more complicated way of doing leg len diff btwn instar
InstarCols<-na.omit(InstarCols)

InstarCols$Diff<-(InstarCols[,3] - InstarCols[,4])


cols<- colnames(InstarCols)

paste(cols[3], "Vs", cols[4], sep = "")

pdf("RuthEcuador2013/NestSize/Graphs/DiffLegLenBtwnInstarsVsNestSize.pdf", onefile= TRUE)

for(i in 1: length(Instar)){ 
	
	MyInstar <- Instar[i]
	
	for(j in 1: length(Instar)){ 
		
		MyInstar2<- Instar[j]
		
		if(MyInstar !=  MyInstar2){
			
			TranSum <- subset(SumN, Instar == MyInstar2 | Instar == MyInstar, 
					select = c(NestID, Instar, mean, logCtFm) )
			
			TranSum$Instar <- factor(TranSum$Instar)
			
			InstarCols<- dcast(TranSum, NestID +  logCtFm ~ Instar, value.var= "mean",  drop = T) #transpose data
			
			InstarCols<-na.omit(InstarCols)
			
			InstarCols$Diff<-(InstarCols[,3] - InstarCols[,4])
			
			print(ggplot(data = InstarCols, aes(x = logCtFm, y = Diff)) + geom_point() +
							stat_smooth(method="lm", se=TRUE, formula = y~ poly(x, 1)) +
							ggtitle(paste("Difference in Mean Weight Between ", MyInstar2,  " and ", Instar[i], "s", sep = ""))+
							xlab("Log Approx. Nest Area") + ylab("Difference in mean weight"))
		}else{}
		
	}}

dev.off()


################################################################################
######Statistical tests

spiders$WeightSqr<- (spiders$Weight.mg)^2


test <- lme(logCtFm ~ WeightSqr, random = ~1|NestID ,  
		data = subset(spiders, Instar = "Adult"), na.action = "na.omit")

summary(test)

anova(test)

min(spiders$WeightSqr, na.rm = TRUE)
min(spiders$Weight.mg, na.rm = TRUE)


########################################################################################
##GRID ARRANGE TEST



p1 = ggplot(spiders, aes(x = Date.Collected, y = Key)) + geom_point()

p2 = ggplot(spiders, aes(x = NestID, y = Key)) + geom_point()

p3 = ggplot(spiders, aes(x = NestID, y = Key)) + geom_point()

p4 = ggplot(spiders, aes(x = NestID, y = Key)) + geom_point()

grid.arrange(p1, p2, p3, p4, ncol = 2, main = "Main title")


###subset testing

test <-subset(spiders, grepl("16.2", spiders$NestID))

test<-subset(spiders, grepl("44.4EXM", spiders$NestID) | NestID == "44.4EX03")


