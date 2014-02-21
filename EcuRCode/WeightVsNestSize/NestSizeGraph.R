
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
library(histogram)
source("G:/PhDWork/EclipseWorkspace/R/EcuRCode/WeightVsNestSize/NestSizeData.R")
mytheme <-theme_bw(base_size=15)  + theme(plot.title = element_text(vjust=2), panel.margin= unit(0.75, "lines"), axis.title.y = element_text(vjust=0),
		plot.margin=unit(c(1,1,1.5,1.2),"cm"), panel.border = element_rect(fill = NA, colour = "grey", linetype=1, size = 1))


#+theme(axis.line = element_line(colour = "black"))#theme(panel.border = element_rect(fill = NA, colour = "grey", linetype=1, size = 1))
#library(lme) wrong packages



##########################################################################################
### histograms to check distribution and outliers etc.

histogram( ~ Weight.mg | Instar , data=spiders, type = "count", equal.widths = FALSE,
		layout=c(3,2) , scales= list(y=list(relation="free"), x=list(relation="free")), 
	)

histogram( ~ LegLen.mm | Instar , data=spiders, type = "count", equal.widths = FALSE,
		layout=c(3,2) , scales= list(y=list(relation="free"), x=list(relation="free")), 
		breaks = 15 )

histogram( ~ LnLegLen | Instar , data=spiders, type = "count", equal.widths = FALSE,
		layout=c(3,2) , scales= list(y=list(relation="free"), x=list(relation="free")), 
		breaks = 15 )

ggplot(spiders, aes(LegLen.mm)) + geom_histogram()

histogram( ~ HeadLength.mm | Instar , data=spiders, type = "count", equal.widths = FALSE,
		layout=c(3,2) , scales= list(y=list(relation="free"), x=list(relation="free")), 
		breaks = 15 )

###histogram checking distribution of catogrical nest sizes

ggplot(subset(spiders, !duplicated(NestID)), aes(x=logCtFm, fill=size))+ geom_histogram()

table(subset(spiders, !duplicated(NestID))$size)

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

SSummariseWeight <- ddply(spiders, .(NestID, CountFemales, type, logCtFm, Instar), summarise,
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
		
MinNoSpis <- 1; SumsWeightN <- subset(SSummariseWeight, N>MinNoSpis)

##removed spiders in single(ish) female nests
SumsWeightN <- subset(SSummariseWeight, type == "multiple") #options multiple or single

Instar<-levels(SumsWeightN$Instar)[c(2, 9, 8, 5, 1)]

##removing the now blank factor levels
SumsWeightN$NestID <-factor(SumsWeightN$NestID)

###exporting graphs of mean weight against nest size, an individual graph for each 

pdf("RuthEcuador2013/NestSize/Graphs/WeightVsNestSize.pdf", height=8, width=13)

nlevels(SumsWeightN$NestID)

##multiple graphs on one page
ggplot(SumsWeightN , aes(x=logCtFm, y = mean)) + geom_point() + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + 
		ggtitle(paste("Mean of weight of multiple nests"))+
		xlab("Log Number of females in nest") + ylab("Log Mean Weight") + 
		facet_wrap(~ Instar, scales = "free_y", ncol = 4) + mytheme


ggplot(SumsWeightN , aes(x=logCtFm, y = cvByN)) + geom_point(shape = 16) + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + 
		ggtitle(paste("CV of weight with multiple spider nests"))+
		xlab("Log Number of Females") + ylab("Log Mean Weight") + 
		facet_wrap(~ Instar, scales = "free_y", ncol=4) + mytheme


dev.off()

###########################################################################################

#GRAPH TO EXPORT ------ LEG LENGTH

### summarise leg length by nest area
SSummariseLeg <- ddply(spiders, .(NestID, logCtFm, Instar, type), summarise,
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

NMin <- 1
SumsLegN <- subset(SSummariseLeg, N > NMin)

pdf("RuthEcuador2013/NestSize/Graphs/LegLengthVsNestSize.pdf", height=8, width=11)

ggplot(SumsLegN , aes(x=logCtFm, y = mean)) + geom_point(shape = 16) + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + 
		ggtitle(paste("Mean leg length with min ", NMin, " spiders", sep = ""))+
		xlab("Log number of females") + ylab("Log leg length") + 
		facet_wrap(~ Instar, scales = "free_y", ncol = 4)

ggplot(subset(SumsLegN, type =='multiple'), aes(x=logCtFm, y = mean)) + geom_point(shape = 16) + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE, colour = "black") + 
		ggtitle(paste("Mean leg length for multiple nests"))+
		xlab("Log number of females") + ylab("Log leg length") + 
		facet_wrap(~ Instar, scales = "free_y", ncol = 4)  + mytheme 


ggplot(SumsLegN , aes(x=logCtFm, y = mean)) + geom_point(shape = 16) + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + 
		ggtitle(paste("Mean leg length with min ", NMin, " spiders", sep = ""))+
		xlab("Log number of females") + ylab("Log leg length") + 
		facet_wrap(~ Instar, scales = "free_y", ncol = 4)

ggplot(subset(SumsLegN, type =='multiple') , aes(x=logCtFm, y = cvByN)) + geom_point(shape = 16) + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE, colour = 'black') + 
		ggtitle(paste("Coefficient of variation of leg length with multiple nests", sep = ""))+
		xlab("Log number of females") + ylab("CV of Len Length") + 
		facet_wrap(~ Instar, scales = "free_y", ncol = 4) + mytheme


dev.off()

##graphs of progression of growth within nest

ggplot(SumsLegN, aes(x=Instar, y= mean)) + geom_point(aes(colour=size)) + geom_line(aes(group=NestID, colour = size))

### Hunger levels
###################################################################################
SSummariseHunger <- ddply(spiders, .(NestID, logCtFm, Instar, type), summarise,
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
SumsHungerN <- subset(SSummariseHunger, type == "multiple" )

pdf("RuthEcuador2013/NestSize/Graphs/MeanHungerVsNestSize.pdf", onefile = "TRUE")

ggplot(subset(SumsHungerN) , aes(x=logCtFm, y = mean)) + geom_point(shape = 16) + 
					geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + 
					ggtitle(paste("Mean Hunger if nest has ", NMin, " or more spiders", sep = ""))+
					xlab("Log Approx. Nest Area") + ylab("Mean hunger") + facet_wrap(~Instar, scales = "free_y")

ggplot(subset(SumsHungerN) , aes(x=logCtFm, y = log(cvByN))) + geom_point(shape = 16) + 
			geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + 
			ggtitle(paste("CV Hunger if nest has ", NMin, " or more spiders", sep = ""))+
			xlab("Log Approx. Nest Area") + ylab("CV hunger") #+ facet_wrap(~Instar, scales = "free_y")			

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

pdf("RuthEcuador2013/NestSize/Graphs/SingleMultipeNests.pdf", height = 6, width = 9.5)

p1 = ggplot(Adults, aes(x=Approx..Single., y=logWeight)) + geom_boxplot() +
		ggtitle("Weight") + ylab("Log Weight") + mytheme +theme(axis.title.x = element_blank())


p2 = ggplot(Adults, aes(x=Approx..Single., y=logLeg)) + geom_boxplot() +
		ggtitle("Leg Length") + ylab("Log Leg Length") +
		mytheme +  theme(axis.title.x = element_blank())

p3 = ggplot(Adults, aes(x=Approx..Single., y=logHead)) + geom_boxplot() +
		ggtitle("Head Length") + ylab("Log Head Length") + theme(axis.title.x = element_blank())

p4 = ggplot(Adults, aes(x=Approx..Single., y=logHung)) + geom_boxplot() +
		ggtitle("Hunger") + ylab("Log Hunger") + theme(axis.title.x = element_blank())

grid.arrange(p1, p2, ncol =2, main = "Multiple vs (approx) single nests")

dev.off()

####################################################################################
## Only including 44.4 nest in the analysis (large nest is 44.4EX03

AdSub <- subset(Adults, grepl("44.4EXM", Adults$NestID) | NestID == "44.4EX03"  )

pdf("RuthEcuador2013/NestSize/Graphs/SingleMultipeNests44.4ONLY.pdf", height = 6, width = 9.5)

#p1 = 
ggplot(AdSub, aes(x=Approx..Single., y=logWeight)) + geom_boxplot() +
		ggtitle("Weight") + ylab("Log Weight") +  theme(axis.title.x = element_blank())

p2 = ggplot(AdSub, aes(x=Approx..Single., y=logLeg)) + geom_boxplot() +
		ggtitle("Leg Length") + ylab("Log Leg Length") + theme(axis.title.x = element_blank())

p3 = ggplot(AdSub, aes(x=Approx..Single., y=logHead)) + geom_boxplot() +
		ggtitle("Head Length") + ylab("Log Head Length") + theme(axis.title.x = element_blank())

p4 = ggplot(AdSub, aes(x=Approx..Single., y=logHung)) + geom_boxplot() +
		ggtitle("Hunger") + ylab("Log Hunger") + theme(axis.title.x = element_blank())

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


#GRAPH TO EXPORT ---- Diff in LEG LENGTH between different instars
########################################################################################

DifSmLeg <- ddply(spiders, .(NestID, logCtFm, Instar), summarise,
		N = length(!is.na(LegLen.mm)),
		mean = mean(LegLen.mm, na.rm = TRUE)
)

DifSmLeg <- subset(DifSmLeg, N>0)

DifSmLeg$NestID <- factor(DifSmLeg$NestID)

InsrColsLeg<- dcast(subset(DifSmLeg, select = c(NestID, Instar, mean, logCtFm)), 
		NestID +  logCtFm ~ Instar, value.var= "mean",  drop = T) #transpose data

LegDiffs <- ddply(InsrColsLeg, .(NestID, logCtFm), summarise,
		AdultVsSub2 = Adult - Sub2,
		Sub2VsSub1 = Sub2 - Sub1,
		Sub1VsJuv4 = Sub1 - Juv4,
		AdMaleVsSub2 = AdMale - Sub2,
		SubMaleVsSub1 = SubMale - Sub1,
		AdMaleVsSubMale = AdMale - SubMale
)
#unstacks the data
SpiderDiffsLeg <- melt(LegDiffs, id.vars=c("NestID","logCtFm"))#dcast(SpiderDiffs, NestID + logCtFm + Instar)

#####makes the graph
pdf("RuthEcuador2013/NestSize/Graphs/LegLengthDiffBtwnInstarVsNestSize.pdf", height=6.5, width=9)

ggplot(SpiderDiffsLeg, aes(x = logCtFm, y = value)) + geom_point() +
				stat_smooth(method="lm", se=TRUE, formula = y~ poly(x, 1)) +
				facet_wrap(~ variable, scales = "free") + xlab("Log of Number of females") +
				ylab("Difference in Leg Length") + xlim(1.10,3.75)
				#ggtitle("Difference in Leg Length Between Instars")
dev.off()


#GRAPH TO EXPORT ---- Diff in weight between different instars
########################################################################################

DifSmWt <- ddply(spiders, .(NestID, logCtFm, Instar), summarise,
		N = length(!is.na(Weight.mg)),
		mean = mean(Weight.mg, na.rm = TRUE)
)

DifSmWt <- subset(DifSmWt, N>0)

DifSmWt$NestID <- factor(DifSmWt$NestID)

WtInsrCols<- dcast(subset(DifSmWt, select = c(NestID, Instar, mean, logCtFm)), 
		NestID +  logCtFm ~ Instar, value.var= "mean",  drop = T) #transpose data

WtDiffs <- ddply(WtInsrCols, .(NestID, logCtFm), summarise,
		AdultVsSub2 = Adult - Sub2,
		Sub2VsSub1 = Sub2 - Sub1,
		Sub1VsJuv4 = Sub1 - Juv4,
		AdMaleVsSub2 = AdMale - Sub2,
		SubMaleVsSub1 = SubMale - Sub1,
		AdMaleVsSubMale = AdMale - SubMale
)

#unstacks the data
SpiderWtDiffs <- melt(WtDiffs, id.vars=c("NestID","logCtFm"))#dcast(SpiderDiffs, NestID + logCtFm + Instar)

pdf("RuthEcuador2013/NestSize/Graphs/WeightDiffBtwnInstarVsNestSize.pdf",  height=6.5, width=9)

ggplot(data = SpiderWtDiffs, aes(x = logCtFm, y = value)) + geom_point() +
		stat_smooth(method="lm", se=TRUE, formula = y~ poly(x, 1)) +
		facet_wrap(~ variable, scales = "free") + xlab("Log Number of spiders") +
		ylab("Log Diff in mean weight") + xlim(1.10,3.75) #+
		#ggtitle("Difference in Weight Between Instars") 
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


### histogram of cvbyN

ggplot(SpiNestAve, aes(log(cvByNLeg))) + geom_histogram()

table<-subset(spiders, NestID == "45.1EXa06" & Instar == "Juv4" )

