
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

### histograme of cvbyN

ggplot(SpiNestAve, aes(cvByNHung)) + geom_histogram() # using log doesn't help the normality
ggplot(SpiNestAve, aes(logcvByNLeg)) + geom_histogram() # this is better log transformed


#GRAPH TO EXPORT
#############################################################################
## Summary of weight
	
MinNoSpis <- 1; SumsWeightN <- subset(SpiNestAve, N>MinNoSpis)

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

ggplot(spidersMul, aes(x=logCtFm, y = logWeight )) + geom_point() + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) +
		facet_wrap(~ Instar, scales = "free_y")


dev.off()

###########################################################################################

#GRAPH TO EXPORT ------ LEG LENGTH

### summarise leg length by nest size

NMin <- 1
SumsLegN <- subset(SpiNestAve, N > NMin)

pdf("RuthEcuador2013/NestSize/Graphs/LegLengthVsNestSize.pdf", height=8, width=11)

ggplot(SumsLegN , aes(x=logCtFm, y = log10(meanLeg))) + geom_point(aes(colour = NestID), shape = 16) + 
		geom_smooth(method = "lm", formula =y ~  poly(x, raw = TRUE), se = FALSE) + 
		ggtitle(paste("Mean leg length with min ", NMin, " spiders", sep = ""))+
		xlab("Log number of females") + ylab("Log leg length") + mytheme+ 
		facet_wrap(~ Instar, scales = "free_y", ncol = 4) + theme(legend.position = "none")


ggplot(subset(SumsLegN, Instar != "SubMaler"), aes(x=logCtFm, y = cvByNLeg)) +  geom_point(aes(colour = NestID), shape = 16) + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + 
		ggtitle(paste("Coefficent of variation of leg length"))+ ylim(0, 0.17) + 
		xlab("Log number of females") + ylab("Log coefficient of variation") + mytheme+
		facet_wrap(~ Instar, scales= "free_y", ncol = 4) + theme(legend.position = "none")



dev.off()

##graphs of progression of growth within nest

ggplot(SumsLegN, aes(x=Instar, y= mean)) + geom_point(aes(colour=size)) + geom_line(aes(group=NestID, colour = size))

### Hunger levels
###################################################################################

NMin <- 1
SumsHungerN <- subset(SpiNestAve, N > NMin)

pdf("RuthEcuador2013/NestSize/Graphs/MeanHungerVsNestSize.pdf", , height=8, width=11)

ggplot(SumsHungerN , aes(x=logCtFm, y = meanCondSq)) + geom_point(aes(colour = NestID), shape = 16) + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = FALSE) +
		xlab("Log number of females") + ylab("Mean condition") + mytheme+
		facet_wrap(~ Instar, scales = "free_y", ncol = 4) + theme(legend.position = "none")

ggplot(SumsHungerN , aes(x=logCtFm^2, y = cvByNHung)) + geom_point(aes(colour = NestID), shape = 16) + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + 
		ggtitle(paste("CV hunger")) + ylim(-0.02, 0.70) + 
		xlab("Log number of females") + ylab("Coefficient of variation of hunger") + mytheme+
		facet_wrap(~ Instar, scales = "free_y",  ncol = 4) + theme(legend.position = "none")


dev.off()


### head size
###################################################################################




ggplot(SpiNestAve , aes(x=logCtFm, y = meanHead)) + geom_point(aes(colour = NestID), shape = 16) + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + 
		ggtitle(paste("Log mean hunger")) +
		xlab("Log number of females") + ylab("Mean Head") + mytheme+
		facet_wrap(~ Instar, scales = "free_y", ncol = 4) + theme(legend.position = "none")




#GRAPH TO EXPORT
#######################################################################################
###single females against multi-female nests


pdf("RuthEcuador2013/NestSize/Graphs/SingleMultipeNests44.pdf", height = 6, width = 6)

ggplot(Spis44, aes(x=type, y=hunger)) + geom_boxplot() + 
		ggtitle("Hunger 44.4 nests only ") + ylab("Hunger") + mytheme +theme(axis.title.x = element_blank()) +
		scale_x_discrete(breaks=c("multiple", "single"), labels=c("source: multiple", "propagules: single"))

ggplot(Spis44, aes(x=type, y=logLeg)) + geom_boxplot() +
		ggtitle("Leg Length 44.4 nests only ") + ylab("Log Leg Length") +
		mytheme +  theme(axis.title.x = element_blank()) +
		scale_x_discrete(breaks=c("multiple", "single"), labels=c("source: multiple", "propagules: single"))



#grid.arrange(p1, p2, ncol =2, main = "Multiple vs (approx) single nests")

dev.off()


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

DifSmWt <- ddply(spidersMul, .(NestID, logCtFm, Instar), summarise,
		N = length(!is.na(Weight.mg)),
		mean = mean(cond, na.rm = TRUE)
)

DifSmWt <- subset(DifSmWt, N>5)

DifSmWt$NestID <- factor(DifSmWt$NestID)

WtInsrCols<- dcast(subset(DifSmWt, select = c(NestID, Instar, mean, logCtFm)), 
		NestID +  logCtFm ~ Instar, value.var= "mean",  drop = T) #transpose data

WtDiffs <- ddply(WtInsrCols, .(NestID, logCtFm), summarise,
		AdultVsSub2 = Adult - Sub2
)

#unstacks the data
SpiderWtDiffs <- melt(WtDiffs, id.vars=c("NestID","logCtFm"))#dcast(SpiderDiffs, NestID + logCtFm + Instar)

pdf("RuthEcuador2013/NestSize/Graphs/CondDiffBtwnInstarVsNestSize.pdf",  height=6.5, width=9)

ggplot(data = WtDiffs, aes(x = logCtFm, y = AdultVsSub2)) + geom_point() +
		stat_smooth(method="lm", se=TRUE, formula = y~ poly(x, 1)) + xlab("Log Nest Size") +
		ylab("Difference in mean condition (Adult - Sub2)") + mytheme + xlim(1.8,3.8) #+
		#ggtitle("Difference in Weight Between Instars") 
dev.off()

CondDiffLm <- lm(AdultVsSub2 ~ logCtFm, data = WtDiffs)

summary(CondDiffLm)


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

