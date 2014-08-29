
# Author: Ruth
###############################################################################

library('plyr')

spiderData <- read.csv("RuthEcuador2013/NestSize/CombinedNestVsWeight.csv")

#removing eggs, parastised individuals and the outlier nest 44.3ex01 as the adults were particularly small
spiders <- subset(spiderData, Instar != "FALSE" & NestID != "44.3ex01"  & Instar !="egg" & 
				Instar != "pj" & Instar != "PST" & Instar != "juv3")

spiders <- subset(spiders, Instar != "Juv4" | (Instar == "Juv4" & LegLen.mm > 0.82))

##replacing ad males with sub bodies as sub males
spiders$Instar<-as.character(spiders$Instar) ; spiders$Instar[spiders$AdMaleSubBd == "y"] <- "SubMale"
spiders$Instar <-as.factor(spiders$Instar)

#Calculating spider hunger
spiders$hunger <- spiders$HeadLength.mm/spiders$Weight.mg

#removing empty levels and ordering for graph
spiders$Instar <- factor(spiders$Instar, levels= c("Juv4", "Sub1", 
				"Sub2", "Adult", "SubMale", "AdMale"))

#changing to simplier name
spiders$type <- spiders$Approx..Single.

####log transforming Female count, weight and leg length
spiders$logWeight <- log10(spiders$Weight.mg)
spiders$logCtFm <- log10(spiders$CountFemales)
spiders$logLeg<- log10(spiders$LegLen.mm)
spiders$logHead <-log10(spiders$HeadLength.mm)

spiders$logAbdm<- log10(spiders$AbdmLen.mm)
spiders$logHung <- log10(spiders$hunger)

Nests<-levels(spiders$NestID)

######################################################################################
##### catorgizing nests into small mediuum and large

spiders$NestSize<-as.factor(ifelse(spiders$Approx..Single =="single", "single",
				ifelse(spiders$CountFemales <200, "small", 
						ifelse(spiders$CountFemales <1000, "medium", "large"))))

#Adding km to use as a random
spiders$km <- substring((as.character(spiders$NestID)), 0, 4)


SpiNestAve<- ddply(spiders, .(NestID, type, Instar, logCtFm), summarise, # need to discount trials where no feeding obs and eve
		N = length(!is.na(Weight.mg)),
		meanWei = mean(Weight.mg, na.rm = TRUE),
		sdWei = sd(Weight.mg, na.rm = TRUE),
		CVWei= sdWei / meanWei,
		cvByNWei = (1+(1/(4*N))) * CVWei,
		logCVByNWei = log10(cvByNWei),
		meanLeg = mean(LegLen.mm, na.rm = TRUE),
		sdLeg = sd(LegLen.mm, na.rm = TRUE),
		CVLeg= sdLeg / meanLeg,
		cvByNLeg = (1+(1/(4*N))) * CVLeg,
		logcvByNLeg = log10(cvByNLeg),
		meanHung = mean(hunger, na.rm = TRUE),
		sdHung = sd(hunger, na.rm = TRUE),
		CVHung= sdHung/ meanHung,
		cvByNHung = (1+(1/(4*N))) * CVHung,
		logcvByNHung= log10(cvByNHung),
		meanHead =  mean(HeadLength.mm)



)

## Removing single nests
SpiNestAve <- subset(SpiNestAve, type == "multiple")
##removing NA's from SpiNestAve
SpiNestAve <- na.omit(SpiNestAve)

spidersMul <- subset(spiders, type == "multiple") #removing single females
spidersMul$NestID <- factor(spidersMul$NestID)


### data for single vs multiple nests
spidersSglMt <- subset(spiders, Instar == "Adult")


#removing single spiders that don't have juvs or eggs to make sure that the higher weight isn't because they are gravid
spidersSglMt <- subset(spidersSglMt , FemalesHaveEggsOrJuvs != "n")
Spis44 <- subset(spidersSglMt, km == "44.4")
Spis44 <- subset(Spis44, type == "single" | NestID == "44.4EX03")


## looking at small juv 4's to see if needs to be removed from analysis

table(spiderData$Instar)

juvs <- subset(spiderData, Instar == 'Juv4' | Instar =='juv3', select = c('NestID', 'Key', 'Instar', 'LegLen.mm' ))


##Leticia Export

LAExport <- subset(SpiNestAve, Instar == "Sub1", select = c("NestID", "Instar", "logCtFm", "meanHung", "cvByNHung" ))

write.table(LAExport, "RuthEcuador2013/NestSize/Graphs/CVHunger.csv", sep = ",")