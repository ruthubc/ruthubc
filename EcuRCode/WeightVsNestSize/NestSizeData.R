
# Author: Ruth
###############################################################################


spiderData <- read.csv("RuthEcuador2013/NestSize/CombinedNestVsWeight.csv")

#removing eggs, parastised individuals and the outlier nest 44.3ex01 as the adults were particularly small
spiders <- subset(spiderData, Instar != "FALSE" & NestID != "44.3ex01"  & Instar !="egg" & 
				Instar != "pj" & Instar != "PST" & Instar != "juv3")

spiders <- subset(spiders, Instar != "Juv4" | (Instar == "Juv4" & LegLen.mm > 0.82))

#Calculating spider hunger
spiders$hunger <- spiders$HeadLength.mm/spiders$Weight.mg

#removing empty levels
spiders$Instar <- factor(spiders$Instar, levels= c("Adult", "Sub2", 
				"Sub1", "Juv4", "AdMale", "SubMale"))

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
		logCVByNWei = log(cvByNWei),
		meanLgLeg = mean(logLeg, na.rm = TRUE),
		sdLgLeg = sd(logLeg, na.rm = TRUE),
		CVLgLeg= sdLgLeg / meanLgLeg,
		cvByNLgLeg = (1+(1/(4*N))) * CVLgLeg,
		meanLeg = mean(LegLen.mm, na.rm = TRUE),
		sdLeg = sd(LegLen.mm, na.rm = TRUE),
		CVLeg= sdLeg / meanLeg,
		cvByNLeg = (1+(1/(4*N))) * CVLeg,
		logcvByNLeg = log(cvByNLeg)


)



## looking at small juv 4's to see if needs to be removed from analysis

table(spiderData$Instar)

juvs <- subset(spiderData, Instar == 'Juv4' | Instar =='juv3', select = c('NestID', 'Key', 'Instar', 'LegLen.mm' ))