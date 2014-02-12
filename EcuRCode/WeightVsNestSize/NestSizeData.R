
# Author: Ruth
###############################################################################


spiderData <- read.csv("RuthEcuador2013/NestSize/CombinedNestVsWeight.csv")

#removing eggs, parastised individuals and the outlier nest 44.3ex01 as the adults were particularly small
spiders <- subset(spiderData, Instar != "FALSE" & NestID != "44.3ex01"  & Instar !="egg" & 
				Instar != "pj" & Instar != "PST" & Instar != "juv3")

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


SpidersNestAve<- ddply(spiders, .(NestID, type, Instar, logCtFm), summarise, # need to discount trials where no feeding obs and eve
		N = length(!is.na(Weight.mg)),
		meanLWei = mean(logWeight, na.rm = TRUE),
		sdLWei = sd(logWeight, na.rm = TRUE),
		CVLWei= sdLWei / meanLWei,
		cvByNLWei = (1+(1/(4*N))) * CVLWei,
		meanLeg = mean(logWeight, na.rm = TRUE),
		sdLWei = sd(logWeight, na.rm = TRUE),
		CVLWei= sdLWei / meanLWei,
		cvByNLWei = (1+(1/(4*N))) * CVLWei

)


SSummariseWeight <- ddply(spiders, .(NestID, CountFemales, Approx..Single., logCtFm, Instar), summarise,
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