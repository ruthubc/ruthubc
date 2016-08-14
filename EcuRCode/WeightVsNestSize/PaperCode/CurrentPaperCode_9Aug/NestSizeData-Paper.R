### Importing the data


spiders <- read.csv("RuthEcuador2013/NestSize/CombinedNestVsWeight.csv")

spiders <- subset(spiders, AdMaleSubBd == "")


#changing to simplier name
spiders$type <- spiders$Approx..Single.

spiders <- subset(spiders, select = c(NestID, type, FemalesHaveEggsOrJuvs, Instar,  Weight.mg, LegLen.mm, HeadLength.mm,  CountFemales ))

spiders$ID<-seq.int(nrow(spiders))

#removing eggs, parastised individuals and the outlier nest 44.3ex01 as the adults were particularly small
spiders <- subset(spiders, Instar != "FALSE" & NestID != "44.3ex01"  & Instar !="egg" & 
				Instar != "pj" & Instar != "PST" & Instar != "juv3")

##replacing ad males with sub bodies as sub males
spiders$Instar<-as.character(spiders$Instar) ; spiders$Instar[spiders$AdMaleSubBd == "y"] <- "SubMale"
spiders$Instar <-as.factor(spiders$Instar)


spiders$Instar <- factor(spiders$Instar, levels= c("Adult", "Sub2", 
				"Sub1", "Juv4", "AdMale", "SubMale"))


#log transforming
## Apparently R doesn't work well with small numbers so converting mg and mm to ug and um.
spiders$logWt <- log10(spiders$Weight.mg*1000)
spiders$logCtFm <- log10(spiders$CountFemales)
spiders$logLeg<- log10(spiders$LegLen.mm*1000)
spiders$logHead <- log10(spiders$HeadLength.mm*1000)




############ Inserting Spider Number #########


instarNum <- c(Juv4 = 4, Sub1 = 5, Sub2 = 6, Adult = 7, SubMale = 5, AdMale = 6)
spiders$InstarNumber <- instarNum[spiders$Instar]

instarSex<- c(Juv4 = "F", Sub1 = "F", Sub2 = "F", Adult = "F", SubMale = "M", AdMale = "M")
spiders$InstarSex <- instarSex[spiders$Instar]

spiders <- condition_residuals(spiders, "logLeg")


spidersMul <- subset(spiders, type == "multiple") #removing single females
spidersMul$NestID <- factor(spidersMul$NestID)

#### Creating subset tables of each instar
#
#spiMulAd <- subset(spidersMul, Instar == "Adult")
#spiMulS1 <- subset(spidersMul, Instar == "Sub1")
#spiMulS2 <- subset(spidersMul, Instar == "Sub2")
#spiMulJv <- subset(spidersMul, Instar == "Juv4")
#spiMulMaleSb <- subset(spidersMul, Instar == "SubMale")
#spiMulMaleAd <- subset(spidersMul, Instar == "AdMale")
