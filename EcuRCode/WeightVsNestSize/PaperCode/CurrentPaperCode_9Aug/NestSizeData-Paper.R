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

condVar <- calRelVariance(spiders, "condResiduals")

legVar <- calRelVariance(spiders, "logLeg")



spidersMul <- subset(spiders, type == "multiple") #removing single females
spidersMul$NestID <- factor(spidersMul$NestID)

#### Propagules against original nest

nestsSigOnly<- subset(spiders, type != "multiple" & Instar == "Adult")

OrgNests <- subset(spidersMul, Instar == "Adult")
OrgNests <- subset(OrgNests, NestID == "44.4EX03" | NestID == "28.8EX15")
OrgNests$OrigNest <- OrgNests$NestID

nestsSigOnly$OrigNest <- ifelse(grepl("^4",as.character(nestsSigOnly$NestID)) == TRUE, "44.4EX03", "28.8EX15")

nestsToTestSig <- rbind(nestsSigOnly, OrgNests)

nestsSigOnlyCond<- subset(nestsSigOnly, nestsSigOnly$FemalesHaveEggsOrJuvs == "y")

nestsToTestSigCond <- rbind(nestsSigOnlyCond, OrgNests)



############## Propagule survival data

rops <- read.csv("PropaguleData/dispersal_propagule_data.csv")

props$dateFormatted <- as.Date(props$date, format = "%d/%m/%Y")

props$propID <- as.factor(paste(props$nestID, props$Propagule, sep = "_"))

props <- ddply(props, "propID", transform, dateFirstRec = min(dateFormatted)) # calculating max and min


props$DiffDays <- difftime(props$dateFormatted, props$dateFirstRec, units = 'days')

props$DiffDays <- as.numeric(props$DiffDays)
props$Dead_bin<- ifelse(props$AliveOrDeadOrEnd == "Dead", 1, 0)


msurv <- with(props, Surv(DiffDays, Dead_bin ==1, type = "right"))
survivalMean <- as.data.frame(mean(msurv[,1])) # don't use mean without the [,1] by itself, wrong!


prop.survfit <- survfit(Surv(DiffDays, AliveOrDeadOrEnd =="Dead", type = "right") ~ 1, data = props)

