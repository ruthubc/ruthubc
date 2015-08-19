# TODO: Add comment
# 
# Author: Ruth
###############################################################################

########## DO THIS!!!!!!!!! #################################
#TODO Remove very small nests i.e. ones that could be pretty new dispersers!
#TODO divide vaiance by mean to standardize like CV
#TODO amount of variance by instar, eg are ads less variable than younger instars?

library(ggplot2)
library(lmerTest) # this puts pvalue in lmer
library(visreg)
library(multcomp)
library(gridExtra)

mytheme <-theme_bw(base_size=15)  + theme(plot.title = element_text(vjust=2), panel.margin= unit(0.75, "lines"), axis.title.y = element_text(vjust=0),
		plot.margin=unit(c(1,1,1.5,1.2),"cm"), panel.border = element_rect(fill = NA, colour = "grey", linetype=1, size = 1))

spiders$cond.sqrt <- sqrt((spiders$Weight.mg)/(spiders$LegLen.mm^3)) # very high correlation between weight and condition

spidersMul$cond.sqrt <- sqrt((spidersMul$Weight.mg)/(spidersMul$LegLen.mm^3)) # very high correlation between weight and condition




SpiAveMerge<- ddply(spiders, .(NestID, Instar), summarise, # need to discount trials where no feeding obs and eve
		N = length(!is.na(Weight.mg)),
		WeightMean = mean(Weight.mg, na.rm = TRUE),
		LogWeightMean = mean(logWeight, na.rm = TRUE),
		LegMean = mean(LegLen.mm, na.rm = TRUE),
		LogLegMean = mean(logLeg, na.rm = TRUE),
		cond.sqrtMean = mean(cond.sqrt, na.rm = TRUE)
		
		

)

#ggplot(data = spidersVar, aes(logWeight)) + geom_histogram() + facet_wrap(~Instar)



spidersVar <- merge(spidersMul, SpiAveMerge, by = c("NestID", "Instar") )

spidersVar <- subset(spidersVar, CountFemales > 60) # removing all nests below 60 ads, about 3 multiple female nests

spidersVar$WeightMean[spidersVar$N == 1] <- NA
spidersVar$LogWeightMean[spidersVar$N == 1] <- NA
spidersVar$LegDiffFromMean[spidersVar$N == 1] <- NA
spidersVar$LogLegDiffFromMean[spidersVar$N == 1] <- NA


spidersVar$LogWeightDiffFromMean <- abs(spidersVar$logWeight - spidersVar$LogWeightMean)

spidersVar$WeightDiffFromMean <- abs(spidersVar$Weight.mg - spidersVar$WeightMean)

spidersVar$LegDiffFromMean <- (abs(spidersVar$LegLen.mm - spidersVar$LegMean))/spidersVar$LegMean # dividing by the mean

spidersVar$LogLegDiffFromMean <- abs(spidersVar$logLeg - spidersVar$LogLegMean)

spidersVar$HungerDiffFromMean <- (abs(spidersVar$hunger - spidersVar$HungerMean))/spidersVar$HungerMean

spidersVar$LogHungerDiffFromMean <- abs(spidersVar$LogHungerMean - spidersVar$logHung)

spidersVar$sqrt.condDiffFromMean <- abs(spidersVar$cond.sqrtMean - spidersVar$cond.sqrt)



spidersVar$SqRtOfHungDiff <- (spidersVar$HungerDiffFromMean)^0.5

spidersVar$SqRtOfLegDiff <- (spidersVar$LegDiffFromMean)^0.5

spidersVar$SqRtOfCondDiff <- (spidersVar$sqrt.condDiffFromMean) ^0.5

## Histograms to check distribution. 
#I am going with the log one for the moment as it appears that the distributions are the most similar among instars

ggplot(spidersVar, aes(sqrt.condDiffFromMean)) + geom_histogram() + facet_wrap(~Instar)

ggplot(spidersVar, aes(SqRtOfCondDiff)) + geom_histogram() + facet_wrap(~Instar)


ggplot(data = spidersVar, aes(WeightDiffFromMean)) + geom_histogram() + facet_wrap(~Instar)

ggplot(data = spidersVar, aes(LegDiffFromMean^0.5)) + geom_histogram() + facet_wrap(~Instar)

ggplot(data = spidersVar, aes(LogLegDiffFromMean)) + geom_histogram() + facet_wrap(~Instar)

ggplot(data = spidersVar, aes(LogOfHungDiff)) + geom_histogram() + facet_wrap(~Instar)

ggplot(data = spidersVar, aes(LogHungerDiffFromMean)) + geom_histogram()# + facet_wrap(~Instar)

ggplot(data = spidersVar, aes(SqRtOfHungDiff)) + geom_histogram() + facet_wrap(~Instar)

ggplot(data = spidersVar, aes(SqRtOfLegDiff)) + geom_histogram() + facet_wrap(~Instar)

pdf("RuthEcuador2013/NestSize/Graphs/LegLengthVsNestSizeHungVar.pdf", height=8, width=11)


ggplot(data = spidersVar, aes(x = logCtFm , y = SqRtOfHungDiff)) + facet_wrap(~Instar) + geom_point(aes(colour = NestID), shape = 16) + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + mytheme + scale_colour_discrete(guide = FALSE)

dev.off()

ggplot(data = spidersVar, aes(x = logCtFm , y = SqRtOfLegDiff)) + facet_wrap(~Instar) + geom_point(aes(colour = NestID), shape = 16) + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + mytheme + scale_colour_discrete(guide = FALSE)

ggplot(data = spidersVar, aes(x = logCtFm, y = SqRtOfCondDiff))  + facet_wrap(~Instar) + geom_point(aes(colour = NestID), shape = 16) + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) + mytheme + scale_colour_discrete(guide = FALSE)



SpiDiffAve<- ddply(spidersVar, .(NestID, Instar, N, logCtFm), summarise, # need to discount trials where no feeding obs and eve
		NewN = length(!is.na(LogWeightDiffFromMean)),
		MeanLogWtDiff = mean(LogWeightDiffFromMean, na.rm = TRUE),
		SELogWtDiff = sd(LogWeightDiffFromMean, na.rm = TRUE)/ sqrt(NewN),
		MeanLogLegDiff = mean(LogLegDiffFromMean, na.rm = TRUE),
		SELogLegDiff = sd(LogLegDiffFromMean, na.rm = TRUE)/ sqrt(NewN)
)



check <- subset(spidersVar, NestID == "16.2EX01" & Instar == "AdMale")
## graphs diff from mean vs nest size
ggplot(data = SpiDiffAve, aes(x = logCtFm , y = MeanLogLegDiff )) + facet_wrap(~Instar) + geom_point() + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE)

ggplot(data = SpiDiffAve, aes(x = logCtFm , y = MeanLogWtDiff )) + facet_wrap(~Instar) + geom_point() + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE)


### Stats #########
#### Leg Length variance 

legDiffLm<- lmer(SqRtOfLegDiff ~ I(logCtFm^2) + logCtFm + Instar+ Instar:logCtFm + 
				I(logCtFm^2):Instar + (1|NestID), data = spidersVar, REML = FALSE)

modelPlot(legDiffLm)

anova(legDiffLm)

legDiffLm2 <- lmer(SqRtOfLegDiff ~ logCtFm + Instar+ Instar:logCtFm + 
				(1|NestID), data = spidersVar, REML = FALSE)

anova(legDiffLm2)

### Checking out the different instars individually

## Adult

legDiffAdLm<- lmer(SqRtOfLegDiff ~ I(logCtFm^2) + logCtFm + (1|NestID), data = subset(spidersVar, Instar == "Adult"), REML = FALSE)

modelPlot(legDiffAdLm)

anova(legDiffAdLm)

## Sub2

legDiffSub2Lm<- lmer(SqRtOfLegDiff ~ I(logCtFm^2) + logCtFm + (1|NestID), data = subset(spidersVar, Instar == "Sub2"), REML = FALSE)

modelPlot(legDiffSub2Lm)

anova(legDiffSub2Lm)

## Sub1

legDiffSub1Lm<- lmer(SqRtOfLegDiff ~ I(logCtFm^2) + logCtFm + (1|NestID), data = subset(spidersVar, Instar == "Sub1"), REML = FALSE)

modelPlot(legDiffSub1Lm)

anova(legDiffSub1Lm)

## Juv4

legDiffJuv4Lm<- lmer(SqRtOfLegDiff ~ I(logCtFm^2) + logCtFm + (1|NestID), data = subset(spidersVar, Instar == "Juv4"), REML = FALSE)

modelPlot(legDiffJuv4Lm)

anova(legDiffJuv4Lm)

## SubMale

legDiffSubMaleLm<- lmer(SqRtOfLegDiff ~ I(logCtFm^2) + logCtFm + (1|NestID), data = subset(spidersVar, Instar == "SubMale"), REML = FALSE)

anova(legDiffSubMaleLm)

## AdMale

legDiffAdMaleLm<- lmer(SqRtOfLegDiff ~ I(logCtFm^2) + logCtFm + (1|NestID), data = subset(spidersVar, Instar == "AdMale"), REML = FALSE)

anova(legDiffAdMaleLm)




############### Hunger variance ################################

noMales <- subset(spidersVar, Instar != "SubMale" & Instar != "AdMale")


CondDiffLm <- lmer(SqRtOfCondDiff ~ I(logCtFm^2) + logCtFm + Instar+ Instar:logCtFm + 
				I(logCtFm^2):Instar + (1|NestID), data = spidersVar, REML = FALSE)

# I removed the males and tested wihtout the square term, then there was a sig difference in variance, decreasing as colonies grow in size!

visreg(CondDiffLm, xvar = "logCtFm", by = "Instar") # This is really useful! Do more research of what this is
summary(CondDiffLm)

modelPlot(CondDiffLm)

anova(CondDiffLm)



## Cond Diff Sub1 ###

sub1Dat <- lmer(SqRtOfCondDiff  ~ I(logCtFm^2) + logCtFm +
				(1|NestID), data = subset(spidersVar, Instar == "Sub1"), REML = FALSE)


anova(sub1Dat)

## Cond Diff Ad ###

AdDat <- lmer(SqRtOfCondDiff  ~ I(logCtFm^2) + logCtFm +
						(1|NestID), data = subset(spidersVar, Instar == "Adult"), REML = FALSE)


anova(AdDat)

## Cond Diff Juv4 ###

Juv4Dat <- lmer(SqRtOfCondDiff  ~ I(logCtFm^2) + logCtFm +
				(1|NestID), data = subset(spidersVar, Instar == "Juv4"), REML = FALSE)


anova(Juv4Dat)


### CV of small vs. large nests

#Histogram of nest sizes to see if there is a break

ggplot(data = SpiNestAve, aes(x= CountFemales )) + geom_histogram()

# gonna start with splitting nest >2000 and < 2000


spidersVar$LgSm<- ifelse(spidersVar$CountFemales > 2000, "Large", ifelse(spidersVar$CountFemales < 300, "Small","Med"))  # updating large small nests
spidersVar$LgSm <- as.factor(spidersVar$LgSm)
spidersVar$LgSm <- factor(spidersVar$LgSm, levels = c("Small", "Med", "Large"))

pdf("RuthEcuador2013/NestSize/Graphs/WeightVsNestSizeCondVarSmLg.pdf", height=8, width=11)

ggplot(data = subset(spidersVar, LgSm != "Med"), aes(x=LgSm, y = SqRtOfCondDiff )) + geom_boxplot() + facet_wrap(~Instar) + mytheme +
		ylab("Square rood of Conder difference from mean") + xlab("Colony Size")

ggplot(data = subset(spidersVar, LgSm != "Med"), aes(x=LgSm, y = SqRtOfCondDiff )) + geom_boxplot() + mytheme +
		ylab("Square rood of Conder difference from mean") + xlab("Colony Size")


dev.off()

SmLgLmFull<- lmer(SqRtOfCondDiff  ~ LgSm + Instar + LgSm:Instar +
				(1|NestID), data = subset(spidersVar, LgSm != "Med"))#, family=gaussian)

modelPlot(SmLgLmFull)

anova(SmLgLmFull)

SmLgLmInt<- lmer(SqRtOfCondDiff  ~  Instar + LgSm +
				(1|NestID), data = subset(spidersVar, LgSm != "Med"))#, family=gaussian)

anova(SmLgLmFull, SmLgLmInt)

SmLgLmRed<- lmer(SqRtOfCondDiff  ~  Instar +
				(1|NestID), data = subset(spidersVar, LgSm != "Med"))


anova(SmLgLmInt, SmLgLmRed)
summary(SmLgLm)




## Testing differences for individual instars


SmLgLmAdFull<- glmer(SqRtOfCondDiff  ~ LgSm + 
				(1|NestID), data = subset(spidersVar, LgSm != "Med" & Instar == "Adult"), family=gaussian)

SmLgLmAdRed<- glmer(SqRtOfCondDiff  ~ 
				(1|NestID), data = subset(spidersVar, LgSm != "Med" & Instar == "Adult"), family=gaussian)

anova(SmLgLmFull, SmLgLmAdRed)



#### Testing difference in variance between instars

spidersVar$InstarLine<- factor(spidersVar$Instar, levels = c("Adult", "Sub2", "AdMale", "Sub1", "SubMale", "Juv4"))

## Conder
noMales <- subset(spidersVar, Instar != "SubMale" & Instar != "AdMale")

png("RuthEcuador2013/NestSize/Graphs/ConderInstarVariance.png")

ggplot(data = noMales, aes(x = InstarLine, y = SqRtOfCondDiff)) + geom_boxplot() + mytheme

dev.off()

LmInstar <- lmer(SqRtOfCondDiff ~ InstarLine + (1|NestID), data = noMales, REML = FALSE)
anova(LmInstar)
summary(LmInstar)

visreg(LmInstar) 

comp.LmInstar <- glht(LmInstar, linfct=mcp(InstarLine ="Tukey"))
summary(comp.LmInstar)

## Leg
ggplot(data = noMales, aes(x = InstarLine, y = SqRtOfLegDiff)) + geom_boxplot() + mytheme

LmInstarLeg <- lmer(SqRtOfCondDiff ~ InstarLine + (1|NestID), data = spidersVar, REML = FALSE)

anova(LmInstarLeg)

comp.LmInstarLeg <- glht(LmInstarLeg, linfct=mcp(InstarLine ="Tukey"))

summary(comp.LmInstarLeg)


## Just looking at the weight vs nests size graph with the start nests removed

ggplot(spidersVar, aes(x=logCtFm, y = logWeight )) + geom_point() + 
		geom_smooth(method = "lm", formula =y ~  poly(x, 2, raw = TRUE), se = TRUE) +
		facet_wrap(~ Instar, scales = "free_y")
