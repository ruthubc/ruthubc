# TODO: Add comment
# 
# Author: Ruth
###############################################################################

require(reshape2)
library (ggplot2)
library (grid)
library (lme4)
library(lmerTest)
library(visreg)
mytheme <-theme_bw(base_size=15)  + theme(plot.title = element_text(vjust=2), panel.margin= unit(0.75, "lines"), axis.title.y = element_text(vjust=0),
		plot.margin=unit(c(1,1,1.5,1.2),"cm"), panel.border = element_rect(fill = NA, colour = "grey", linetype=1, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

spiders$InstarOrdered <- factor(spiders$Instar, levels= c("Adult", "Sub2", 
				"Sub1", "Juv4", "AdMale", "SubMale"))

spiders<- subset(spiders, !is.na(logWt) )

#spiders$lnLeg <- log(spiders$LegLen.mm)
#spiders$lnWt <- log(spiders$Weight.mg) # natural log rather than log10 but doesn't matter




#calculating the residual index
ggplot(spiders, aes(logLeg, logWt)) + geom_point() + geom_smooth(method=lm) + facet_wrap(~Instar,  scales = "free")
#ggplot(spiders, aes(lnLeg, lnWt)) + geom_point() + geom_smooth(method=lm, fullrange = TRUE) # log or nat log doesn't make a difference

model <- lm(logWt ~ logLeg, spiders ) # doesn't matter which way round this is
#model <- lm(logWt ~ logLeg + Instar + logLeg:Instar, spiders ) # whichever one i use doesn't make a difference

visreg(model, xvar = "logLeg", by = "Instar" )


summary(model)
spiders$condResiduals <- resid(model)  # putting the residuales into the dable
spidersMult <- subset(spiders, type== 'multiple') # remving single nests

ggplot(spiders, aes(condResiduals, fill = Instar)) + geom_histogram() # normal

# plotting res condition against leg length
ggplot(spiders, aes(x = condResiduals, y = logLeg, colour = Instar)) + geom_point()

# plotting res condition against weight
ggplot(spiders, aes(x = condResiduals, y = logWt, colour = Instar)) + geom_point() + geom_smooth(method=lm, fullrange = TRUE)



################ graphs  ############

# condition vs nest size
ggplot(spidersMult, aes(x = logCtFm, y = condResiduals)) + geom_point()+ 
		geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE) +
		facet_wrap(~Instar, scales = "free_y") + xlab("Log Nest Size (num ad females)") + ylab("Condition Residuals") + mytheme

ggplot(spidersMult, aes(x = logCtFm, y = condResiduals, colour = Instar)) + geom_point()+ 
		geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE) + 
		xlab("Log Nest Size (num ad females)") + ylab("Condition Residuals") + mytheme

## Stats condition vs nest size
condResLmFull <- lmer(condResiduals ~  logCtFm + Instar + logCtFm:Instar  + 
				(1|NestID), data = spidersMult, REML = FALSE)

visreg(condResLmFull, xvar = "logCtFm", by = "Instar")

anova(condResLmFull) 

condResLmRed <- lmer(condResiduals ~   Instar + 
				(1|NestID), data = spidersMult, REML = FALSE)


anova(condResLmFull, condResLmRed) # comparing full model to reduced model

levels(spiders$Instar)

condResByInstar <- lmer(condResiduals ~  logCtFm  + 
				(1|NestID), data = subset(spidersMult, Instar == 'Juv4'), REML = FALSE)

summary(condResByInstar)

#adult significant
# Sub2 not significant
# sub1 not signifcant
# juv 4 not significant
#submale not significant
#ad male not significant


###############Back to graphs ##############
#condition vs instar Males have significantly reduced condition compared to all other instars
ggplot(spiders, aes(x = InstarOrdered, y = condResiduals)) + geom_boxplot() + mytheme


# cond residuals vs multiple and single nests
ggplot(subset(spiders, Instar == 'Adult'), aes(x = type, y = condResiduals)) + geom_boxplot() + mytheme


## variances for residual conditions

CVCondRes<- ddply(spidersMult, .(NestID, type, Instar, InstarOrdered, InstarNumber, logCtFm, CountFemales), summarise,
		N = length(!is.na(ID)),
		meanCondRes = abs(mean(condResiduals, na.rm = TRUE)),
		sdCondRes = sd(condResiduals, na.rm = TRUE),
		CVCondRes = (1 + (1/(4*N))) * (sdCondRes / meanCondRes),
		logCVCond = log(CVCondRes)
)

#histogram to check normality
ggplot(CVCondRes, aes(log(CVCondRes), fill = Instar)) + geom_histogram() 

# graphs of cond Variance, not very exciting!
ggplot(CVCondRes, aes(x = logCtFm, y = log(CVCondRes))) + geom_point()+ geom_smooth(method = "lm", formula =y ~  poly(x, 1, raw = TRUE), se = TRUE) +
		facet_wrap(~Instar, scales = "free_y") + xlab("Log Nest Size (num ad females)") + ylab("Condition Residuals Variance") + mytheme

## Stats
condVarLmFull <- lmer(logCVCond ~  logCtFm + Instar + logCtFm:Instar  + N + 
				(1|NestID), data = CVCondRes, REML = FALSE)

summary(condVarLmFull)

visreg(condVarLmFull, xvar = "logCtFm", by = "Instar" )


condVarLmAd <- lm(logCVCond ~  logCtFm + N , data = subset(CVCondRes, Instar == 'Adult'))

summary(condVarLmAd)


#condition variance by instar
ggplot(CVCondRes, aes(x = InstarOrdered, y = log(CVCondRes))) + geom_boxplot() + mytheme #  might show something
ggplot(CVCondRes, aes(x = InstarNumber, y = log(CVCondRes))) + geom_point() + geom_smooth(method = "lm") + mytheme 

CVNoMales <- subset(CVCondRes, !is.na(InstarNumber) & N > 1)

InstarCondFull <- lmer(logCVCond ~   InstarNumber  + N  + 
				(1|NestID), CVNoMales, REML = FALSE)

anova(InstarCondFull)
visreg(InstarCondFull, xvar = "InstarNumber" )

InstarCondRed <- lmer(logCVCond ~  N + (1|NestID),
				CVNoMales, REML = FALSE)

anova(InstarCondFull, InstarCondRed)



# Sample size by instar
ggplot(subset(CVCondRes, "N" >1), aes(InstarNumber, N)) + geom_point() + geom_smooth(method = "lm") # random factors can only be catogorical


# testing if the variation changes with sample size
ggplot(CVCondRes, aes(x = N, y = log(CVCondRes))) + geom_point() + geom_smooth(method = "lm", formula = y~ poly(x, 1)) # it does

CVSmpSize <- lmer(logCVCond ~  N + Instar + N:Instar + 
				(1|NestID), data = CVCondRes, REML = FALSE)

anova(CVSmpSize)

CVCondResFull <- lmer(logCVCond ~  logCtFm + Instar + logCtFm:Instar  + N + 
				(1|NestID), data = CVCondRes, REML = FALSE)

anova(CVCondResFull)

CVCondResRed <- lmer(logCVCond ~  Instar  + N + 
				(1|NestID), data = CVCondRes, REML = FALSE)

summary(CVCondResRed)
anova(CVCondResRed)
visreg(CVCondResRed, xvar = "Instar")

anova(CVCondResFull, CVCondResRed)


## Difference between instars

CondInsrCols<- dcast(subset(CVCondRes, select = c(NestID, Instar, meanCondRes, CountFemales, logCtFm)), 
		NestID +  logCtFm + CountFemales ~ Instar, value.var= "meanCondRes",  drop = T) #transpose data

CondInsrCols$AdSub2Diff <- CondInsrCols$Adult- CondInsrCols$Sub2
CondInsrCols$Sub2Sub1Diff <- CondInsrCols$Sub2- CondInsrCols$Sub1
CondInsrCols$Sub1Juv4Diff <- CondInsrCols$Sub1- CondInsrCols$Juv4



ggplot(CondInsrCols, aes(AdSub2Diff)) + geom_histogram()

ggplot(data = (CondInsrCols), aes(x = logCtFm, y = AdSub2Diff)) + geom_point() +
		stat_smooth(method="lm", se=TRUE, formula = y~ poly(x, 1)) + xlab("Log Nest Size") +
		ylab("Difference in mean condition (Adult - Sub2)") + mytheme

CondDiff <- melt(subset(CondInsrCols, select = c(NestID, CountFemales, logCtFm, AdSub2Diff, Sub2Sub1Diff, Sub1Juv4Diff)), 
				id.vars = c('NestID', 'CountFemales' , 'logCtFm'))
		
CondDiff <- na.omit(CondDiff)

CondDiff <- subset(CondDiff, CountFemales > 5)

ggplot(data = CondDiff, aes(x = logCtFm, y = value)) + geom_point() +
		stat_smooth(method="lm", se=TRUE, formula = y~ poly(x, 1)) + xlab("Log Nest Size") +
		ylab("Difference in mean condition") + facet_wrap(~variable) + mytheme

CondDiffLm <- lmer(value ~  logCtFm  + variable + logCtFm:variable + 
				(1|NestID), data = CondDiff, REML = FALSE)

anova(CondDiffLm)

visreg(CondDiffLm,  xvar = "logCtFm", by = "variable")


############ Overall variances for nests NOT by instar

CVCondResAll<- ddply(spidersMult, .(NestID, type, logCtFm, CountFemales), summarise,
		N = length(!is.na(ID)),
		meanCondRes = abs(mean(condResiduals, na.rm = TRUE)),
		sdCondRes = sd(condResiduals, na.rm = TRUE),
		CVCondRes = (1 + (1/(4*N))) * (sdCondRes / meanCondRes),
		logCVCond = log(CVCondRes)
)


ggplot(CVCondResAll, aes(x = N, y = logCVCond)) + geom_point() + geom_smooth(method = "lm", formula = y~ poly(x, 1))


