# TODO: Add comment
# 
# Author: user
###############################################################################
# Cubed condition very insignificant - stick with normal condition from now on.

SpiNestAveMul<- ddply(spidersMul, .(NestID, type, Instar, logCtFm, CountFemales), summarise,
		N = length(!is.na(ID)),
		meanLeg = mean(LegLen.mm, na.rm = TRUE),
		sdLeg = sd(LegLen.mm, na.rm = TRUE),
		CVLeg= sdLeg / meanLeg,
		logCVLeg = log10(CVLeg),
		# cvByNLeg = (1+(1/(4*N))) * CVLeg, not sure where this came from
		#logcvByNLeg = log10(cvByNLeg),
		meanCond = mean(condition, na.rm = TRUE),
		sdCond = sd(condition, na.rm = TRUE),
		CVCond = sdCond / meanCond,
		sqCVCond = sqrt(CVCond)
)

## Histogram to check fit, log cond doesn't really help normality
# prob square root transformation of cv condition is best 
ggplot(SpiNestAveMul, aes(CVCond)) + geom_histogram()
ggplot(SpiNestAveMul, aes(logCVLeg)) + geom_histogram() # log of leg best transformation

### Test of CV of condition original
condDiffLm <- lmer(sqCVCond~  logCtFm + I(logCtFm ^2)+ Instar + Instar:logCtFm + I(logCtFm ^2):Instar +  
				(1|NestID), data = subset(SpiNestAveMul, N > 0), REML = FALSE)

anova(condDiffLm)

visreg(condDiffLm, xvar = "logCtFm", by = "Instar")
visreg(condDiffLm)

ggplot(data = SpiNestAveMul, aes(x = Instar, y = sqCVCond)) + geom_boxplot()

### Test of CV of leg
legDiffLm <- lmer(logCVLeg~  logCtFm + I(logCtFm ^2)+ Instar + Instar:logCtFm + I(logCtFm ^2):Instar +  
				(1|NestID), data = subset(SpiNestAveMul, N > 0), REML = FALSE)

anova(legDiffLm)

visreg(legDiffLm, xvar = "logCtFm", by = "Instar")


# Checking out large vs small nests

## Nothing to see here!

SpiNestAveMul$LgSm<- ifelse(SpiNestAveMul$CountFemales > 2000, "Large", ifelse(SpiNestAveMul$CountFemales < 400, "Small","Med"))  # updating large small nests
SpiNestAveMul$LgSm <- as.factor(SpiNestAveMul$LgSm)
SpiNestAveMul$LgSm <- factor(SpiNestAveMul$LgSm, levels = c("Small", "Med", "Large"))

SmLgLmFull<- lmer(sqCVCond ~ LgSm + Instar + LgSm:Instar +
				(1|NestID), data = SpiNestAveMul)#, family=gaussian)

anova(SmLgLmFull)
visreg(SmLgLmFull, xvar = "LgSm", by = "Instar")



############## Individual testing cv by instar

# none individually significant prob because too few observations

### Adult
condDiffLmAd <- lm(sqCVCond ~  I(logCtFm ^2) + logCtFm , data = subset(SpiNestAveMul, Instar == "Adult"))

anova(condDiffLmAd)


### Sub2
condDiffLmS2 <- lm(sqCVCond ~  I(logCtFm ^2) + logCtFm , data = subset(SpiNestAveMul, Instar == "Sub2"))

anova(condDiffLmS2)

### Sub1
condDiffLmS1 <- lm(sqCVCond ~  I(logCtFm ^2) + logCtFm , data = subset(SpiNestAveMul, Instar == "Sub1"))

anova(condDiffLmS1)

### Juv4
condDiffLmJv <- lm(sqCVCond ~  I(logCtFm ^2) + logCtFm , data = subset(SpiNestAveMul, Instar == "Juv4"))

anova(condDiffLmJv)

### AdMale
condDiffLmAdMl <- lm(sqCVCond ~  I(logCtFm ^2) + logCtFm , data = subset(SpiNestAveMul, Instar == "AdMale"))

anova(condDiffLmAdMl)

### SubMale
condDiffLmSbMl <- lm(sqCVCond ~  I(logCtFm ^2) + logCtFm , data = subset(SpiNestAveMul, Instar == "SubMale"))

anova(condDiffLmSbMl)










