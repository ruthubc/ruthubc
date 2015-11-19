# TODO: Add comment
# 
# Author: user
###############################################################################
# Cubed condition very insignificant - stick with normal condition from now on.

library(multcomp)

spidersMul$LogLegMul10 <- log(spidersMul$LegLen.mm*10)


SpiNestCV<- ddply(spidersMul, .(NestID, type, Instar, logCtFm, CountFemales), summarise,
		N = length(!is.na(ID)),
		meanLeg = mean(LogLegMul10, na.rm = TRUE),
		sdLeg = sd(LogLegMul10, na.rm = TRUE),
		CVLeg= (1 + (1/N)) * sdLeg / meanLeg,
		meanCond = mean(logcond, na.rm = TRUE),
		sdCond = sd(logcond, na.rm = TRUE),
		CVCond = (1 + (1/N)) * (sdCond / meanCond)
)

		# cvByNLeg = (1+(1/(4*N))) * CVLeg, not sure where this came from
		#logcvByNLeg = log10(cvByNLeg),
## Histogram to check fit, log cond doesn't really help normality
# prob square root transformation of cv condition is best 
ggplot(SpiNestCV, aes((CVCond^0.5))) + geom_histogram()
ggplot(SpiNestCV, aes(CVLeg^0.5)) + geom_histogram()

SpiNestCV$CVCondSqrt <- SpiNestCV$CVCond^0.5


### Test of CV of condition original
condDiffLm <- lmer(CVCond ^ 0.5 ~  logCtFm + I(logCtFm ^2)+ Instar + Instar:logCtFm + I(logCtFm ^2):Instar +  
				(1|NestID), data = subset(SpiNestCV, N > 3), REML = FALSE)

anova(condDiffLm)

visreg(condDiffLm, xvar = "logCtFm", by = "Instar")
visreg(condDiffLm)

#### Non_sq Cond Test
condDiffLm <- lmer(CVCond ^ 0.5 ~  logCtFm + Instar + Instar:logCtFm  +  
				(1|NestID), data = subset(SpiNestCV, N > 3), REML = FALSE)

anova(condDiffLm)

visreg(condDiffLm, xvar = "logCtFm", by = "Instar")
visreg(condDiffLm)

condDiffLmRed <- lmer((CVCond ^ 0.5) ~   Instar  +  
				(1|NestID), data = subset(SpiNestCV, N > 3), REML = FALSE)


anova(condDiffLmRed)
summary(condDiffLmRed)

anova(condDiffLmRed, condDiffLm)

### Test of CV of leg
legDiffLm <- lmer((CVLeg^0.5)~  logCtFm + I(logCtFm ^2)+ Instar + Instar:logCtFm + I(logCtFm ^2):Instar +  
				(1|NestID), data = subset(SpiNestCV, N > 3), REML = FALSE)

anova(legDiffLm)

visreg(legDiffLm, xvar = "logCtFm", by = "Instar")

### No sqr term

legDiffLmNoSq <- lmer((CVLeg^0.5) ~  logCtFm + Instar + Instar:logCtFm + 
				(1|NestID), data = subset(SpiNestCV, N > 3), REML = FALSE)

anova(legDiffLmNoSq )

visreg(legDiffLmNoSq , xvar = "logCtFm", by = "Instar")



# Checking out large vs small nests

## Nothing to see here!

SpiNestCV$LgSm<- ifelse(SpiNestCV$CountFemales > 2000, "Large", ifelse(SpiNestCV$CountFemales < 400, "Small","Med"))  # updating large small nests
SpiNestCV$LgSm <- as.factor(SpiNestCV$LgSm)
SpiNestCV$LgSm <- factor(SpiNestCV$LgSm, levels = c("Small", "Med", "Large"))

SmLgLmFull<- lmer(sqCVCond ~ LgSm + Instar + LgSm:Instar +
				(1|NestID), data = SpiNestCV)#, family=gaussian)

anova(SmLgLmFull)
visreg(SmLgLmFull, xvar = "LgSm", by = "Instar")



############## Individual testing cv by instar

# none individually significant prob because too few observations

### Adult
condDiffLmAd <- lm(sqCVCond ~  I(logCtFm ^2) + logCtFm , data = subset(SpiNestCV, Instar == "Adult"))

anova(condDiffLmAd)


### Sub2
condDiffLmS2 <- lm(sqCVCond ~  I(logCtFm ^2) + logCtFm , data = subset(SpiNestCV, Instar == "Sub2"))

anova(condDiffLmS2)

### Sub1
condDiffLmS1 <- lm(sqCVCond ~  I(logCtFm ^2) + logCtFm , data = subset(SpiNestCV, Instar == "Sub1"))

anova(condDiffLmS1)

### Juv4
condDiffLmJv <- lm(sqCVCond ~  I(logCtFm ^2) + logCtFm , data = subset(SpiNestCV, Instar == "Juv4"))

anova(condDiffLmJv)

### AdMale
condDiffLmAdMl <- lm(sqCVCond ~  I(logCtFm ^2) + logCtFm , data = subset(SpiNestCV, Instar == "AdMale"))

anova(condDiffLmAdMl)

### SubMale
condDiffLmSbMl <- lm(sqCVCond ~  I(logCtFm ^2) + logCtFm , data = subset(SpiNestCV, Instar == "SubMale"))

anova(condDiffLmSbMl)




############### Testing diff between instars

noMales <- subset(SpiNestCV, Instar != "SubMale" & Instar != "AdMale")

pdf("RuthEcuador2013/NestSize/Graphs/ConderInstarVariance.pdf")

#ggplot(data = spidersVar, aes(x = InstarLine, y = CVCond)) + geom_boxplot() + mytheme

ggplot(data = SpiNestCV, aes(x = Instar, y = CVLeg^0.5)) + geom_boxplot() + mytheme

ggplot(data = SpiNestCV, aes(x = Instar, y = CVCond ^ 0.5)) + geom_boxplot() + mytheme

#ggplot(data = noMales, aes(x = InstarLine, y = SqRtOfCondDiff)) + geom_boxplot() + mytheme

dev.off()

LmInstar <- lmer((CVCond ^ 0.5) ~ Instar + (1|NestID), data = SpiNestCV, REML = FALSE)
anova(LmInstar)
summary(LmInstar)

visreg(LmInstar) 

comp.LmInstar <- glht(LmInstar, linfct=mcp(Instar ="Tukey"))
summary(comp.LmInstar)

anova(condDiffLm, condDiffLmRed)


ggplot(data = SpiNestCV, aes(x = Instar, y = sqCVCond)) + geom_boxplot()






