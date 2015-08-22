# TODO: Add comment
# 
# Author: Ruth
###############################################################################

library (lme4)
library(lmerTest)
library(visreg)

#  LEG LENGTH vs nest size
legLmFull <- lmer(logLeg ~  I(logCtFm^2) + logCtFm + Instar + logCtFm:Instar + I(logCtFm^2):Instar + 
				(1|NestID), data = spidersMul, REML = FALSE)

visreg(legLmFull, xvar = "logCtFm", by = "Instar")

anova(legLmFull) # neither square or non squared term was significant

legLmNoSq <- lmer(logLeg ~  logCtFm + Instar + logCtFm:Instar + 
				(1|NestID), data = spidersMul, REML = FALSE)

visreg(legLmNoSq, xvar = "logCtFm", by = "Instar")

anova(legLmNoSq) # count female is very significant 

legLmRed <- lmer(logLeg ~ Instar + (1|NestID), data = spidersMul, REML = FALSE)

anova(legLmFull, legLmRed) # comparing full model to reduced model: very significant

#################################################################
# org condition vs nest size

condLmFull <- lmer(conditionSq ~  I(logCtFm^2) + logCtFm + Instar + logCtFm:Instar + I(logCtFm^2):Instar + 
				(1|NestID), data = spidersMul, REML = FALSE)

anova(condLmFull) # neither square or non squared term was significant


visreg(condLmFull, xvar = "logCtFm", by = "Instar")

condLmNoSq <- lmer(conditionSq ~  logCtFm + Instar + logCtFm:Instar + 
				(1|NestID), data = spidersMul, REML = FALSE)

visreg(condLmNoSq, xvar = "logCtFm", by = "Instar")


anova(condLmNoSq) # count female not significant

condLmRed <- lmer(conditionSq ~ Instar + (1|NestID), data = spidersMul, REML = FALSE)

anova(condLmFull, condLmRed) # comparing full model to reduced model: quite significant but also removes the interaction

anova(condLmNoSq, condLmRed)


###############################################################
# cubed condition vs nest size

condCbLmFull <- lmer(condCbSq ~  I(logCtFm^2) + logCtFm + Instar + logCtFm:Instar + I(logCtFm^2):Instar + 
				(1|NestID), data = spidersMul, REML = FALSE)

anova(condCbLmFull) # neither square or non squared term was significant

visreg(condCbLmFull, xvar = "logCtFm", by = "Instar")

condCbLmNoSq <- lmer(condCbSq ~  logCtFm + Instar + logCtFm:Instar + 
				(1|NestID), data = spidersMul, REML = FALSE)

anova(condCbLmNoSq) # count female not significant

visreg(condCbLmNoSq, xvar = "logCtFm", by = "Instar")

condCbLmRed <- lmer(condCbSq ~ Instar + (1|NestID), data = spidersMul, REML = FALSE)

anova(condCbLmFull, condCbLmRed) # comparing full model to reduced model: quite significant but also removes the interaction

anova(condCbLmNoSq, condCbLmRed)


#########################################################################
# LEG LENGTH by instar

## Adult

legAdLm <- lmer(logLeg ~ I(logCtFm^2) + logCtFm + (1|NestID), data = spiMulAd, REML = FALSE)  ## not going to bother with the square term
anova(legAdLm)

legAdLmNoSq <- lmer(logLeg ~ logCtFm + (1|NestID), data = spiMulAd, REML = FALSE)
anova(legAdLmNoSq)

legAdLmRed <- lmer(logLeg ~ (1|NestID), data = spiMulAd, REML = FALSE)

anova(legAdLmNoSq, legAdLmRed)

## Sub2

legS2LmNoSq <- lmer(logLeg ~ logCtFm + (1|NestID), data = spiMulS2, REML = FALSE)
summary(legS2LmNoSq)
anova(legS2LmNoSq)

legS2LmRed <- lmer(logLeg ~ (1|NestID), data = spiMulS2, REML = FALSE)

anova(legS2LmNoSq, legS2LmRed )

## Sub1

legS1LmNoSq <- lmer(logLeg ~ logCtFm + (1|NestID), data = spiMulS1, REML = FALSE)
summary(legS1LmNoSq)
anova(legS1LmNoSq)

legS1LmRed <- lmer(logLeg ~ (1|NestID), data = spiMulS1, REML = FALSE)

anova(legS1LmNoSq, legS1LmRed )

## Juv

legJvLmNoSq <- lmer(logLeg ~ logCtFm + (1|NestID), data = spiMulJv, REML = FALSE)
summary(legJvLmNoSq)
anova(legJvLmNoSq)

legJvLmRed <- lmer(logLeg ~ (1|NestID), data = spiMulJv, REML = FALSE)

anova(legJvLmNoSq, legJvLmRed )

## Adult male

legMaleAdLmNoSq <- lmer(logLeg ~ logCtFm + (1|NestID), data = spiMulMaleAd, REML = FALSE)
summary(legMaleAdLmNoSq)
anova(legMaleAdLmNoSq)
visreg(legMaleAdLmNoSq)

legMaleAdLmRed <- lmer(logLeg ~ (1|NestID), data = spiMulMaleAd, REML = FALSE)

anova(legMaleAdLmNoSq, legMaleAdLmRed )


## SubMale

legMaleSbLmNoSq <- lmer(logLeg ~ logCtFm + (1|NestID), data = spiMulMaleSb, REML = FALSE)
summary(legMaleSbLmNoSq)
anova(legMaleSbLmNoSq)

legMaleSbLmRed <- lmer(logLeg ~ (1|NestID), data = spiMulMaleSb, REML = FALSE)

anova(legMaleSbLmNoSq, legMaleSbLmRed )


# Cubed condition by individual instar


## Adult


cdCbAdLmNoSq <- lmer(condCbSq ~ logCtFm + (1|NestID), data = spiMulAd, REML = FALSE)
anova(cdCbAdLmNoSq)

cdCbAdLmRed <- lmer(condCbSq ~ (1|NestID), data = spiMulAd, REML = FALSE)

anova(cdCbAdLmNoSq, cdCbAdLmRed)

## Sub2

cdCbS2LmNoSq <- lmer(condCbSq ~ logCtFm + (1|NestID), data = spiMulS2, REML = FALSE)
summary(cdCbS2LmNoSq)
anova(cdCbS2LmNoSq)

cdCbS2LmRed <- lmer(condCbSq ~ (1|NestID), data = spiMulS2, REML = FALSE)

anova(cdCbS2LmNoSq, cdCbS2LmRed )

## Sub1

cdCbS1LmNoSq <- lmer(condCbSq ~ logCtFm + (1|NestID), data = spiMulS1, REML = FALSE)
summary(cdCbS1LmNoSq)
anova(cdCbS1LmNoSq)

cdCbS1LmRed <- lmer(condCbSq ~ (1|NestID), data = spiMulS1, REML = FALSE)

anova(cdCbS1LmNoSq, cdCbS1LmRed )

## Juv

cdCbJvLmNoSq <- lmer(condCbSq ~ logCtFm + (1|NestID), data = spiMulJv, REML = FALSE)
summary(cdCbJvLmNoSq)
anova(cdCbJvLmNoSq)

cdCbJvLmRed <- lmer(condCbSq ~ (1|NestID), data = spiMulJv, REML = FALSE)

anova(cdCbJvLmNoSq, cdCbJvLmRed )


## Ad male

cdCbMaleAdLmNoSq <- lmer(condCbSq ~ logCtFm + (1|NestID), data = spiMulMaleAd, REML = FALSE)
summary(cdCbMaleAdLmNoSq)
anova(cdCbMaleAdLmNoSq)

cdCbMaleAdLmRed <- lmer(condCbSq ~ (1|NestID), data = spiMulMaleAd, REML = FALSE)

anova(cdCbMaleAdLmNoSq, cdCbMaleAdLmRed )



## Sub male

cdCbMaleSbLmNoSq <- lmer(condCbSq ~ logCtFm + (1|NestID), data = spiMulMaleSb, REML = FALSE)
summary(cdCbMaleSbLmNoSq)
anova(cdCbMaleSbLmNoSq)

cdCbMaleSbLmRed <- lmer(condCbSq ~ (1|NestID), data = spiMulMaleSb, REML = FALSE)

anova(cdCbMaleSbLmNoSq, cdCbMaleSbLmRed )

