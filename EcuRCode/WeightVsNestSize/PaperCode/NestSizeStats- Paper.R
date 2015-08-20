# TODO: Add comment
# 
# Author: Ruth
###############################################################################

library (lme4)
library(lmerTest)

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
visreg(condLmFull)

visreg(condLmFull, xvar = "logCtFm", by = "Instar")

condLmNoSq <- lmer(conditionSq ~  logCtFm + Instar + logCtFm:Instar + 
				(1|NestID), data = spidersMul, REML = FALSE)

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

anova(legAdLmNoSq, legAdLmRed )