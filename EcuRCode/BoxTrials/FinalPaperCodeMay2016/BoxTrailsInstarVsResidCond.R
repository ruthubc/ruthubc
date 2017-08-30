
##########################################################################################################################
# Checking if the new method of finding condition - residual condition means condition is no longer significant by instar


sink('RuthEcuador2013/BoxFeedingTrials/StatsOutput/InstarVsCond.txt')
"Checking if the new method of finding condition - residual condition means condition is no longer significant by instar"

CondInstarMod <- lm(residCond ~ Instar, data = BoxCombo)

CondInstarMod$call
anova(CondInstarMod)
"Not Significant - no where near"

CondInstarModRed <- lm(residCond ~ 1, data = BoxCombo)

anova(CondInstarMod, CondInstarModRed)

xtabs( ~ Instar, data=  BoxCombo)

sink()



