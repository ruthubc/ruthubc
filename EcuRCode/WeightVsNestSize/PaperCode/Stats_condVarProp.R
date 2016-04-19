# Author: Ruth
###############################################################################

source("G:/EclipseWorkspace/Python/ruthubc/EcuRCode/WeightVsNestSize/PaperCode/FunctionCalculateMaxVariance.R")

source("${PROJECT_LOC:/WeightVsNestSize/PaperCode/FunctionCalculateMaxVariance.R}")

source("FunctionCalculateMaxVariance.R", local = TRUE)
PROJECT_LOC$FunctionCalculateMaxVariance.R

### Leg variance

legVar <- calRelVariance(spidersMul, "logLeg")

ggplot(legVar, aes(x = relativeVar)) + geom_histogram() # seems normal enough

InstarGridGraph(legVar, "relativeVar")

LegVarianceOverall <- lmer(relativeVar ~  logCtFm + Instar + logCtFm:Instar + 
				(1|NestID), data = legVar, REML = FALSE)

anova(LegVarianceOverall)



