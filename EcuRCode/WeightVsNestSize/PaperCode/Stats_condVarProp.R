# Author: Ruth
###############################################################################


source("G:/EclipseWorkspace/Python/ruthubc/EcuRCode/WeightVsNestSize/PaperCode/NestSizeData-Paper.R")
source("G:/EclipseWorkspace/Python/ruthubc/EcuRCode/WeightVsNestSize/PaperCode/CondResidualFunction.R")
source("G:/EclipseWorkspace/Python/ruthubc/EcuRCode/WeightVsNestSize/PaperCode/FunctionCalculateMaxVariance.R") # importing function
source("G:/EclipseWorkspace/Python/ruthubc/EcuRCode/WeightVsNestSize/PaperCode/InstarSizeGridGraphFunction.R")

############ Leg variance

legVar <- calRelVariance(spiders, "logLeg")

ggplot(legVar, aes(x = relativeVar)) + geom_histogram() +xlim(0, 1) # seems normal enough

InstarGridGraph(legVar, "relativeVar", "Leg Length Variance", "y", "LegLengthVariance")

LegVarianceOverall <- lmer(relativeVar ~  logCtFm + Instar + logCtFm:Instar + 
				(1|NestID), data = legVar, REML = FALSE)

LegVarianceOverallRed <- lmer(relativeVar ~  Instar +
				(1|NestID), data = legVar, REML = FALSE)


LMERS_fun <- function(myData) {
	lmFull <- lm(relativeVar ~  logCtFm, data = myData,)
	
	lmRed <- lm(relativeVar ~  1, data = myData,)
	
	return(list(lmFull, lmRed))
	
}


instarLMER <- dlply(legVar, .(Instar),        
		function(x) LMERS_fun(x))

sink('RuthEcuador2013/NestSize/StatsOutput/LegVarianceUsingALLNestsIncSingToGetVar.txt')

printOutput(LegVarianceOverall)
print("")
print("overall leg variance test- using variance within each nest")
anova(LegVarianceOverall, LegVarianceOverallRed)
print("")
print("leg variance by instar")
lapply(instarLMER, FUN = function(x) anova(x[[1]], x[[2]]))

sink()

#### Condition Variance

spidersVar<- condition_residuals(spiders, "logLeg","n")

condVar <- calRelVariance(spidersVar, "condResiduals")

ggplot(condVar, aes(x = relativeVar)) + geom_histogram() + xlim(0, 1) # seems normal enough

############ Graph

InstarGridGraph(condVar, "relativeVar", "Condition Variance TEST", "n", "ConditionVarianceLeg_byInstar_ZGSA")

condVarianceOverall <- lmer(relativeVar ~  logCtFm + Instar + logCtFm:Instar + 
				(1|NestID), weights = N, data = condVar , REML = FALSE)

condVarianceNoInt <- lmer(relativeVar ~  logCtFm + Instar +
				(1|NestID), weights = N, data = condVar , REML = FALSE)

visgTest <- visreg(condVarianceNoInt, "logCtFm", by = "Instar")
visreg(condVarianceOverall, "logCtFm", by = "Instar")

condVarianceOverallRed <- lmer(relativeVar ~  Instar +
				(1|NestID), weights = N, data = condVar, REML = FALSE)


LMERS_fun <- function(myData) {
	lmFull <- lm(relativeVar ~  logCtFm, data = myData,)
	
	lmRed <- lm(relativeVar ~  1, data = myData,)
	
	return(list(lmFull, lmRed))
	
}


instarLMER <- dlply(condVar, .(Instar),        
		function(x) LMERS_fun(x))

sink('RuthEcuador2013/NestSize/StatsOutput/condVarianceLegAsBDmeasure_ALLNESTSFORMAXMIN.txt')

print("note: tried including mean, and logCtFm^2 but all non significant")

print("Having sample size as a weights in the model")
printOutput(condVarianceOverall)
print("")

printOutput(condVarianceNoInt)
print("")

anova(condVarianceNoInt, condVarianceOverallRed)

print("")

print("overall Cond variance test- using variance within each nest")
anova(condVarianceOverall, condVarianceOverallRed)
print("")
print("Cond variance by instar")
lapply(instarLMER, FUN = function(x) anova(x[[1]], x[[2]]))

sink()


