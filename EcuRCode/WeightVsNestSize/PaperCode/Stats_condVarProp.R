# Author: Ruth
###############################################################################

source("G:/EclipseWorkspace/Python/ruthubc/EcuRCode/WeightVsNestSize/PaperCode/NestSizeData-Paper.R")
source("G:/EclipseWorkspace/Python/ruthubc/EcuRCode/WeightVsNestSize/PaperCode/CondResidualFunction.R")
source("G:/EclipseWorkspace/Python/ruthubc/EcuRCode/WeightVsNestSize/PaperCode/FunctionCalculateMaxVariance.R") # importing function
source("G:/EclipseWorkspace/Python/ruthubc/EcuRCode/WeightVsNestSize/PaperCode/InstarSizeGridGraphFunction.R")

############ Leg variance

legVar <- calRelVariance(spiders, "logLeg")

ggplot(legVar, aes(x = relativeVar)) + geom_histogram() +xlim(0, 1) # seems normal enough

InstarGridGraph(legVar, "relativeVar")

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

print("overall leg variance test- using variance within each nest")
anova(LegVarianceOverall, LegVarianceOverallRed)

print("leg variance by instar")
lapply(instarLMER, FUN = function(x) anova(x[[1]], x[[2]]))

sink()

#### Condition Variance

spidersVar<- condition_residuals(spiders, "logLeg")

condVar <- calRelVariance(spidersVar, "condResiduals")

ggplot(condVar, aes(x = relativeVar)) + geom_histogram() + xlim(0, 1) # seems normal enough

InstarGridGraph(condVar, "relativeVar")

condVarianceOverall <- lmer(relativeVar ~  logCtFm + Instar + logCtFm:Instar + 
				(1|NestID), data = condVar, REML = FALSE)

condVarianceOverallRed <- lmer(relativeVar ~  Instar +
				(1|NestID), data = condVar, REML = FALSE)


LMERS_fun <- function(myData) {
	lmFull <- lm(relativeVar ~  logCtFm, data = myData,)
	
	lmRed <- lm(relativeVar ~  1, data = myData,)
	
	return(list(lmFull, lmRed))
	
}


instarLMER <- dlply(condVar, .(Instar),        
		function(x) LMERS_fun(x))

sink('RuthEcuador2013/NestSize/StatsOutput/condVarianceLegAsBDmeasure_ALLNESTSFORMAXMIN.txt')

print("overall Cond variance test- using variance within each nest")
anova(condVarianceOverall, condVarianceOverallRed)

print("Cond variance by instar")
lapply(instarLMER, FUN = function(x) anova(x[[1]], x[[2]]))

sink()


########### Variance by instar

#########Leg #################################

ggplot(legVar, aes(x = Instar, y = relativeVar)) + geom_boxplot()


sink('RuthEcuador2013/NestSize/StatsOutput/legVarByInstar_MMincSing.txt')

print("the max and min to cal the variance inc single nests")

legVarLmFull <- lmer(relativeVar ~  Instar + 
				(1|NestID), data = legVar, REML = FALSE)

print("Instar as factor")
printOutput(legVarLmFull)

print("Instar as Number")

## InteractionNotSignaficant
legVarNumFull <- lmer(relativeVar ~  InstarNumber + InstarSex + I(InstarNumber^2) +
				(1|NestID), data = legVar, REML = FALSE)

printOutput(legVarNumFull)

legVarNumNoIntercept <- lmer(relativeVar ~  InstarNumber + InstarSex +
				(1|NestID), data = legVar, REML = FALSE)

printOutput(legVarNumNoIntercept)

print("Model with no intercept vs reduced model")

legVarNumRed <- lmer(relativeVar ~  InstarSex +
				(1|NestID), data = legVar, REML = FALSE)

anova(legVarNumNoIntercept, legVarNumRed)


sink()


############ Conditions ####################
ggplot(condVar, aes(x = Instar, y = relativeVar)) + geom_boxplot()

sink('RuthEcuador2013/NestSize/StatsOutput/condVarByInstar_MMincSing.txt')

print("the max and min to cal the variance inc single nests")

CondVarLmFull <- lmer(relativeVar ~  Instar + 
				(1|NestID), data = condVar, REML = FALSE)

print("Instar as factor")
printOutput(CondVarLmFull)

print("Instar as Number")

## InteractionNotSignaficant
CondVarNumFull <- lmer(relativeVar ~  InstarNumber + InstarSex + I(InstarNumber^2) + InstarNumber:InstarSex+ 
				(1|NestID), data = condVar, REML = FALSE)

printOutput(CondVarNumFull)

CondVarNumNoIntercept <- lmer(relativeVar ~  InstarNumber + InstarSex + InstarNumber:InstarSex + 
				(1|NestID), data = condVar, REML = FALSE)

printOutput(CondVarNumNoIntercept)

print("Model with no intercept vs reduced model")

CondVarNumRed <- lmer(relativeVar ~  InstarSex +
				(1|NestID), data = condVar, REML = FALSE)

anova(CondVarNumNoIntercept, CondVarNumRed)

sink()

###########just looking at condition
ggplot(spidersMult, aes(x = Instar, y = condResiduals)) + geom_boxplot()

conditionInstarLmFull <- lmer(condResiduals ~  Instar + 
				(1|NestID), data = spidersMult, REML = FALSE)

anova(conditionInstarLmFull)


visreg(conditionInstarLmFull)
