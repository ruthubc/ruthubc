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

InstarGridGraph(condVar, "relativeVar", "Condition Variance", "y", "ConditionVarianceLeg_byInstar_ZGSA")

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

printOutput(condVarianceOverall)
print("")

print("overall Cond variance test- using variance within each nest")
anova(condVarianceOverall, condVarianceOverallRed)
print("")
print("Cond variance by instar")
lapply(instarLMER, FUN = function(x) anova(x[[1]], x[[2]]))

sink()


########### Variance vs instar

#########Leg variance #################################

pdf("RuthEcuador2013/NestSize/Graphs/LegLengthVarianceByInstar.pdf", height=13, width=13)
ggplot(data = legVar, aes(x = Instar, y = relativeVar)) + geom_boxplot() + mytheme + ylab("Leg Length Variance")
dev.off()


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


print("")
print("females only")
legVarLmFull_FemOnly <- lmer(relativeVar ~  Instar + 
				(1|NestID), data = subset(legVar, InstarSex == "F"), REML = FALSE)

printOutput(legVarLmFull_FemOnly)

sink()


############ Conditions variance ####################

pdf("RuthEcuador2013/NestSize/Graphs/ConditionVarianceByInstar.pdf", height=13, width=13)
ggplot(condVar, aes(x = Instar, y = relativeVar)) + geom_boxplot() + mytheme + ylab("Condition Variance")
dev.off()

ggplot(condVar, aes(x = InstarNumber, y = relativeVar)) + geom_point() + stat_smooth(method = "lm", formula = y~poly(x , 2), se = FALSE)

sink('RuthEcuador2013/NestSize/StatsOutput/condVarByInstar_MMincSing.txt')

print("the max and min to cal the variance inc single nests")

CondVarLmFull <- lmer(relativeVar ~  1+ Instar + 
				(1|NestID), data = condVar, REML = FALSE)

print("Instar as factor")
printOutput(CondVarLmFull)
print("")
print("Instar as Number")

## InteractionNotSignaficant
CondVarNumFull <- lmer(relativeVar ~  InstarNumber + InstarSex + I(InstarNumber^2) + InstarNumber:InstarSex +  
				(1|NestID), data = condVar, REML = FALSE)

printOutput(CondVarNumFull)

CondVarNumNoIntercept <- lmer(relativeVar ~  InstarNumber + InstarSex + InstarNumber:InstarSex + 
				(1|NestID), data = condVar, REML = FALSE)

printOutput(CondVarNumNoIntercept)

print("Model with no intercept vs reduced model")

CondVarNumRed <- lmer(relativeVar ~  InstarSex +
				(1|NestID), data = condVar, REML = FALSE)

anova(CondVarNumNoIntercept, CondVarNumRed)

print("")
print("females only")
CondVarLmFull_FemOnly <- lmer(relativeVar ~  Instar + 
				(1|NestID), data = subset(condVar, InstarSex == "F"), REML = FALSE)

printOutput(CondVarLmFull_FemOnly)

sink()

###########just looking at condition
ggplot(spidersVar, aes(x = Instar, y = condResiduals)) + geom_boxplot() 

ggplot(spidersVar, aes(x = InstarNumber, y = condResiduals, colour = InstarSex)) + geom_point() + stat_smooth(method = "lm", formula = y~x, se = FALSE)

conditionInstarLmFull <- lmer(condResiduals ~  Instar + 
				(1|NestID), data = spidersVar, REML = FALSE)

anova(conditionInstarLmFull)

conditionInstarNumLmFull <- lmer(condResiduals ~  InstarNumber + InstarSex + InstarNumber:InstarSex +
				(1|NestID), data = spidersVar, REML = FALSE)

conditionInstarNum_FemOnly <- lmer(condResiduals ~  InstarNumber +
				(1|NestID), data = subset(spidersVar, InstarSex == "F"), REML = FALSE)

anova(conditionInstarNum_FemOnly)

anova(conditionInstarNumLmFull)





visreg(conditionInstarNumLmFull)



