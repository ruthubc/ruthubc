# Author: Ruth
###############################################################################


############ Leg variance by nest size

legVar <- calRelVariance(spiders, "logLeg")

## checking if normal
ggplot(legVar, aes(x = relativeVar)) + geom_histogram() +xlim(0, 1) # seems normal enough



## models
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

### Outputting the stat results to text file

sink('RuthEcuador2013/NestSize/StatsOutput/LegVarianceUsingALLNestsIncSingToGetVar.txt')

printOutput(LegVarianceOverall)
print("")
print("overall leg variance test- using variance within each nest")
anova(LegVarianceOverall, LegVarianceOverallRed)
print("")
print("leg variance by instar")
lapply(instarLMER, FUN = function(x) anova(x[[1]], x[[2]]))

sink()


### Graph 
InstarGridGraph(legVar, "relativeVar", "Leg Length Variance", "y", "LegLengthVariance", "ConditionVarianceLeg_byInstar_ZGSA")

