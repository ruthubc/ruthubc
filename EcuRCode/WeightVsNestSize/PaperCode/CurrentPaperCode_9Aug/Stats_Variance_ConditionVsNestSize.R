# 
# 
###############################################################################



#### Condition Variance by nest size

spidersVar <- condition_residuals(spiders, "logLeg","n")

condVar <- calRelVariance(spidersVar, "condResiduals")

##Checking normality
ggplot(condVar, aes(x = relativeVar)) + geom_histogram() + xlim(0, 1) # seems normal enough




########### Checking AIC of all models ##########


outcome    <- c("relativeVar")
predictors <- c("logCtFm", "logCtFm:Instar", "I(logCtFm^2)", "I(logCtFm^2):Instar")
dataset    <- condVar

varianceCondAllMods <- allModelsAIC(outcome, predictors, dataset)




############ Graph grid by nest size


condVarianceOverall <- lmer(relativeVar ~  logCtFm + Instar + logCtFm:Instar + 
				(1|NestID), weights = N, data = condVar , REML = FALSE)

condVarianceNoInt <- lmer(relativeVar ~  logCtFm + Instar +
				(1|NestID), weights = N, data = condVar , REML = FALSE)

visgTest <- visreg(condVarianceNoInt, "logCtFm", by = "Instar")
visreg(condVarianceOverall, "logCtFm", by = "Instar")

visgTest <- visreg(condVarianceNoInt, "logCtFm", by = "Instar")


ggplot(fit , aes(logCtFm, visregFit))+
		geom_line(colour='blue', size=1) + facet_wrap(~Instar)

ggplot( condVar, aes(logCtFm, relativeVar))+
		geom_point(colour = 'blue') + 
		geom_line(data = fit, aes(x = logCtFm, y= visregFit)) + facet_wrap(~Instar)


### Graph grid by nest size
InstarGridGraph(condVar, "relativeVar", "Condition Variance", "y", "ConditionVarianceLeg_byInstar_ZGSA", condVarianceNoInt)



### Stats output to text file

vvcondVarianceOverallRed <- lmer(relativeVar ~  Instar +
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


