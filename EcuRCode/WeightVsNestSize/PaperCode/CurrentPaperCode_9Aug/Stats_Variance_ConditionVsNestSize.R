# 
# 
###############################################################################

####### Boovar

bootVar <- read.csv("RuthSync/BootStrapVarianceForCluster/combined5Sept_13Missing.csv")

########### Checking AIC of all models ##########

ggplot(data = bootVar, aes(x = bootSD_cond)) + geom_histogram()
#meanCond <- mean(bootVar$bootSD_cond)
#bootVar$bootSD_trans <- log10(bootVar$bootSD_cond/(meanCond)+0.00001)
bootVar$bootSD_trans <- bootVar$bootSD_cond^(1/3)



ggplot(data = bootVar, aes(x = bootSD_trans)) + geom_histogram()


outcome    <- c("relativeVar")
predictors <- c("logCtFm", "logCtFm:Instar", "I(logCtFm^2)", "I(logCtFm^2):Instar")
dataset    <- condVar

varianceCondAllMods <- allModelsAIC(outcome, predictors, dataset)

myData <- subset(condVar, N >3)
ggplot(data = myData, aes(N, fill = Instar)) + geom_histogram()
ggplot(data = myData, aes(relativeVar, fill = Instar)) + geom_histogram()

require(car)
# from http://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html

qqp(myData$relativeVar, "norm")


########## Relative variance over max
############ Graph grid by nest size

ggplot(condVar, aes(N, relativeVar)) + geom_point() + geom_smooth(se = FALSE)
ggplot(condVar, aes(logCtFm, N)) + geom_point() + geom_smooth(se = FALSE)

totSpis <- sum(condVar$N)
condVar$lmrWgts <- condVar$N/ totSpis
sum(condVar$lmrWgts)


condVarianceOverall <- lmer(relativeVar ~  logCtFm + Instar + logCtFm:Instar + 
				(1|NestID), data = myData, weights = lmrWgts , REML = FALSE)

#print(VarCorr(condVarianceOverall),comp=c("Variance","Std.Dev."))
extractAIC(condVarianceOverall)
anova(condVarianceOverall)

condVarianceNoInt <- lmer(relativeVar ~  logCtFm + Instar +
				(1|NestID), weights = N, data = condVar , REML = FALSE)

anova(condVarianceNoInt)

model <- lmer(relativeVar ~  logCtFm  + Instar + (1|NestID), data = myData, weights = lmrWgts , REML = FALSE)
extractAIC(model)


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



ggplot(data = condVar, aes(x = Instar, y = relativeVar, fill = InstarSex)) + labs(x = "Instar") +
		labs(y = "Variance in Condition") + geom_boxplot() + mytheme


