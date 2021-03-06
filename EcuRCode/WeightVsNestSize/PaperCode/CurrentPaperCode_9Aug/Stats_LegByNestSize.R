
######## Statistics of Leg vs Nest Size #######



### Testing AIC of all models

outcome    <- c("logLeg")
predictors <- c("logCtFm", "logCtFm:Instar", "I(logCtFm^2)", "I(logCtFm^2):Instar")
dataset    <- spidersMul

legLegAllMods <- allModelsAIC(outcome, predictors, dataset)


## Model with lowest AIC value
legLmFull <- lmer(logLeg ~  I(logCtFm^2) + logCtFm + Instar + logCtFm:Instar + I(logCtFm^2):Instar + 
				(1|NestID)  , data = spidersMul, REML = FALSE)




legLmRed <- lmer(logLeg ~ Instar  + 
				(1|NestID), data = spidersMul, REML = FALSE)



## Model without square term

legLmNoSqFull <- lmer(logLeg ~  logCtFm + Instar  + 
				(1|NestID), data = spidersMul, REML = FALSE)



############# Using non-squared term as full model
FullModel <- lmer(logLeg ~  logCtFm + Instar + logCtFm:Instar  +  (1|NestID)  , data = spidersMul, REML = FALSE)
RedModel <- lmer(logLeg ~  Instar +  (1|NestID)  , data = spidersMul, REML = FALSE)
anova(FullModel, RedModel)



#InstarGridGraph(spidersMul, "logLeg", "Leg Length - log transformed", "n", "LegLengthvsNestSize_by_Instar_ZGSA", legLmFull)




# Testing Individual Instars (although not necessary)

LMERS_fun <- function(myData) { # function to test individual instars
	lmFull <- lm(logLeg ~  logCtFm, data = myData)
	
	lmRed <- lm(logLeg ~  1, data = myData,)
	
	return(list(lmFull, lmRed))
	
}


instarLMER <- dlply(spidersMul, .(Instar),        
		function(x) LMERS_fun(x))


print("leg length by instar")
lapply(instarLMER, FUN = function(x) anova(x[[1]], x[[2]]))






