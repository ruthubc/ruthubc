

######## Leg vs Nest Size #####

InstarGridGraph(spidersMul, "logLeg", "Leg Length - log transformed", "y", "LegLengthvsNestSize_by_Instar_ZGSA")

LMERS_fun <- function(myData) {
	lmFull <- lm(logLeg ~  logCtFm, data = myData)
	
	lmRed <- lm(logLeg ~  1, data = myData,)
	
	return(list(lmFull, lmRed))
	
}


instarLMER <- dlply(spidersMul, .(Instar),        
		function(x) LMERS_fun(x))


sink('RuthEcuador2013/NestSize/StatsOutput/LegLengthVsNestSize.txt')

legLmFull <- lmer(logLeg ~  I(logCtFm^2) + logCtFm + Instar + logCtFm:Instar + I(logCtFm^2):Instar + 
				(1|NestID), data = spidersMul, REML = FALSE)

printOutput(legLmFull)


legLmNoSq <- lmer(logLeg ~  logCtFm + Instar + logCtFm:Instar + 
				(1|NestID), data = spidersMul, REML = FALSE)

printOutput(legLmNoSq)


legLmRed <- lmer(logLeg ~ Instar + (1|NestID), data = spidersMul, REML = FALSE)
print("")
print("no square vs red")
anova(legLmNoSq, legLmRed) # comparing full model to reduced model: very significant

print("")


print("leg length by instar")
lapply(instarLMER, FUN = function(x) anova(x[[1]], x[[2]]))

sink()





