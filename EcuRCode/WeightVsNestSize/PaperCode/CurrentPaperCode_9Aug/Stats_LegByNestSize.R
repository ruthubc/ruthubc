



######## Leg vs Nest Size #####



LMERS_fun <- function(myData) {
	lmFull <- lm(logLeg ~  logCtFm, data = myData)
	
	lmRed <- lm(logLeg ~  1, data = myData,)
	
	return(list(lmFull, lmRed))
	
}


instarLMER <- dlply(spidersMul, .(Instar),        
		function(x) LMERS_fun(x))




legLmFull <- lmer(logLeg ~  I(logCtFm^2) + logCtFm + Instar + logCtFm:Instar + I(logCtFm^2):Instar + 
				(1|NestID), data = spidersMul, REML = FALSE)


stepOutput <- step(legLmNoSq)

stepOutput$anova
summary(stepOutput)
stepOutput$model


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



legOnlySqr <- lmer(logLeg ~ I(logCtFm^2) + (1|NestID), data = spidersMul, REML = FALSE)
legRed <- lmer(logLeg ~  (1|NestID), data = spidersMul, REML = FALSE)
anova(legOnlySqr)



InstarGridGraph(spidersMul, "logLeg", "Leg Length - log transformed", "n", "LegLengthvsNestSize_by_Instar_ZGSA", legLmNoSq)

Vis_fit <- (visreg(legOnlySqr, "logCtFm", plot = FALSE))$fit

Vis_fit <- subset(legOnlySqr, Instar == current.Instar)

p <- p + geom_line(data = Vis_fit, aes(x = (10^logCtFm), y= visregFit))

ggplot(spidersMul, aes(logCtFm, logLeg)) + geom_point() + geom_line(data = Vis_fit, aes(x = (logCtFm), y= visregFit))

## Model with lowest AIC value
legLmFull <- lmer(logLeg ~  I(logCtFm^2) + logCtFm + Instar  + 
				(1|NestID), data = spidersMul, REML = FALSE)
anova(legLmFull)

legLmRed <- lmer(logLeg ~ Instar  + 
				(1|NestID), data = spidersMul, REML = FALSE)

anova(legLmFull, legLmRed)

InstarGridGraph(spidersMul, "logLeg", "Leg Length - log transformed", "n", "LegLengthvsNestSize_by_Instar_ZGSA", legLmFull)


