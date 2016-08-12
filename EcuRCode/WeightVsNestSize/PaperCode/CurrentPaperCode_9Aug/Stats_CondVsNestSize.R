
spiConRes <- condition_residuals(spiders, "logLeg", "y") # Calculating condition residuals

### Testing AIC of all models

outcome    <- c("condResiduals")
predictors <- c("logCtFm", "logCtFm:Instar", "I(logCtFm^2)", "I(logCtFm^2):Instar")
dataset    <- spiConRes

conditionAllMods <- allModelsAIC(outcome, predictors, dataset)


######## Condition vs Nest Size #####

condLmRed <- lmer(condResiduals ~ Instar + (1|NestID), data = spiConRes, REML = FALSE)


condLmFull <- lmer(condResiduals ~  I(logCtFm^2) + logCtFm + Instar + 
				(1|NestID), data = spiConRes, REML = FALSE)

printOutput(condLmFull)


condLmNoSq <- lmer(condResiduals ~  logCtFm + Instar  + 
				(1|NestID), data = spiConRes, REML = FALSE)

printOutput(condLmNoSq)

condSqOnlyLm <- lmer(condResiduals ~  I(logCtFm^2) + Instar + 
				(1|NestID), data = spiConRes, REML = FALSE)



#######################


InstarGridGraph(spiConRes, "condResiduals", "Body Condition", "y", "BodyCondvsNestSize_by_Instar_ZGSA", legLmNoSq)

pdf("RuthEcuador2013/NestSize/Graphs/ConditionModel.pdf", height=14, width=16)

ggplot(spiders, aes(x = logLeg, y = logWt)) + geom_point(aes(colour = Instar)) + mytheme + stat_smooth(method = "lm", formula = y~x, se = FALSE, colour = "black") +
		ylab("Weight log transformed") + xlab("Leg Length log transformed") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

dev.off()


############  Models by instar #########


LMERS_fun <- function(myData) {
	lmFull <- lm(condResiduals ~  logCtFm, data = myData)
	
	lmRed <- lm(condResiduals ~  1, data = myData,)
	
	return(list(lmFull, lmRed))
	
}

instarLMER <- dlply(spiConRes, .(Instar),        
		function(x) LMERS_fun(x))


