
spiConRes <- condition_residuals(spiders, "logLeg", "y")


######## Condition vs Nest Size #####



LMERS_fun <- function(myData) {
	lmFull <- lm(condResiduals ~  logCtFm, data = myData)
	
	lmRed <- lm(condResiduals ~  1, data = myData,)
	
	return(list(lmFull, lmRed))
	
}


instarLMER <- dlply(spiConRes, .(Instar),        
		function(x) LMERS_fun(x))


sink('RuthEcuador2013/NestSize/StatsOutput/ConditionResidual_legVsNestSize.txt')

legLmFull <- lmer(condResiduals ~  I(logCtFm^2) + logCtFm + Instar + logCtFm:Instar + I(logCtFm^2):Instar + 
				(1|NestID), data = spiConRes, REML = FALSE)

printOutput(legLmFull)


legLmNoSq <- lmer(condResiduals ~  logCtFm + Instar + logCtFm:Instar + 
				(1|NestID), data = spiConRes, REML = FALSE)

printOutput(legLmNoSq)


legLmRed <- lmer(condResiduals ~ Instar + (1|NestID), data = spiConRes, REML = FALSE)
print("")
print("no square vs red")
anova(legLmNoSq, legLmRed) # comparing full model to reduced model: very significant

print("")


print("leg length by instar")
lapply(instarLMER, FUN = function(x) anova(x[[1]], x[[2]]))

sink()


InstarGridGraph(spiConRes, "condResiduals", "Body Condition", "y", "BodyCondvsNestSize_by_Instar_ZGSA", legLmNoSq)

pdf("RuthEcuador2013/NestSize/Graphs/ConditionModel.pdf", height=14, width=16)

ggplot(spiders, aes(x = logLeg, y = logWt)) + geom_point(aes(colour = Instar)) + mytheme + stat_smooth(method = "lm", formula = y~x, se = FALSE, colour = "black") +
		ylab("Weight log transformed") + xlab("Leg Length log transformed") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

dev.off()


