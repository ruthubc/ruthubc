# TODO: Add comment
# 
# Author: user
###############################################################################


SpiAveMerge<- ddply(spidersMul, .(NestID, Instar), summarise, 
		N = length(!is.na(Weight.mg)),
		LogLegMean = mean(logLeg, na.rm = TRUE),
		condSqMean = mean(conditionSq, na.rm = TRUE),
		condCbSqMean = mean(condCbSq, na.rm = TRUE)
)

