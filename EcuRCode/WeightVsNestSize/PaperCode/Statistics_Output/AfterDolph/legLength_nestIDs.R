# TODO: Add comment
# 
# Author: Ruth
###############################################################################

setwd("C:/Users/Ruth/Dropbox/")

nestsList <- levels(spidersMul$NestID)


pdf("RuthSync/Thesis/legGraphs.pdf")

for (i in 1:length(nestsList)) {
	
title <- as.character(nestsList[i])
	
InstarGridGraph(spidersMul, "logLeg", title, nestID = nestsList[i], "n", "", FullModelLeg)
}

dev.off()

pdf("RuthSync/Thesis/condGraphs.pdf")

for (i in 1:length(nestsList)) {
	
	title <- as.character(nestsList[i])
	
	InstarGridGraph(spidersMul, "condResiduals", title, nestID = nestsList[i], "n", "", FullModel)
}

dev.off()