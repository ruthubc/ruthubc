# TODO: Add comment
# 
# Author: Ruth
###############################################################################

setwd("C:/Users/Ruth/Dropbox/")

nestsList <- levels(spidersMul$NestID)


pdf("RuthSync/Thesis/legGraphs.pdf", width = 10, height = 6)

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


write.csv(spidersMul, "RuthSync/Thesis/NestSizeDateLA.csv")