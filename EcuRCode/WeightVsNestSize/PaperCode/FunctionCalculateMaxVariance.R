# Author: Ruth
###############################################################################

instarInput <- "Adult"


spidersBoot <- subset(spidersMul, Instar == instarInput, select = c(NestID, logLeg))

spidersBoot <- subset(spidersMul, Instar == instarInput & NestID == "28.8EXa03", select = c(NestID, logLeg))



spidersBoot <- spidersBoot[complete.cases(spidersBoot), ]  # removing any NA's

colnames(spidersBoot)[2] <- "variable"  # changing name of variable of interest for function

spidersBootAve <- ddply(spidersBoot, .(NestID), summarise,
		N = length(variable),
		mean = mean(variable))


maxVariable <- max(spidersBoot$variable)
minVariable <- min(spidersBoot$variable)

for(nest in 1:nrow(spidersBootAve)){
	mean <- spidersBootAve$mean[nest]
	sampSize <- spidersBootAve$N[nest]
	list <- c(rep(minVariable, sampSize))
	mean(list)
	
	
	
}


list <- c(rep(0, 50))





