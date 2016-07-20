# Author: Ruth
###############################################################################

library(plyr)
library(dplyr)

# function to calculate maximum variance for eea
calMaxVarFun <- function(mean, sampSize, min, max){

	data_tot <- mean * sampSize
	list <- c(rep(min, sampSize))  # making a list with the min value for all spiders
	calTot<- sum(list)
	totDiff <- data_tot - calTot	
	n <- sampSize + 1
	numToInput <- totDiff + min  # because you are removing a min value
	
	
	while(abs(totDiff) > 0.0001){
	
		
		n <- n -1
		
		
		if (numToInput <= max){
			list[n] <- numToInput
			
		}else{
			list[n] <- max		
			
		}
		calTot<- sum(list)
		totDiff <- (data_tot - calTot)
		numToInput <- totDiff + min
#		print("calTot")
#		print(calTot)
#		print("totDiff")
#		print(totDiff)
#		print("numToInput")
#		print(numToInput)
#		print("list")
#		print(list)
		
	}
	#print(mean(list))
	return(list)
	
}

#calMaxVarFun(legVar$mean[14], 2, legVar$minVar[14], legVar$maxVar[14])
#
#calMaxVarFun(4, 10, 2, 7)
#
##



calRelVariance <- function(data, inputVar) {
	# returns subset = multiple
	
	
	column_index <- which(names(data) == inputVar)
	
	if(length(column_index) == 0){ stop('variable not found - check spelling')}
	
	
	spidersBoot <- select(data, one_of(c("NestID", "Instar", "CountFemales", "logCtFm", "InstarNumber", "InstarSex", "type", inputVar)))
	spidersBoot <- spidersBoot[complete.cases(spidersBoot), ]  # removing any NA's
	colnames(spidersBoot)[8] <- "variable"  # changing name of variable of interest for function
	
	spidersBoot <- ddply(spidersBoot, "Instar", transform, maxVar = max(variable), minVar = min(variable)) # calculating max and min
	
	spidersBootAve <- ddply(spidersBoot, .(NestID, Instar, CountFemales, logCtFm, minVar, maxVar, InstarNumber, InstarSex, type), summarise,
			N = length(variable),
			mean = mean(variable),
			sd_data = sd(variable)
	)
	
	spidersBootAve <- spidersBootAve[complete.cases(spidersBootAve), ]
	
	
	spidersBootAve$mean_list <- as.numeric(NA)
	spidersBootAve$sd_Max <- as.numeric(NA)
	
	numNests <- nrow(spidersBootAve)
	
	for(nest in 1:numNests){
		
		print(nest)
		data_mean <- spidersBootAve$mean[nest]
		sampSize <- spidersBootAve$N[nest]
		minVariable <- spidersBootAve$minVar[nest]
		maxVariable <- spidersBootAve$maxVar[nest]	
		maxVarList<- calMaxVarFun(data_mean, sampSize, minVariable,  maxVariable )  # mean, sampleSize, minVar, maxVar
		mean_list <- mean(maxVarList)
		sd_max <- sd(maxVarList)
		spidersBootAve[nest, 13] <- mean_list
		spidersBootAve[nest, 14] <- sd_max
	}
	
	spidersBootAve <- mutate(spidersBootAve, relativeVar =sd_data/sd_Max)
	
	print (max(spidersBootAve$relativeVar) )
	
	if(max(spidersBootAve$relativeVar) >= 1) { warning("something went wrong - real variance more than calculated max variance") }
	
	spidersBootAve <- subset(spidersBootAve, type == "multiple")

	
	return(spidersBootAve)
	
	
}
#
#condVar <- calRelVariance(spidersMult, "condResiduals")
#
#calMaxVarFun(legVar$mean[14], 2, condVar$minVar[14], condVar$maxVar[14])
#
#condVar$mean[36]
#

