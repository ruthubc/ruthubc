# Author: Ruth
###############################################################################

library(plyr)
library(dplyr)

# function to calculate maximum variance
calMaxVarFun <- function(mean, sampSize, min, max){
	
	data_tot <- mean * sampSize
	list <- c(rep(min, sampSize))  # making a list with the min value for all spiders
	calTot<- sum(list)
	totDiff <- data_tot - calTot	
	n <- sampSize + 1
	numToInput <- totDiff + min
	
	
	while(totDiff > 0.00){
		
		n <- n -1
		numToInsert <- max + min
		
		if (numToInput < max){
			list[n] <- numToInput
			
		}else{
			list[n] <- max		
			
		}
		calTot<- sum(list)
		totDiff <- (data_tot - calTot)
		numToInput <- totDiff + min
		
	}
	return(list)
	
}



calRelVariance <- function(data, inputVar) {
	
	
	column_index <- which(names(data) == inputVar)
	
	if(length(column_index) == 0){ stop('variable not found - check spelling')}
	
	
	spidersBoot <- select(data, one_of(c("NestID", "Instar", "CountFemales", "logCtFm", inputVar)))
	spidersBoot <- spidersBoot[complete.cases(spidersBoot), ]  # removing any NA's
	colnames(spidersBoot)[5] <- "variable"  # changing name of variable of interest for function
	
	spidersBoot <- ddply(spidersBoot, "Instar", transform, maxVar = max(variable), minVar = min(variable)) # calculating max and min
	
	spidersBootAve <- ddply(spidersBoot, .(NestID, Instar, CountFemales, logCtFm, minVar, maxVar), summarise,
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
		spidersBootAve[nest, 10] <- mean_list
		spidersBootAve[nest, 11] <- sd_max
	}
	
	spidersBootAve <- mutate(spidersBootAve, relativeVar =sd_data/sd_Max)
	
	print (max(spidersBootAve$relativeVar) )
	
	if(max(spidersBootAve$relativeVar) >= 1) { warning("something went wrong - real variance more than calculated max variance") }

	
	return(spidersBootAve)
	
	
}



