# Author: Ruth
###############################################################################


require(reshape)
library(SciencesPo)
library(stringr)
library(qdap)
library(tidyr)

#TODO: (1) Errorhandling: if input variance is not in the cumDist table http://adv-r.had.co.nz/Exceptions-Debugging.html#condition-handling
#      (2) Increase number of bins


makeDistInputMean <- function(inputMean, sampleSize, minSize, maxSize) {
	
	measures <- seq(minSize, maxSize, by =  0.005)
	
	sumAim <- inputMean * sampleSize
	
	sumSample <- sumAim + 100
	
	while (sumSample >= (sumAim + minSize)) {
		randSamp <- sample(measures, (sampleSize-1))
		
		sumSample <- sum(randSamp)
		
	}
	
	diff <-  sumAim - sumSample
	output <- c(randSamp, diff)
	return(output)
}


bootstrapVariance <- function(inputMean, sampleSize, minSize, maxSize, inputVariance){

	df <- data.frame(mean=numeric(), SD = numeric())
	
	i <- 0
	
	while (i <= 5000) {
		i <- i + 1
		output <- makeDistInputMean(inputMean, sampleSize, minSize, maxSize)
		randMean <- mean(output)
		randSD <- sd(output)
		df <- rbind(df, c(randMean, randSD))
	}
	
	names(df)[1] <- "mean"
	names(df)[2] <- "SD"
	
	
	freqDist <- freq(df$SD)
	
	freqDist$class <- as.character(freqDist$class)
	
	freqDist$class <- gsub("]", "", freqDist$class)
	freqDist$class<- gsub("\\(", "", freqDist$class)
	freqDist$class <- gsub("\\[", "", freqDist$class)
	
	freqDist <- separate(freqDist, class, c("lowerLmt", "upperLmt"), sep = ",")
	freqDist$lowerLmt <- as.numeric(freqDist$lowerLmt)
	freqDist$upperLmt <- as.numeric(freqDist$upperLmt)
	
	
	rowIndex <- which(freqDist$lowerLmt <= inputVariance & freqDist$upperLmt > inputVariance)
	
	bootVar <- freqDist$cumperc[rowIndex]
	
	return(bootVar)
	
}


bootstrapVariance(20, 10, 0, 100, 14)


SD_ecdf <- ecdf(df$SD)

plot(SD_ecdf)



find_bootstrapVariance <- function(data, inputVar){
	
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
	
	
	numNests <- nrow(spidersBootAve)
	
	for(nest in 1:numNests){
		
		print(nest)
		print(as.character(spidersBootAve$NestID[nest]))
		print(as.character(spidersBootAve$Instar[nest]))
		data_mean <- spidersBootAve$mean[nest]
		sampSize <- spidersBootAve$N[nest]
		minVariable <- spidersBootAve$minVar[nest]
		maxVariable <- spidersBootAve$maxVar[nest]
		sd_org <- spidersBootAve$sd_data[nest]
		bootstrapVarReturn <- bootstrapVariance(data_mean, sampSize, minVariable, maxVariable, sd_org) #inputMean, sampleSize, minSize, maxSize, inputVariance
		print(str_c("bootstrap variance:", bootstrapVarReturn))

	}	
	
}

find_bootstrapVariance(spidersMul, "logWt")