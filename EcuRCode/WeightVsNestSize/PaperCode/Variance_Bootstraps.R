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
	
	sumSample <- sumAim
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
	
	while (i <= 100) {
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



