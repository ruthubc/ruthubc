# Author: Ruth
###############################################################################
library(ggplot2)



sampleFunction <- function(sampleSize, noBoots, minVariable, maxVariable){
	outCols <- sampleSize + 2
	output <- matrix(ncol=outCols, nrow=noBoots)
	for(b in 1:noBoots){
		
		print(b)		
		
		rndSample <-runif(sampleSize, minVariable, maxVariable)  # random number generator
		mean <- mean(rndSample)
		sd <- sd(rndSample)#/ mean
		rndSample <- sort(rndSample)
		outputList <- c(rndSample, mean, sd)
		#print(outputList)
		output[b,] <- outputList

		
	}

	outputDF <- data.frame(output)
	colnames(outputDF)[sampleSize+1] <- "mean"
	colnames(outputDF)[sampleSize+2] <- "SD"
	
	return(outputDF)
	
}

output <- sampleFunction(17, 1000000, 1, 4)

#ggplot (output, aes(mean)) + geom_histogram()

outputSub <- subset(output, mean > 1.99 & mean < 2.01)

write.csv(outputSub, "boots_sample17_mean2.csv")