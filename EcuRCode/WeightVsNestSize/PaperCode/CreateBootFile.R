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

output <- sampleFunction(5, 500000, 1, 10 )

#ggplot (output, aes(mean)) + geom_histogram()

outputSub <- subset(output, mean > 7.999 & mean < 8.001)

write.csv(outputSub, "boots.csv")