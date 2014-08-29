# TODO: Add comment
# 
# Author: user
###############################################################################


# getting bootstrap samples for the coeficient of variation to try and remove
# the influence of large sample sizes

# start with just the adults, need to make a list of all the values for each nest
#still need a minimum number 

bootFunction <- function(nestID, weightList, noInSamp, nboots){

outputDF <- data.frame(matrix(NA, nrow = 0, ncol = (noInSamp +1)))

	for(j in seq_len(nboots)){
		bootvals <-  (sample(weightList, size = noInSamp, replace = TRUE)) # var = variace
		print (j)
		print (c(nestID, bootvals))
		outputDF[j,] <- c(nestID, bootvals)
	}
return (outputDF)	
}



spidersAdult <- subset(spidersMul, Instar == "Adult")

spidersAdult <- spidersAdult[complete.cases(spidersAdult$logWeight),]

bootNests <- levels(spidersAdult$NestID)



bootWeights <- data.frame(matrix(NA, nrow = 0, ncol = (sampleSize +1)))
sampleSize <- 8
nboots <- 10

for(i in bootNests){
	
	print (i)	
	table <- spidersAdult[which (spidersAdult$NestID == i),]
	weights <- table$logWeight
	bootWeightsNew <- bootFunction(i, weights, sampleSize, nboots )
	bootWeights <- rbind(bootWeightsNew, bootWeights)
}

bootWeights$X2 <- as.numeric(bootWeights$X2)

for (g in 2:ncol(bootWeights)){ #converts the numbers to numbers
	
	bootWeights[,g] <- as.numeric(bootWeights[,g])
}



bootWeights$means <- rowMeans(bootWeights[,2:(sampleSize + 1)])




############# Test Function  ###########

df <- data.frame(matrix(NA, nrow = 0, ncol = (2)))


testFunction <- function(table){
	for (j in seq_len(nboots)){
		print (j)
		df[j,] <- c(1,2)
		return (df)
		
	}

}

testFunction(df)

