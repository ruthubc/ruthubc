# TODO: Add comment
# 
# Author: user
###############################################################################


# getting bootstrap samples for the coeficient of variation to try and remove
# the influence of large sample sizes

# start with just the adults, need to make a list of all the values for each nest
#still need a minimum number 

bootFunction <- function(nestID, weightList, noInSamp, outputDF, nboots){

	
	for(j in seq_len(nboots)){
		bootvals <-  (sample(weightList, size = noInSamp, replace = TRUE)) # var = variace
		print (j)
		print (c(nestID, bootvals))
		outputDF[j,] <- c("nestID", bootvals)
	}
	
}



spidersAdult <- subset(spidersMul, Instar == "Adult")

bootNests <- levels(spidersAdult$NestID)



bootWeights <- data.frame(matrix(NA, nrow = 0, ncol = (sampleSize +1)))
sampleSize <- 8

for(i in bootNests){
	
	print (i)	
	table <- spidersAdult[which (spidersAdult$NestID == i),]
	weights <- table$logWeight
	bootFunction(i, weights, sampleSize, bootWeights, 10 )
}



df <- data.frame(matrix(NA, nrow = 0, ncol = (2)))


testFunction <- function(table){
	for (j in seq_len(nboots)){
		print (j)
		df[j,] <- c(1,2)
		return (df)
		
	}

}

testFunction(df)

