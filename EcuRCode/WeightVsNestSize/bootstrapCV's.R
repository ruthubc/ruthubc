# TODO: Add comment
# 
# Author: user
###############################################################################


# getting bootstrap samples for the coeficient of variation to try and remove
# the influence of large sample sizes

# start with just the adults, need to make a list of all the values for each nest
#still need a minimum number 

library("plyr")

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
nboots <- 100

for(i in bootNests){
	
	print (i)	
	table <- spidersAdult[which (spidersAdult$NestID == i),]
	weights <- table$logWeight
	bootWeightsNew <- bootFunction(i, weights, sampleSize, nboots )
	bootWeights <- rbind(bootWeightsNew, bootWeights)
}

for (g in 2:ncol(bootWeights)){ #converts the numbers to numbers
	
	bootWeights[,g] <- as.numeric(bootWeights[,g])
}



bootWeights$means <- rowMeans(bootWeights[,2:(sampleSize + 1)])

bootWeights$sum <- rowSums(bootWeights[,2:(sampleSize + 1)])
bootWeights$cv <- 0


for (r in 1:nrow(bootWeights)){
sdSum <- 0	
	mean <- bootWeights$means[r]

	for (c in 2:sampleSize){
		btwt <- bootWeights[r, c]
		indSd <- (btwt - mean)^2
		sdSum <- sdSum + indSd
		
	}
stddev <- sqrt(sdSum/sampleSize)
bootWeights$cv[r] <- stddev/mean
	
}



ddply(bootWeights, ~ X1, summarise, mean = mean (cv), sd = sd(cv))

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

########## 24th feb 2016
#from http://www.mayin.org/ajayshah/KB/R/documents/boot.html

require(boot)

x <- c(1,2,3,4,5,6,7,8,9,10)

samplemean <- function(x, d) {
	return(mean(x[d]))
}

samplesd <- function(x, d) {
	return(c(mean(x[d]), sd(x[d])))
}




b = boot(x, samplesd,  R=10)   

print(b$t[1,1])  # shows all samplemeans

plot(b)
