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



spidersBoot<- subset(spidersMul, Instar == "Adult", select = c(NestID, logLeg))

spidersBoot <- spidersBoot[complete.cases(spidersBoot), ]  # removing any NA's

colnames(spidersBoot)[2] <- "variable"  # changing name of variable of interest for function

spidersBootAve <- ddply(spidersBoot, .(NestID), summarise,
		N = length(variable),
		mean = mean(variable))
		

sampleSizes <- unique(spidersBootAve$N)

bootNests <- levels(spidersAdult$NestID)

maxVariable <- max(spidersBoot$variable)
minVariable <- min(spidersBoot$variable)


# SampleOutput <-  data.frame(x= numeric(0), y= integer(0), z = character(0)) # making empty data frame

iterations <- noBoots  * length(sampleSizes)

sampleSize <- 10


sampleFunction <- function(sampleSize, noBoots, minVariable, maxVariable, rowNumStart){
	
	for(b in 1:noBoots){
		print(b)
		rowNum <- rowNumStart + b
		rndSample <-runif(sampleSize, minVariable, maxVariable)  # random number generator
		mean <- mean(rndSample)
		COfV <- sd(rndSample)/ mean
		outputList <- c(sampleSize, mean, COfV)
		#print(outputList)
		output[rowNum,] <- outputList
		
	}
return(output)
	
}

noBoots <- 10

iterations <- noBoots  * length(sampleSizes)

output <- matrix(ncol=3, nrow=iterations) # must make empty matrix first



for(i in 1:length(sampleSizes)){
	sampleSizeInd <- sampleSizes[i]
	rowNumStart <- (i * noBoots) - noBoots
	output <- sampleFunction(sampleSizeInd, noBoots, minVariable, maxVariable, rowNumStart)	
	
	
}





output <- matrix(ncol=3, nrow=iterations*2) # must make empty matrix first

output <- sampleFunction(20, 10, minVariable, maxVariable )

outputDF <- data.frame(output)

names(outputDF) <- c("sampleSize", "mean", "COfV")

# from http://stackoverflow.com/questions/13442461/populating-a-data-frame-in-r-in-a-loop





for(i in 1:iterations){
	output[i,] <- runif(2)
	
}


rndSample <-runif(20, minVariable, maxVariable)  # random number generator

mean <- mean(rndSample)
COfV <- sd(rndSample)/ mean





sampleSize <- 8
nboots <- 100
bootWeights <- data.frame(matrix(NA, nrow = 0, ncol = (sampleSize +1)))


for(i in bootNests){
	
	print (i)	
	table <- spidersAdult[which (spidersAdult$NestID == i),]
	weights <- table$logWt
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


sample(x, size = 2)

b = boot(sample(x, size = 5), samplesd,  R=10)   

print(b$t)  # shows all samplemeans

plot(b)
