# Author: Ruth
###############################################################################


library("plyr")


noBoots <- 50000
decPlaces <- 10
instarInput <- "Adult"

		
FileSubset <- subset(spidersMul, Instar == instarInput, select = c(NestID, logLeg))

maxVariable <- max(FileSubset$logLeg)
minVariable <- min(FileSubset$logLeg)

FileSubset <- subset(spidersMul, Instar == instarInput & NestID == "28.8EXa03", select = c(NestID, logLeg))



sampleFunction <- function(sampleSize, noBoots, minVariable, maxVariable, rowNumStart, output){
	# output is an empty data frame
	for(b in 1:noBoots){
		
		rowNum <- rowNumStart + b
		rndSample <-runif(sampleSize, minVariable, maxVariable)  # random number generator
		mean <- mean(rndSample)
		COfV <- sd(rndSample)#/ mean
		outputList <- c(sampleSize, mean, COfV)
		#print(outputList)
		output[rowNum,] <- outputList
		
	}
	return(output)
	
}


relativeVarFun <- function(noBoots, decPlaces, spidersBoot){


	spidersBoot <- spidersBoot[complete.cases(spidersBoot), ]  # removing any NA's

	colnames(spidersBoot)[2] <- "variable"  # changing name of variable of interest for function

	spidersBootAve <- ddply(spidersBoot, .(NestID), summarise,
			N = length(variable),
			mean = mean(variable))


	sampleSizes <- unique(spidersBootAve$N)  # list of unique sample sizes


	maxVariable# <- max(spidersBoot$variable)
	minVariable# <- min(spidersBoot$variable)


	iterations <- noBoots  * length(sampleSizes)


	output <- matrix(ncol=3, nrow=iterations) # must make empty matrix first



	for(i in 1:length(sampleSizes)){
		
		#print(i)
		
		print("rowNumStart")
		
		sampleSizeInd <- sampleSizes[i]

		rowNumStart <- (i * noBoots) - noBoots
		print(rowNumStart)
		output <- sampleFunction(sampleSizeInd, noBoots, minVariable, maxVariable, rowNumStart, output)	
		
	
	}


	outputDF <- data.frame(output)

	names(outputDF) <- c("sampleSize", "mean", "COfV")

	### getting max variance for sample size and mean
	
	spidersBootAve[,'mean']=round(spidersBootAve[,'mean'], decPlaces )  # rounding decimal places
	outputDF[,'mean']=round(outputDF[,'mean'], decPlaces )  # rounding decimal places

	spidersBootAve$maxVar <- as.numeric(NA)
	spidersBootAve$NumMatch <- as.numeric(NA)
	
	outputDF$sampleSize <- as.integer(outputDF$sampleSize)


	numNests <- nrow(spidersBootAve)

	for(nest in 1:numNests){
		print(nest)
		meanOrg <- spidersBootAve$mean[nest]
		smpSzeOrg <- spidersBootAve$N[nest]
		subTab <- subset(outputDF, mean == meanOrg & sampleSize == smpSzeOrg)
		print("length of sub table")
		subTabLen <- nrow(subTab)
		maxCOfV <- max(subTab$COfV) # max coefficient of variation
		spidersBootAve$maxVar[nest] <- maxCOfV
		spidersBootAve$NumMatch[nest] <- subTabLen
		
	}
	
	return(outputDF) #(spidersBootAve)
	
}



SpiderVarOutput <- relativeVarFun(noBoots, decPlaces, FileSubset)

#write.csv(SpiderVarOutput, file = "RuthEcuador2013/NestSize/MaxVariance/Adults_logLeg.csv")



#check <- subset(spidersMul, NestID == "28.8EXa03" & Instar == "Adult")

ggplot(SpiderVarOutput, aes(x = mean, y = COfV)) + geom_point()