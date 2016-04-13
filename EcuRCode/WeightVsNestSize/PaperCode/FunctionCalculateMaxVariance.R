# Author: Ruth
###############################################################################

library(plyr)

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

## put in check that no max sd is larger than data sd

#instarInput <- "Adult"


spidersBoot <- subset(spidersMul, select = c(NestID, Instar, logLeg))

#spidersBoot <- subset(spidersMul, Instar == instarInput & NestID == "28.8EXa03", select = c(NestID, logLeg))

spidersBoot <- spidersBoot[complete.cases(spidersBoot), ]  # removing any NA's

colnames(spidersBoot)[3] <- "variable"  # changing name of variable of interest for function

spidersBoot$variable <- spidersBoot$variable + 1

spidersMinMaxVar <- ddply(spidersBoot, .(Instar), summarise,
		max_variable = max(variable),
		min_variable = min(variable)
)


spidersBootAve <- ddply(spidersBoot, .(NestID, Instar), summarise,
		N = length(variable),
		mean = mean(variable),
		sd_data = sd(variable)
)

spidersBootAve <- merge(spidersBootAve, spidersMinMaxVar, by = c("Instar"))

spidersBootAve <- spidersBootAve[complete.cases(spidersBootAve), ]


spidersBootAve$mean_list <- as.numeric(NA)
spidersBootAve$sd_Max <- as.numeric(NA)

numNests <- nrow(spidersBootAve)

for(nest in 1:numNests){
	
	print(nest)
	data_mean <- spidersBootAve$mean[nest]
	sampSize <- spidersBootAve$N[nest]
	minVariable <- spidersBootAve$min_variable[nest]
	maxVariable <- spidersBootAve$max_variable[nest]	
	maxVarList<- calMaxVarFun(data_mean, sampSize, minVariable,  maxVariable )  # mean, sampleSize, minVar, maxVar
	print(maxVarList)
	mean_list <- mean(maxVarList)
	sd_max <- sd(maxVarList)
	spidersBootAve[nest, 8] <- mean_list
	spidersBootAve[nest, 9] <- sd_max
}
	
spidersBootAve$relativeVar <- spidersBootAve$sd_data/spidersBootAve$sd_Max

out <- calMaxVarFun(mean, sampSize, maxVariable, minVariable) 
out <- calMaxVarFun(0.449, 18, 0.41, 0.557)   # mean, sampleSize, minVar, maxVar
out

mean(out)
sd(out)




