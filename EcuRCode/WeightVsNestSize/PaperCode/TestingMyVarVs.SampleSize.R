# Author: Ruth
###############################################################################

num_IDs <- 300

ID <- 1:num_IDs

prob_list <- seq(0, 1, by = (1/(num_IDs - 1)))

N_tot <- 20000

ID_list <- sample(ID, N_tot, replace = TRUE, prob = prob_list)

measures <- seq(0, 20, by =  0.05)

variable <- sample(measures, N_tot, replace = TRUE)

spidersBoot <- data.frame(ID_list, variable)





spidersBoot <- ddply(spidersBoot, "ID_list", transform) # calculating max and min

spidersBoot$minVar <- min(spidersBoot$variable)
spidersBoot$maxVar <- max(spidersBoot$variable)
	
spidersBootAve <- ddply(spidersBoot, .(ID_list,  minVar, maxVar), summarise,
			N = length(variable),
			mean = mean(variable),
			sd_data = sd(variable)
	)
	
spidersBootAve$mean_list <- as.numeric(NA)
spidersBootAve$sd_Max <- as.numeric(NA)
	
numNests <- nrow(spidersBootAve)
	
	for(nest in 1:numNests){
		
		print(nest)
		data_mean <- spidersBootAve$mean[nest]
		sampSize <- spidersBootAve$N[nest]
		minVariable <- spidersBootAve$minVar[nest]
		maxVariable <- spidersBootAve$maxVar[nest]	
		maxVarList<- calMaxVarFun(data_mean, sampSize, minVariable,  maxVariable )  # mean, sampleSize, minVar, maxVar
		mean_list <- mean(maxVarList)
		sd_max <- sd(maxVarList)
		spidersBootAve[nest, 7] <- mean_list
		spidersBootAve[nest, 8] <- sd_max
	}
	
	spidersBootAve <- mutate(spidersBootAve, relativeVar =sd_data/sd_Max)
	
	#print (max(spidersBootAve$relativeVar) )
	
#if(max(spidersBootAve$relativeVar) >= 1) { warning("something went wrong - real variance more than calculated max variance") }


ggplot(spidersBootAve, aes(x = N, y = sd_data)) + geom_point() + geom_smooth()

ggplot(spidersBootAve, aes(x = N, y = relativeVar/mean)) + geom_point() + geom_smooth()

ggplot(spidersBootAve, aes(x = mean, y = sd_max)) + geom_point() + geom_smooth()

