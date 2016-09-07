

###############################################################

############ Making a more specific sample ######

numNests <-200

sizeDistNests <- (2:20)
#measures <- seq(10, 30, by =  0.05)

ID_list <- c()
variable <- c()
multpBy_list <- seq(1, 10, by = 0.005)
repsize<- rep(sizeDistNests, 20000)

for (i in seq(1:numNests)) {
	
	# Changing mean
	multpBy <- sample(multpBy_list, 1)
	multpBy <- 1
	measures <- seq(10* multpBy, 20 *multpBy, by =  0.005)
	
	ID <- i
	nestSize <- repsize[i]
	randSamp <- sample(measures, nestSize)
	IDVect <- rep(i, nestSize)
	ID_list <- c(ID_list, IDVect)
	variable <- c(variable, randSamp)
	
	
}

spidersBoot <- data.frame(ID_list, variable)


################################################################
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

#spidersBootAve$one <- as.numeric(NA)
#spidersBootAve$two <- as.numeric(NA)
#spidersBootAve$three <- as.numeric(NA)
#spidersBootAve$four <- as.numeric(NA)
#spidersBootAve$five <- as.numeric(NA)
#spidersBootAve$six <- as.numeric(NA)
#spidersBootAve$seven <- as.numeric(NA)
#spidersBootAve$eight <- as.numeric(NA)
#spidersBootAve$nine <- as.numeric(NA)
#spidersBootAve$ten <- as.numeric(NA)


	
numNests <- nrow(spidersBootAve)
	
	for(nest in 1:numNests){
		
		print(nest)
		data_mean <- spidersBootAve$mean[nest]
		sampSize <- spidersBootAve$N[nest]
		minVariable <- spidersBootAve$minVar[nest]
		maxVariable <- spidersBootAve$maxVar[nest]	
		maxVarList<- calMaxVarFun(data_mean, sampSize, minVariable,  maxVariable )  # mean, sampleSize, minVar, maxVar
		mean_Maxlist <- mean(maxVarList)
		sd_max <- sd(maxVarList)
		spidersBootAve[nest, 7] <- mean_Maxlist
		spidersBootAve[nest, 8] <- sd_max
		#spidersBootAve[nest, 9:18] <- maxVarList
	}
	
	spidersBootAve <- mutate(spidersBootAve, relativeVar =sd_data/sd_Max)
	spidersBootAve <- mutate(spidersBootAve, CofV =sd_data/mean)
	spidersBootAve <- mutate(spidersBootAve, CofVSamp =(1 + (1/ (4*sampSize))   * CofV))
	spidersBootAve <- mutate(spidersBootAve, Diff =sd_data - relativeVar)
	
	#print (max(spidersBootAve$relativeVar) )
	
#if(max(spidersBootAve$relativeVar) >= 1) { warning("something went wrong - real variance more than calculated max variance") }

## Note: the coefficient of variaion changes a lot with mean

ggplot(spidersBootAve, aes(x = sd_data)) + geom_histogram()
ggplot(spidersBootAve, aes(x = relativeVar)) + geom_histogram()
ggplot(spidersBootAve, aes(x = mean)) + geom_histogram()



# Graphs by N
ggplot(spidersBootAve, aes(x = N, y = sd_data)) + geom_point() + geom_smooth()

ggplot(spidersBootAve, aes(x = N, y = CofV)) + geom_point() + geom_smooth()

ggplot(spidersBootAve, aes(x = N, y = CofVSamp)) + geom_point() + geom_smooth()

ggplot(spidersBootAve, aes(x = N, y = relativeVar)) + geom_point() + geom_smooth()

ggplot(spidersBootAve, aes(x = N, y = mean)) + geom_point() + geom_smooth()

ggplot(spidersBootAve, aes(x = N, y = Diff)) + geom_point() + geom_smooth()

#Graphs by Mean

ggplot(spidersBootAve, aes(x = mean, y = sd_data)) + geom_point() + geom_smooth()

ggplot(spidersBootAve, aes(x = mean, y = CofV)) + geom_point() + geom_smooth()

ggplot(spidersBootAve, aes(x = mean, y = relativeVar)) + geom_point() + geom_smooth()

ggplot(spidersBootAve, aes(x = mean, y = sd_Max)) + geom_point() + geom_point(aes(x = mean, y = sd_data)) + geom_smooth()


ggplot(spidersBootAve, aes(x = mean, y = sd_Max/mean)) + geom_point()

ggplot(spidersBootAve, aes(x = mean, y =sd_Max))+ geom_point() + geom_smooth()

ggplot(spidersBootAve, aes(x = mean, y = Diff)) + geom_point() + geom_smooth()

# Others
ggplot(spidersBootAve, aes(x = sd_data, y = CofV)) + geom_point() + geom_smooth()

ggplot(spidersBootAve, aes(x = sd_data, y = relativeVar)) + geom_point() + geom_smooth()



ggplot(spidersBootAve, aes(x = CofV, y = relativeVar)) + geom_point() + geom_smooth()

ggplot(spidersBootAve, aes(x = N, y = sd_Max)) + geom_point() + geom_smooth()

ggplot(spidersBootAve, aes(x = N, y = mean)) + geom_point() + geom_smooth()





AveofAve<- ddply(spidersBootAve, .(N), summarise,
		NumSamps = length(N),
		sd_of_mean = sd(mean),
		sd_of_sd = sd(sd_data)
)

ggplot(AveofAve, aes(x = N, y = NumSamps)) + geom_point() + geom_smooth()

ggplot(AveofAve, aes(x = N, y = sd_of_mean)) + geom_point() + geom_smooth()

ggplot(AveofAve, aes(x = N, y = sd_of_sd)) + geom_point() + geom_smooth()

 



write.csv(spidersBootAve, 'spidersBootAve.csv')


########## Old crapper sample method
# Author: Ruth
###############################################################################

num_IDs <- 500


ID <- 1:num_IDs

prob_list <- seq(0, 1, by = (1/(num_IDs - 1)))

N_tot <- 3500

ID_list <- sample(ID, N_tot, replace = TRUE, prob = prob_list)

measures <- seq(10, 30, by =  0.05)

variable <- sample(measures, N_tot/2, replace = TRUE)

#sample2 different mean

ID <- (num_IDs+1):(num_IDs*2)

ID_list2 <- sample(ID, N_tot, replace = TRUE, prob = prob_list)

measures <- seq(0, 20, by =  0.05)

variable2 <- sample(measures, N_tot/2, replace = TRUE)

ID_list <- c(ID_list, ID_list2)

variable <- c(variable, variable2)

ggplot(spiders, aes(x = logWt)) + geom_histogram() + facet_wrap(~Instar)



mean(spidersBootAve$relativeVar, na.rm = TRUE)

filtered <- filter(spidersBootAve, mean > 14.990 & mean < 15.001)

ggplot(filtered, aes(x = exp(sd_data))) + geom_histogram()

ggplot(filtered, aes(x = asin(sqrt(relativeVar)))) + geom_histogram()


########## Getting cumulative frequency distribution

nestSD <- spidersBootAve$sd_data

min_sd <- min(spidersBootAve$sd_data)
max_sd <- max(spidersBootAve$sd_data)

breaks = seq(min_sd, max_sd, by = 0.5)

duration.cut = cut(nestSD, breaks, right=FALSE) 
duration.freq = table(duration.cut)
duration.cumfreq = cumsum(duration.freq)

cbind(duration.cumfreq)


#########
library(SciencesPo)
library(stringr)



a_test <- freq(spidersBootAve$sd_data)

a_freqBins <- as.character(a_test$class)



str_replace("don[", "[[]", "t")

a_freqBins<- gsub("]", "", a_freqBins)
a_freqBins<- gsub("\\(", "", a_freqBins)
a_freqBins <- gsub("\\[", "", a_freqBins)

#a_test<- 
a_freqBins <- str_split(a_freqBins, ",")

a_table <- read.table(text = a_freqBins, sep = ",",  colClasses = "character", col.names = c("name", "name2"))

an_table <- cbind(a_table, a_test)


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


df <- data.frame(mean=numeric(), SD = numeric())

i <- 0

while (i <= 100) {
	i <- i + 1
	output <- makeDistInputMean(20, 10, 0, 100)
	randMean <- mean(output)
	randSD <- sd(output)
	df <- rbind(df, c(randMean, randSD))
}

names(df)[1] <- "mean"
names(df)[2] <- "SD"

require(reshape)
library(SciencesPo)
library(stringr)
library(qdap)
freqDist <- freq(df$SD)

freqDist$class <- as.character(freqDist$class)

freqDist$class <- gsub("]", "", freqDist$class)
freqDist$class<- gsub("\\(", "", freqDist$class)
freqDist$class <- gsub("\\[", "", freqDist$class)



dfTest = transform(freqDist, colsplit(class, split = ",", names = c('a', 'b')))
colsplit(freqDist$class, split = ",", names = c('a', 'b'))

colsplit2df(dataframe, splitcol = 1, new.names = NULL,
		sep = ".", keep.orig = FALSE)


freqDist <- within(freqDist, class <-data.frame(do.call('rbind', strsplit(as.character(class), ',', fixed=TRUE))))

names(freqDist)[1] <- "lowLimit"

SD_ecdf <- ecdf(df$SD)

plot(SD_ecdf)

#a_test<- 
a_freqBins <- str_split(a_freqBins, ",")

a_table <- read.table(text = a_freqBins, sep = ",",  colClasses = "character", col.names = c("name", "name2"))

an_table <- cbind(a_table, a_test)
cbind(duration.cumfreq)

