
# TODO: Add comment
# test
# Author: Ruth
###############################################################################
library (plyr)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(broom)
library(doParallel)
source("FilesExist.R")

cl <- makeCluster(2)
# Register cluster
registerDoParallel(cl)

min_popAge <-1 # the number of generations to discount from the start of the calculations

logisticFn = function(nnplus1){
	
	logistic <- nls(NPlus1 ~ I((N^(1+a)) * exp(b) * (exp(-c * N))) , data = nnplus1, start = list(a=0.4, b=1.5, c=0.02), 
			algorithm= "port", trace = T)
	return (logistic) #list(a=0.4, b=1.5, c=0.02)
}	

tryCatchLogistic= function(x) {
	tryCatch(logisticFn(x), warning = function(w) {print("warning")},
			error = function(e) {print("error")}) 
}


#graph making function
graphFunction <- function(folder, fileName, num_gens, min_popAge){
	
	print("START")
	print(fileName)
	
	#### REMEMBER: Before sending to grex remove the printing stuff.
	#filetoImport <- paste(fileName, ".py.csv", sep = "") #############################################################################################
	filetoImport <- paste(folder, fileName, ".py.csv", sep = "")
	
	mytheme = theme(text = element_text(size=16))
	
	
	pngTitle <- paste(folder, fileName, "_graph", ".png", sep = "")
	
	
	File <- read.csv(filetoImport, quote = "")
	maxPopAge <- max(File$pop_age)
	#print ("maxPop age")
	#print (maxPopAge)
	
	if(maxPopAge < (min_popAge + 50)){
		fn_min_popAge <- 0
		print("pop did not survive to sufficent generations")
	}else{
		fn_min_popAge <- min_popAge
		File <- subset(File, pop_age >= min_popAge) # removing the first x number of gens before do cals
	}
	
	
	
	File$AveOffAd <- File$numjuvs/File$num_ads
	File$AveOffAd[which(File$AveOffAd == Inf)] <- NA
	
	
	#cols<- as.numeric(levels(as.factor(File$colony_ID)))
	
	firstDisp <- ddply(subset(File, dispersers > 0), .(colony_ID), summarise, firstDisp = min(pop_age))
	## Updating dispersal information
	if(nrow(firstDisp) > 0){
		
		File <- merge(File, firstDisp, by = "colony_ID", all.x = TRUE)
		
		File$prevDisp <-  ifelse(File$firstDisp > File$pop_age | is.na(File$firstDisp), "n", "y" )
		
		File$prevDisp <- ifelse(File$dispersers > 0, "now", File$prevDisp)
	}else{
		File$prevDisp <- "n"
	}
	
	
	
	ColInfo <- data.frame(pop_age = File$pop_age, col_age = File$colony_age, col_id = File$colony_ID, 
			numAdsB4dis = File$num_adsB4_dispersal, dispersers = File$dispersers, prevDisp = File$prevDisp)
	ColInfo <- unique(ColInfo)
	
	ColInfo$colDisp <- 0
	
	ColInfo$colDisp[ColInfo$dispersers > 0 ] <- 1 # marking nests that had dispersers
	
	
	# graph variables
	
	print ("file name before first text grob")
	print (fileName)
	
	#textgrob
	
	mytitle = fileName
	
	pngHeight = 480 * (20 +1 )# 400 * number of graphs)
	
	png(pngTitle,  width = 1600, height = pngHeight, units = "px", pointsize = 16) # height = 400* num graphs
	
	
	ByPopAge<- ddply(subset(File, num_ads!=0), .(pop_age), summarise,
			NCols = length(!is.na(colony_ID)),
			TotNumInd = sum(num_ads),
			maxNumAds = max(num_ads),
			minNumAds = min(num_ads),
			meanNumAds = mean(num_ads)
	)
	
	
	
	File$pcntDisperse <- File$dispersers / File$num_adsB4_dispersal
	File$pcntDisperse[which(File$pcntDisperse == Inf)] <- NA
	
	File$pcntMoult <- File$num_juvs_moulting/File$numjuvs
	File$pcntMoult[which(File$pcntMoult == Inf)] <- NA
	
	interval <- ceiling(max(File$num_adsB4_dispersal)/200)
	
	#Histogram of colony sizes
	
	p0 <- ggplot(data = File, aes(x= num_adsB4_dispersal, fill = prevDisp)) + geom_histogram(binwidth = interval) +  mytheme + ggtitle("histogram of colony sizes")
	
	p00 <- ggplot(data = subset(File, num_ads > 1) , aes(x= num_adsB4_dispersal, fill = prevDisp)) + geom_histogram(binwidth = interval) +  
			mytheme + ggtitle("histogram of colony sizes with single nests removed")
	
	#pop age by total number of individuals
	p1 <- ggplot(data = ByPopAge, aes(x = pop_age, y = TotNumInd)) + geom_line() +  mytheme + ggtitle("total number of individuals in the population") 
	
	#pop age by number of colonies
	p2 <- ggplot(data = ByPopAge, aes(x = pop_age, y = NCols)) + geom_line() +  mytheme + ggtitle("total number of colonies in the population")
	
	#number of adults per nest
	p3 <- ggplot(data = ByPopAge, aes(x= pop_age, y = meanNumAds)) + geom_line() +  geom_line(aes(x=pop_age, y = maxNumAds), colour = "blue") +
			geom_line(aes(x=pop_age, y = minNumAds), colour = "red")+ mytheme + ggtitle("Mean, max and min num ads in nest")
	
	#average age
	#p4 <- ggplot(data = ByPopAgeAndCol, aes(x= factorAge, y = colony_age)) + geom_point(size = 1) +  mytheme + ggtitle("colony age by population age")
	
	p4 <- ggplot(data = File, aes(x= colony_age, y = num_ads, colour = prevDisp )) + geom_point() + mytheme + ggtitle("colony size by colony age")
	
	# next size vs dispersers
	p5 <- ggplot(data = File, aes(x= num_adsB4_dispersal, y = dispersers)) + geom_point() +  mytheme + stat_smooth(se=FALSE) + 
			scale_y_continuous(limits = c(0, NA)) + ggtitle("nest size before dispersal vs number of dispersers")
	
	p6 <- ggplot(data = File, aes(x= num_adsB4_dispersal, y = pcntDisperse)) + geom_point() + stat_smooth(se = FALSE) + mytheme +
			scale_y_continuous(limits = c(0, 1)) + ggtitle("percentage of ads dispersing")
	
	maxcolsize <- max(ColInfo$numAdsB4dis)
	
	p6a <-ggplot(data = subset(File, prevDisp == "now"), aes(x = num_adsB4_dispersal, y = num_ads, colour = prevDisp)) + geom_point() + mytheme +
			ylim(0, maxcolsize) + xlim(0, maxcolsize) + ggtitle("num ads before disperal against num ads after dispersal")
	
	
	File$foodPerAd <- File$colony_food / File$num_ads
	File$foodPerAd[which(File$foodPerAd == Inf)] <- NA
	
	File$foodPerJuv <- File$colony_food / File$numjuvs
	File$foodPerJuv[which(File$foodPerJuv == Inf)] <- NA
	
	
	# Nest size before dispersal vs food per adult 
	#### OK THIS DOESN'T MAKE ANY SENSE. SHOULD BE POTENTIAL FOOD IF NO INDIVIDUAL DISPERSED
	p7 <- ggplot(data = File, aes(x=num_adsB4_dispersal, y = foodPerAd, colour = prevDisp)) + geom_point() + stat_smooth(se = FALSE) +  
			mytheme + scale_y_continuous(limits = c(0, NA)) + ggtitle("colony food per capita before dispersal x=num_adsB4_dispersal, y = colony_food/num_ads")
	
	#Nest size after dispersal with food per adult
	p8 <- ggplot(data = File, aes(x=num_ads, y = foodPerAd, colour = prevDisp)) + geom_point() + stat_smooth(se = FALSE) +  
			mytheme + scale_y_continuous(limits = c(0, NA)) + ggtitle("num ads after dispersal vs per capita food")
	
	p8a <- ggplot(data = File, aes(x=num_ads, y = foodPerJuv, colour = prevDisp)) + geom_point() + stat_smooth(se = FALSE) +  
			mytheme + scale_y_continuous(limits = c(0, NA)) + ggtitle("Food Per Juv")
	
	
	p9 <- ggplot(data = File, aes(x=num_ads, y = AveOffAd, colour = prevDisp )) + geom_point() + stat_smooth(se = FALSE) +  
			mytheme + scale_y_continuous(limits = c(0, NA)) + ggtitle("ave offspring per adults vs col size after dispersal")
	
	
	
	# number juvs moulting with size after dispersal
	p11 <- ggplot(data = File, aes(x=num_ads, y = pcntMoult, colour = prevDisp)) + stat_smooth(se = FALSE)  + geom_point() +  mytheme +
			scale_y_continuous(limits = c(0, 1)) + ggtitle("percentage juvs moults vs. num ads after dispersal")
	
	# percentage of adults dispersing by nest size
	
	## AGAIN I DON'T THINK THIS MAKES SENSE 
	p12 <- ggplot(data = File, aes(x=num_ads, y = ave_food, colour = prevDisp)) + geom_point() +
			mytheme + scale_y_continuous(limits = c(0, 1)) + stat_smooth(se = FALSE) + ggtitle("ave food per adult")
	
	rm(ByPopAge)
	
	
	deadcols <- subset(File, colAlive== 'dead')
	
	if (nrow(deadcols) == 0){
		df <- data.frame()
		p13<-ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 100) + ggtitle("NO COLONIES DIED!!")
		print("no colonies died")
	}else{
		# histogram of size of dead colonies
		p13 <- ggplot(data = subset(File, colAlive== 'dead'), aes(x= num_ads, fill = prevDisp )) + geom_histogram() + mytheme + ggtitle("histogram of size of colonies when died (num ads after dispersal)")
	}
	rm(deadcols)
	
	
	
	
	#### Ind Files Graphs
	
	
	
	AdsByPopAgeAndCol <- melt(File, id.vars = c("colony_ID", "colony_age", "num_adsB4_dispersal"), 
			measure.vars = c("adSz_B4_min", "adSz_B4_max", "adSz_B4_mean"), variable.name="MinMax", value.name="AdSize")
	
	# colony size by adult size
	p16 <- ggplot(data = AdsByPopAgeAndCol, aes(x= num_adsB4_dispersal, y = AdSize, colour = MinMax)) + geom_point() +
			stat_smooth(se = FALSE) + mytheme + scale_y_continuous(limits = c(0, 1)) + ggtitle("ad size vs col size b4 dispersal")
	
	rm(AdsByPopAgeAndCol)
	
	
	JuvsByPopAgeAndCol <- melt(File, id.vars = c("colony_ID", "colony_age", "num_adsB4_dispersal"), 
			measure.vars = c("jvSz_B4_min", "jvSz_B4_max", "jvSz_B4_mean"), variable.name="MinMax", value.name="JuvSize")
	
	# Colony size by number of juvs
	p17 <- ggplot(data = JuvsByPopAgeAndCol, aes(x= num_adsB4_dispersal, y = JuvSize, colour = MinMax)) + geom_point() +
			stat_smooth(se = FALSE) + mytheme + scale_y_continuous(limits = c(0, 1)) + ggtitle("juv size vs ads before dispersal")
	
	
	rm(JuvsByPopAgeAndCol)		
	
	
	rm(File)
	
	########## Making n n+1 graphs
	
	#nnplus1 <- data.frame(col_id=numeric(), pop_age = numeric(),  N=numeric(), NPlus1=numeric(), disp = numeric(), prevDisp = character(),
	#stringsAsFactors=FALSE) # creating empty data frame
	
	
	#print(length(cols))
	ColInfo$ColAgePlus1 <- ColInfo$col_age
	
	ColInfoPlus1 <- subset(ColInfo, select = c("col_id", "numAdsB4dis", "col_age"))
	
	colnames(ColInfoPlus1)[2] <- "NPlus1"
	
	ColInfoPlus1$ColAgePlus1 <- ColInfoPlus1$col_age - 1
	
	nnplus1 <- merge(ColInfo, ColInfoPlus1,  by =c("ColAgePlus1", "col_id"))
	
	colnames(nnplus1)[5] <- "N"
	
	
	
	nnplus1 <- subset(nnplus1, prevDisp != "now")  # removing colonies that have just dispersed
	
	
	nnplus1 <- subset(nnplus1, pop_age < num_gens - 1 ) # removing nests at the end of generations that might not have died
	
	nnplus1$AveGrowth <- (nnplus1$NPlus1 - nnplus1$N) / nnplus1$N	
	
	
	
	nnplus1$AveGrowth <- (nnplus1$NPlus1 - nnplus1$N) / nnplus1$N
	
	#ave growth rate per ind
	p13a <- ggplot(data = nnplus1, aes(x  = N, y = AveGrowth)) + geom_point(aes(colour = prevDisp)) + stat_smooth() + mytheme + ggtitle("ave growth rate per ind nnplus1-n/n") + stat_smooth(se = FALSE)
	
	
	
	######## calculating logistic equation with all nests
	
	
	logistic <- tryCatchLogistic(nnplus1)
	
	options(warn = -1) #suppress warnings globally
	
	if (logistic =="error"){
		print ("error in logistic calculation") 
		
		p14 <- ggplot(data = nnplus1, aes(x= N, y = NPlus1)) + geom_point() + 
				geom_abline(intercept = 0, slope = 1 ) + scale_y_continuous(limits = c(0, NA)) + scale_x_continuous(limits = c(0, NA))+
				ggtitle("logistic eqn did not work :-(")
		
		
	} else {
		print ("logistic equation did work!")
		
		nnplus1$logisticPredict <- predict(logistic)
		
		p14 <- ggplot(data = nnplus1, aes(x= N, y = NPlus1)) + geom_point() + 
				geom_abline(intercept = 0, slope = 1 ) + scale_y_continuous(limits = c(0, NA)) + scale_x_continuous(limits = c(0, NA)) +
				geom_line(aes(x = N, y = logisticPredict), colour = 'blue') + ggtitle("logistic eqn with all nests")
		
		
	}
	
	
	
	######## logisitc equation without single nests
	
	
	nnplus1 <- subset(nnplus1, N >1)
	
	logistic <- tryCatchLogistic(nnplus1)
	
	options(warn = -1) #suppress warnings globally
	
	if (logistic =="error"){
		print ("error in logistic calculation") 
		
		p15 <- ggplot(data = nnplus1, aes(x= N, y = NPlus1)) + geom_point() + 
				geom_abline(intercept = 0, slope = 1 ) + scale_y_continuous(limits = c(0, NA)) + scale_x_continuous(limits = c(0, NA))+
				ggtitle("logistic eqn without single nests did not work :-(")
		
		
	} else {
		print ("logistic equation did work!")
		
		nnplus1$logisticPredict <- predict(logistic)
		
		p15 <- ggplot(data = nnplus1, aes(x= N, y = NPlus1)) + geom_point() + 
				geom_abline(intercept = 0, slope = 1 ) + scale_y_continuous(limits = c(0, NA)) + scale_x_continuous(limits = c(0, NA)) +
				geom_line(aes(x = N, y = logisticPredict), colour = 'blue') + ggtitle("logistic eqn without single nests")
		
		
	}
	
	
	options(warn = 0) # turns warnings back on
	
	
	
	
	rm(nnplus1)
	
	
	h1 <- 2/5
	h2 <- 3/5
	
	p_grob <- arrangeGrob(p14,p15, ncol=2)
	print(grid.arrange(p0, p00, p1, p2, p3,  p4, p5, p6, p6a, p7, p8, p8a, p9, p11, p12, p13, p13a,  p_grob,
					p16, p17, ncol = 1, heights = c(h1, h1, h1, h1, h1, h1, h1, h1, h1, h1, h1, h1, h1, h1, h1, h1, h1, h2, h1, h1), top = mytitle))
	
	
	#main = mytitle))
	
	dev.off()
	
	
}



#folder <- "R_Graphs/" ####################################################################################################################
folder <- "DisperalSimulationOutput/"

fileNames<-read.csv(paste(folder, "FilesCreated.csv", sep = ""), quote="") # import file names csv file

fileNames[] <- lapply(fileNames, as.character) # making factors into strings


files <- fileExistsFn(fileNames)

#files <- sample(files) # if I want to randomize the order that they are done, i.e. if there are too many!


numFiles<- length(files)

print ("the number of files that exist:")
print (numFiles)

loop <- foreach(i=1:numFiles, .errorhandling='remove', .packages= c("ggplot2", "plyr", "gridExtra", "reshape2", "broom")) %dopar%{
	
#for (i in 1:numFiles){ # for testing, if you just do the parrallel loop thing errors don't show
	
	num <- files[i]
	
	theFileName <-fileNames[num,1]
	print ("file name in loop")                                        
	print (theFileName)
	num_gens <- as.numeric(fileNames[num, 3])
	output <- graphFunction(folder, theFileName, num_gens, min_popAge)
	
}

stopCluster(cl)

