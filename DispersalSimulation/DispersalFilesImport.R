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

cl <- makeCluster(2)
# Register cluster
registerDoParallel(cl, outfile = "")

#graph making function
graphFunction <- function(folder, fileName, num_gens, min_popAge){
	
	print("START")
	print(fileName)

	#### REMEMBER: Before sending to grex remove the printing stuff.
	#filetoImport <- paste(fileName, ".py.csv", sep = "")
	#indFileToImport <- paste(fileName, ".py_inds.csv", sep = "")
	filetoImport <- paste(folder, fileName, ".py.csv", sep = "")
	indFileToImport <- paste(folder, fileName, ".py_inds.csv", sep = "")
	mytheme = theme(text = element_text(size=16))

	
	pngTitle <- paste(folder, fileName, "_graph", ".png", sep = "")
	#print ("overall file exists?")
	#print(file.exists(filetoImport))
	#print ("ind file exists?")
	#print(file.exists(indFileToImport))

	File <- read.csv(filetoImport, quote = "")
	maxPopAge <- max(File$pop_age)
	#print ("maxPop age")
	#print (maxPopAge)
	
	if(maxPopAge < 300){
		fn_min_popAge <- 0
		print("pop did not survive to 400 generations")
	}else{
		fn_min_popAge <- min_popAge
		File <- subset(File, pop_age >= min_popAge) # removing the first x number of gens before do cals
	}
	
	
	
	
	File$AveOffAd <- File$numjuvs/File$num_ads
	File$AveOffAd[which(File$AveOffAd == Inf)] <- NA
	
	
	File$prevDisp <- "n"
	
	cols<- as.numeric(levels(as.factor(File$colony_ID)))
	for (i in 1:length(cols)){
		
		thisCol <- cols[i]
		#print(thisCol)

		age_FstDisp <- File$colony_age[which(File$dispersers > 0 & File$colony_ID == thisCol)]
		if (length(age_FstDisp) > 0){
			min_ageFstDisp <- min(age_FstDisp)
			#print (min_ageFstDisp)
			File$prevDisp[which(File$colony_age >= min_ageFstDisp & File$colony_ID == thisCol)] <- "y"
			
		}else{
			#print("no dispersers")
		}
	}
	
	File$prevDisp[File$dispersers >0] <- "now"
	
	ColInfo <- data.frame(pop_age = File$pop_age, col_age = File$colony_age, col_id = File$colony_ID, 
			numAdsB4dis = File$num_adsB4_dispersal, dispersers = File$dispersers, prevDisp = File$prevDisp)
	ColInfo <- unique(ColInfo)
	
	ColInfo$colDisp <- 0
	
	ColInfo$colDisp[ColInfo$dispersers > 0 ] <- 1 # marking nests that had dispersers
	
	
	# graph variables
	
	mytitle = textGrob(label = fileName)
	
	pngHeight = 450 * (23 +1 )# 400 * number of graphs)
	
	png(pngTitle,  width = 1600, height = pngHeight, units = "px", pointsize = 16) # height = 400* num graphs
	
	#print("png title")
	#print (pngTitle)
	
	DF <- data.frame(Comp = File$Comp_slope[1], disp_rsk = File$disp_rsk[1], var = File$input_var[1], 
			meanK = File$meanK[1], dis_size = File$ad_dsp_fd[1], pop_age = max(File$pop_age), disperse = max(ColInfo$colDisp))#, fileName = character(0))
	
	
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

######TO DO!!#########################
#http://docs.ggplot2.org/current/geom_histogram.html 
#Colour the graph as to whether the nest ever dispersed or not - need to do some stuff to the data to update whether the nest ever dispersed
	
	# for each colony find min col age when dispersal first takes place, mark the colony after that
	



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
	
	# Nest size before dispersal vs food per adult 
#### OK THIS DOESN'T MAKE ANY SENSE. SHOULD BE POTENTIAL FOOD IF NO INDIVIDUAL DISPERSED
	p7 <- ggplot(data = File, aes(x=num_adsB4_dispersal, y = foodPerAd, colour = prevDisp)) + geom_point() + stat_smooth(se = FALSE) +  
			mytheme + scale_y_continuous(limits = c(0, NA)) + ggtitle("colony food per capita before dispersal x=num_adsB4_dispersal, y = colony_food/num_ads")
	
	#Nest size after dispersal with food per adult
	p8 <- ggplot(data = File, aes(x=num_ads, y = foodPerAd, colour = prevDisp)) + geom_point() + stat_smooth(se = FALSE) +  
			mytheme + scale_y_continuous(limits = c(0, NA)) + ggtitle("num ads after dispersal vs per capita food")
	
	
	p9 <- ggplot(data = File, aes(x=num_ads, y = AveOffAd, colour = prevDisp )) + geom_point() + stat_smooth(se = FALSE) +  
			mytheme + scale_y_continuous(limits = c(0, NA)) + ggtitle("ave offspring per adults vs col size after dispersal")
	
	p10 <- ggplot(data = File, aes(x=num_adsB4_dispersal, y = AveOffAd, colour = prevDisp )) + geom_point() + stat_smooth(se = FALSE) +  
			mytheme + scale_y_continuous(limits = c(0, NA)) + ggtitle("ave offspring per adults vs col size BEFORE dispersal")
	
	
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
	rm(File)
	
	## Making n, n+1 graph
	nnplus1 <- data.frame(col_id=numeric(), pop_age = numeric(),  N=numeric(), NPlus1=numeric(), disp = numeric(), prevDisp = character(),
			stringsAsFactors=FALSE) # creating empty data frame

	counter <- 0
	
	cols <- as.numeric(levels(as.factor(ColInfo$col_id)))
	
	#print(length(cols))
	
	for (i in 1:length(cols)){
		
		colony <- cols[i]
		#print ("colony")
		#print(colony)
		
		col_subset <- subset(ColInfo, col_id == colony)
		
				
		maxcol_age <- max(col_subset$col_age)
		mincol_age <- min(col_subset$col_age)
	
		for (age in mincol_age:maxcol_age){
			#print("age")
			#print(age)
			counter <- counter + 1
			
			if (col_subset$pop_age[which(col_subset$col_age == age)]){
		
				nnplus1[counter,1] <- colony # [row number, col num]
				nnplus1[counter,2] <- col_subset$pop_age[which(col_subset$col_age == age)]
				nnplus1[counter,3] <- col_subset$numAdsB4dis[which(col_subset$col_age == age)]
				nnplus1[counter,5] <- col_subset$dispersers[which(col_subset$col_age == age)]	
				nnplus1[counter,6] <- as.character(col_subset$prevDisp[which(col_subset$col_age == age)])
				
				
				if (age == maxcol_age){ # setting 
					nnplus1[counter,4] <- 0 
				}else{
					nnplus1[counter,4] <- col_subset$numAdsB4dis[which(col_subset$col_age == (age +1))]	
				}
			
				

		
			}
		}
	}
	
	
	
	
	nnplus1 <- subset(nnplus1, pop_age < num_gens ) # removing nests at the end of generations that might not have died
	
	nnplus1$AveGrowth <- (nnplus1$NPlus1 - nnplus1$N) / nnplus1$N
	
	#ave growth rate per ind
	p13a <- ggplot(data = nnplus1, aes(x  = N, y = AveGrowth)) + geom_point(aes(colour = prevDisp)) + stat_smooth() + mytheme + ggtitle("ave growth rate per ind nnplus1-n/n") + stat_smooth(se = FALSE)
	
	nnplus1 <- subset(nnplus1, disp == 0)  # removes colonies that had dispersed as the numbers will be wrong
	
	######## calculating logistic equation
	
	logisticFn = function(nnplus1){
	
		logistic <- nls(NPlus1 ~ I((N^(1+a)) * exp(b) * (exp(-c * N))) , data = nnplus1, start = list(a=0.4, b=1.5, c=0.02), 
			algorithm= "port", trace = T)
		return (logistic) #list(a=0.4, b=1.5, c=0.02)
	}	
	
	tryCatchLogistic= function(x) {
		tryCatch(logisticFn(x), warning = function(w) {print("warning")},
				error = function(e) {print("error")}) 
	}
	
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
				geom_line(aes(x = N, y = logisticPredict), colour = 'blue') + ggtitle("logistic eqn")
		
		logisticTable <- tidy(logistic)
		logisticTable$type <- "logistic"		
		
	}
	

	
	######## Calculating ricker equation
	
	rickerFn = function(nnplus1){
		ricker <- nls(NPlus1 ~ I((N^(1+a)) * b * (1-(N/K))) , data = nnplus1, start = list(a=0.4, b=1.5, K=100), 
				algorithm= "port", trace = T)
		return(ricker) # start = list(a=0.4, b=1.5, K=100)
	}
	
	
	
	tryCatchRicker= function(x) {
		tryCatch(rickerFn(x), warning = function(w) {print("warning")},
				error = function(e) {print("error")}) 
	}

	ricker <- tryCatchRicker(nnplus1)
	
	
	if (ricker =="error"){
		print ("error in ricker calculation") 
		
		p15 <- 	ggplot(data = nnplus1, aes(x= N, y = NPlus1, colour = prevDisp)) + geom_point() + 
				geom_abline(intercept = 0, slope = 1 ) + scale_y_continuous(limits = c(0, NA)) + scale_x_continuous(limits = c(0, NA)) +
				 ggtitle("ricker eqn did not work :-(")
		

		
		
	} else {
		print ("ricker equation did work!")
		nnplus1$rickerPredict <- predict(ricker)		
		
		p15 <- 	ggplot(data = nnplus1, aes(x= N, y = NPlus1, colour = prevDisp)) + geom_point() + 
				geom_abline(intercept = 0, slope = 1 ) + scale_y_continuous(limits = c(0, NA)) + scale_x_continuous(limits = c(0, NA)) +
				geom_line(aes(x = N, y = rickerPredict), colour = 'blue') + ggtitle("ricker eqn")
		
		rickerTable <- tidy(ricker)
		rickerTable$type <- "ricker"
		
		
	}
	
	
	options(warn = 0) # turns warnings back on

	
	
	if (exists("rickerTable") == TRUE & exists("logisticTable") == TRUE){
		print ("both tables exist")
		nnplusoneCom <- rbind(rickerTable, logisticTable)	

		
	} else {
		if(exists("rickerTable") == TRUE & exists("logisticTable") == FALSE){
		print("only ricker")	
		nnplusoneCom <- rickerTable
		
		

	} else {
		if(exists("rickerTable") == FALSE & exists("logisticTable") == TRUE){
			print("only the logistic table exists")
			nnplusoneCom <- logisticTable

	}else{	
		print("neither ricker or logistic worked")
		nnplusoneCom <- data.frame(term = NA, estimate = NA, std.error = NA,  p.value = NA,
				type = "both")

					
				}
			}
		}

		
		nnplusoneCom$Comp <- DF[1,1]
		nnplusoneCom$disp <- DF[1,2]
		nnplusoneCom$var <- DF[1,3]
		nnplusoneCom$meanK <- DF[1,4]
		nnplusoneCom$FileName<-fileName

	
	rm(nnplus1)
	
	#### Ind Files Graphs
	
	indFile <- read.csv(indFileToImport, quote = "")
	
	# change indFile$disperse to NA if nest did not produce dispersers, 0 if nests disperses but ind did not dispere
	# ColInfo$disp = 0 for nests that dispersed
	indFile <- merge(indFile, ColInfo, by = c("col_id", "col_age") )
	indFile$disperse[indFile$colDisp  == 0 & indFile$disperse == 0 ] <- NA
	
	ads <- subset(indFile, type =="Ad" & disperse != "NA")
	
	# boxplot of adult size disperal  vs no dispersal
	p16a <- ggplot(data = subset(indFile, type == "Ad"), aes(x= factor(disperse), y = food)) + geom_boxplot()
	
	p16b <- ggplot(data = subset(indFile, type == "Ad"), aes(x=factor(no_off), y = food)) + geom_boxplot()
	
	
	
	
	AdsByPopAgeAndCol<- ddply(subset(indFile, type =="Ad"), .(col_age, col_id, type), summarise, 
			AdSzeMax = max(food),
			AdSzeMin = min(food),
			AdSzeMean = mean(food)
	)
	
	AdsByPopAgeAndCol <- melt(AdsByPopAgeAndCol, id.vars = c("col_id", "col_age"), 
			measure.vars = c("AdSzeMax", "AdSzeMin", "AdSzeMean"), variable.name="MinMax", value.name="AdSize")
	
	AdsByPopAgeAndCol <-merge(ColInfo, AdsByPopAgeAndCol, by = c("col_id", "col_age"))
	
	# colony size by adult size
	p16 <- ggplot(data = AdsByPopAgeAndCol, aes(x= numAdsB4dis, y = AdSize, colour = MinMax)) + geom_point() +
			stat_smooth(se = FALSE) + mytheme + scale_y_continuous(limits = c(0, 1)) + ggtitle("ad size vs col size b4 dispersal")
	
	rm(AdsByPopAgeAndCol)
	
	JuvsByPopAgeAndCol<- ddply(subset(indFile, type =="juv"), .(col_age, col_id, type), summarise, 
			JuvSzeMax = max(food),
			JuvSzeMin = min(food),
			JuvSzeMean = mean(food)
	)
	
	JuvsByPopAgeAndCol <- melt(JuvsByPopAgeAndCol, id.vars = c("col_id", "col_age"), 
			measure.vars = c("JuvSzeMax", "JuvSzeMin", "JuvSzeMean"), variable.name="MinMax", value.name="JuvSize")
		
	JuvsByPopAgeAndCol <-merge(ColInfo, JuvsByPopAgeAndCol, by = c("col_id", "col_age"))
	
	# Colony size by number of juvs
	p17 <- ggplot(data = JuvsByPopAgeAndCol, aes(x= numAdsB4dis, y = JuvSize, colour = MinMax)) + geom_point() +
			stat_smooth(se = FALSE) + mytheme + scale_y_continuous(limits = c(0, 1)) + ggtitle("juv size vs ads before dispersal")
	

	rm(JuvsByPopAgeAndCol)	

	rm(indFile)

	
	h1 <- 2/5
	h2 <- 3/5
	
	p_grob <- arrangeGrob(p14,p15, ncol=2)
	print(grid.arrange(p0, p00, p1, p2, p3,  p4, p5, p6, p6a, p7, p8, p9, p10, p11, p12, p13, p13a,  p_grob,
					p16a, p16b, p16, p17, ncol = 1, heights = c(h1, h1, h1, h1, h1, h1, h1, h1, h1, h1, h1, h1, h1, h1, h1, h1, h2, h1, h1, h1, h1), 
					main = mytitle))
	
	dev.off()
	
	returnList <- list(DF, nnplusoneCom)
	#print (returnList)
	#return(returnList)
	

}

fileExistsFn <- function(filesCreatedcsv){ 	#checking whether files exist and returing a list of existing files

	filesThatExist <- c()
	

	for (i in 1:nrow(fileNames)){
		theFileName <-fileNames[i,1]
		
		#fileToImport <- paste(theFileName, ".py.csv", sep = "")
		#indFileToImport <- paste(theFileName, ".py_inds.csv", sep = "")
		fileToImport <- paste(folder, theFileName, ".py.csv", sep = "")
		indFileToImport <- paste(folder, theFileName, ".py_inds.csv", sep = "")
		
		
		if(file.exists(fileToImport) == "TRUE"  && file.exists(indFileToImport) == TRUE){
			
			print (paste("both files exist. File:", fileToImport))
			filesThatExist <- c(filesThatExist,  i)
			
		}else{
			print (paste("file(s) don't exist. File:", fileToImport))
		}
		
	}	
	return (filesThatExist)
}


 
#folder <- "R_Graphs/"
folder <- "DisperalSimulationOutput/"

fileNames<-read.csv(paste(folder, "FilesCreated.csv", sep = ""), quote="") # import file names csv file

fileNames[] <- lapply(fileNames, as.character) # making factors into strings


min_popAge <-100 # the number of generations to discount from the start of the calculations


## TO test the function
fileName <- fileNames[16,1]

files <- fileExistsFn(fileNames)

#files <- sample(files) # if I want to randomize the order that they are done, i.e. if there are too many!


numFiles<- length(files)

loop <- foreach(i=1:numFiles, .errorhandling='remove', .packages= c("ggplot2", "plyr", "gridExtra", "reshape2", "broom")) %dopar%{
	
#for (i in 1:numFiles){

	num <- files[i]
	
	theFileName <-fileNames[num,1]
	print ("file name in loop")
	print (theFileName)
	num_gens <- as.numeric(fileNames[num, 3])
	returnList <- graphFunction(folder, theFileName, num_gens, min_popAge)
}


DF <- loop[[1]][[1]]
N_NPlus1Vars <- loop[[1]][[2]]

if (length(files) > 1){
	for(i in 2:length(files)){
	print (i)
	DF <- rbind(DF, loop[[i]][[1]])
	N_NPlus1Vars <- rbind(N_NPlus1Vars, loop[[i]][[2]])
	
	}
}

stopCluster(cl)

write.table(DF, paste(folder, "PopDets.csv", sep = ""), sep=",", row.names = FALSE)

write.table(N_NPlus1Vars,paste(folder, "PopMapVars.csv", sep = ""), sep=",", row.names = FALSE)
