# TODO: Add comment
# test
# Author: Ruth
###############################################################################
library (plyr)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(broom)

#folder <- "R_Graphs/"
folder <- "DisperalSimulationOutput/"

fileNames<-read.csv(paste(folder, "FilesCreated.csv", sep = ""), quote="")# import file names csv file

fileNames[] <- lapply(fileNames, as.character) # making factors into strings

DF <- data.frame(Comp = numeric(0), disp = numeric(0), var = numeric(0), meanK = integer(0), pop_age = integer(0))#, fileName = character(0))

N_NPlus1_Vars <- data.frame(term = character(0), estimate = numeric(0), std.error = numeric(0),  p.value = numeric(0),
		type = character(0), Comp = numeric(0), disp = numeric(0), var = numeric(0), meanK = integer(0), 
		fileName = character(0))

num_gens <- 2000

## TO test the function
fileName <- fileNames[14,1]

#graph making function
graphFunction <- function(folder, fileName){

	#filetoImport <- paste(fileName, ".py.csv", sep = "")
	#indFileToImport <- paste(fileName, ".py_inds.csv", sep = "")
	filetoImport <- paste(folder, fileName, ".py.csv", sep = "")
	indFileToImport <- paste(folder, fileName, ".py_inds.csv", sep = "")
	
	
	pngTitle <- paste(folder, fileName, "_graph", ".png", sep = "")
	print ("overall file exists?")
	print(file.exists(filetoImport))
	print ("ind file exists?")
	print(file.exists(indFileToImport))

	File <- read.csv(filetoImport, quote = "")
	
	ColInfo <- data.frame(pop_age = File$pop_age, col_age = File$colony_age, col_id = File$colony_ID, numAdsB4dis = File$num_adsB4_dispersal)
	ColInfo <- unique(ColInfo)
	
	# graph variables
	
	mytitle = textGrob(label = fileName)
	
	pngHeight = 400 * 15 # 400 * number of graphs
	
	png(pngTitle,  width = 1600, height = pngHeight, units = "px", pointsize = 16) # height = 400* num graphs
	
	print("png title")
	print (pngTitle)
	mytheme = theme(text = element_text(size=16))

	
	
	DF_list <- c(as.numeric(File$Comp_slope[1]), File$disp_rsk[1], File$input_var[1], File$meanK[1], max(File$pop_age))#, filetoImport)

	
	
	ByPopAge<- ddply(subset(File, num_ads!=0), .(pop_age), summarise,
			NCols = length(!is.na(colony_ID)),
			TotNumInd = sum(num_ads),
			maxNumAds = max(num_ads),
			minNumAds = min(num_ads),
			meanNumAds = mean(num_ads)
		)
	
	ByPopAgeAndCol<- ddply(File, .(pop_age, colony_ID, colony_age, colAlive), summarise, 
			TotNumAds = sum(num_ads)
		)
	
	ByPopAgeAndCol$factorAge <- as.factor(ByPopAgeAndCol$pop_age)
	
	File$pcntDisperse <- File$dispersers / File$num_adsB4_dispersal
	File$pcntMoult <- File$num_juvs_moulting/File$numjuvs
	
	
	#pop age by total number of individuals
	p1 <- ggplot(data = ByPopAge, aes(x = pop_age, y = TotNumInd)) + geom_line() +  mytheme + ggtitle("total number of individuals in the population")
	
	#pop age by number of colonies
	p2 <- ggplot(data = ByPopAge, aes(x = pop_age, y = NCols)) + geom_line() +  mytheme + ggtitle("total number of colonies in the population")
	
	#number of adults per nest
	p3 <- ggplot(data = ByPopAge, aes(x= pop_age, y = meanNumAds)) + geom_line() +  geom_line(aes(x=pop_age, y = maxNumAds), colour = "blue") +
			geom_line() +  geom_line(aes(x=pop_age, y = minNumAds), colour = "red")+ mytheme + ggtitle("Mean, max and min num ads in nest")

	#average age
	#p4 <- ggplot(data = ByPopAgeAndCol, aes(x= factorAge, y = colony_age)) + geom_point() +  mytheme + ggtitle("colony age by population age")
	
	p4 <- ggplot(data = ByPopAgeAndCol, aes(x= colony_age, y = TotNumAds)) + geom_point() + mytheme + ggtitle("colony size by colony age")
	
	# next size vs dispersers
	p5 <- ggplot(data = File, aes(x= num_adsB4_dispersal, y = dispersers)) + geom_point() +  mytheme + stat_smooth(se=FALSE) + 
			scale_y_continuous(limits = c(0, NA)) + ggtitle("nest size before dispersal vs number of dispersers")
	
	p6 <- ggplot(data = File, aes(x= num_adsB4_dispersal, y = pcntDisperse)) + geom_point() + stat_smooth(se = FALSE) + mytheme +
			scale_y_continuous(limits = c(0, 1)) + ggtitle("percentage of ads dispersing")
	
	
	# Nest size before dispersal vs food per adult 
#### OK THIS DOESN'T MAKE ANY SENSE. SHOULD BE POTENTIAL FOOD IF NO INDIVIDUAL DISPERSED
	p7 <- ggplot(data = File, aes(x=num_adsB4_dispersal, y = colony_food/num_ads)) + geom_point() + stat_smooth(se = FALSE) +  
			mytheme + scale_y_continuous(limits = c(0, NA)) + ggtitle("colony food per capita before dispersal")
	
	#Nest size after dispersal with food per adult
	p8 <- ggplot(data = File, aes(x=num_ads, y = colony_food/num_ads )) + geom_point() + stat_smooth(se = FALSE) +  
			mytheme + scale_y_continuous(limits = c(0, NA)) + ggtitle("num ads after dispersal vs per capita food")
	
	# number juvs moulting with size after dispersal
	p9 <- ggplot(data = File, aes(x=num_ads, y = pcntMoult)) + stat_smooth(se = FALSE)  + geom_point() +  mytheme +
			scale_y_continuous(limits = c(0, 1)) + ggtitle("percentage juvs moults vs. num ads after dispersal")
	
	# percentage of adults dispersing by nest size

	## AGAIN I DON'T THINK THIS MAKES SENSE 
	p10 <- ggplot(data = File, aes(x=num_ads, y = ave_food)) + geom_point() +
			mytheme + scale_y_continuous(limits = c(0, 1)) + stat_smooth(se = FALSE) + ggtitle("")
	
	rm(ByPopAge)
	rm(ByPopAgeAndCol)
	
	
	deadcols <- subset(File, colAlive== 'dead')
	
	if (nrow(deadcols) == 0){
		df <- data.frame()
		p11<-ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 100) + ggtitle("NO COLONIES DIED!!")
		print("no colonies died")
	}else{
	# histogram of size of dead colonies
		p11 <- ggplot(data = subset(File, colAlive== 'dead'), aes(x= num_ads)) + geom_histogram() + mytheme + ggtitle("histogram of size of colonies when died (num ads after dispersal)")
	}
	rm(deadcols)
	rm(File)
	
	## Making n, n+1 graph
	nnplus1 <- data.frame(col_id=numeric(), pop_age = numeric(),  N=numeric(), NPlus1=numeric()) # creating empty data frame
	maxcol_id <- max(ColInfo$col_id)
	
	counter <- 0
	
	for (colony in 1:maxcol_id){
		print(colony)
		
		col_subset <- subset(ColInfo, col_id == colony) # test 3 colony 11 incorrect numbering of colonies somehow
		maxcol_age <- max(col_subset$col_age)
		mincol_age <- min(col_subset$col_age)
	
		for (age in mincol_age:maxcol_age){
			counter <- counter + 1		
		
			nnplus1[counter,1] <- colony # [row number, col num]
			nnplus1[counter,2] <- col_subset$pop_age[which(col_subset$col_age == age)]
			nnplus1[counter,3] <- col_subset$numAdsB4dis[which(col_subset$col_age == age)]
			
			if (age == maxcol_age){
				nnplus1[counter,4] <- 0
			}else{
				nnplus1[counter,4] <- col_subset$numAdsB4dis[which(col_subset$col_age == (age +1))]	
			}

		
		}
	}
	
	
	nnplus1 <- subset(nnplus1, pop_age != num_gens)
	
	######## calculating logistic equation
	
	logisticFn = function(nnplus1){
	
		logistic <- nls(NPlus1 ~ I((N^(1+a)) * exp(b) * (exp(-c * N))) , data = nnplus1, start = list(a=0.4, b=1.5, c=0.02), 
			algorithm= "port", trace = T)
		return (logistic)
	}	
	
	tryCatchLogistic= function(x) {
		tryCatch(logisticFn(x), warning = function(w) {print("warning")},
				error = function(e) {print("error")}) 
	}
	
	logistic <- tryCatchLogistic(nnplus1)
	 
	options(warn = -1) #suppress warnings globally
	
	if (logistic =="error"){
		print ("error in logistic calculation") 
		
		p12 <- ggplot(data = nnplus1, aes(x= N, y = NPlus1)) + geom_point() + 
				geom_abline(intercept = 0, slope = 1 ) + scale_y_continuous(limits = c(0, NA)) + scale_x_continuous(limits = c(0, NA))+
				ggtitle("logistic eqn did not work :-(")
		
		
	} else {
		print ("logistic equation did work!")
		
		nnplus1$logisticPredict <- predict(logistic)
		
		p12 <- ggplot(data = nnplus1, aes(x= N, y = NPlus1)) + geom_point() + 
				geom_abline(intercept = 0, slope = 1 ) + scale_y_continuous(limits = c(0, NA)) + scale_x_continuous(limits = c(0, NA)) +
				geom_line(aes(x = N, y = logisticPredict), colour = 'blue') + ggtitle("logistic eqn")
		
		logisticTable <- tidy(logistic)
		logisticTable$type <- "logistic"		
		
	}
	

	
	######## Calculating ricker equation
	
	rickerFn = function(nnplus1){
		ricker <- nls(NPlus1 ~ I((N^(1+a)) * b * (1-(N/K))) , data = nnplus1, start = list(a=0.4, b=1.5, K=100), 
				algorithm= "port", trace = T)
		return(ricker)
	}
	
	
	
	tryCatchRicker= function(x) {
		tryCatch(rickerFn(x), warning = function(w) {print("warning")},
				error = function(e) {print("error")}) 
	}

	ricker <- tryCatchRicker(nnplus1)
	
	
	if (ricker =="error"){
		print ("error in ricker calculation") 
		
		p13 <- 	ggplot(data = nnplus1, aes(x= N, y = NPlus1)) + geom_point() + 
				geom_abline(intercept = 0, slope = 1 ) + scale_y_continuous(limits = c(0, NA)) + scale_x_continuous(limits = c(0, NA)) +
				 ggtitle("ricker eqn did not work :-(")
		

		
		
	} else {
		print ("ricker equation did work!")
		nnplus1$rickerPredict <- predict(ricker)		
		
		p13 <- 	ggplot(data = nnplus1, aes(x= N, y = NPlus1)) + geom_point() + 
				geom_abline(intercept = 0, slope = 1 ) + scale_y_continuous(limits = c(0, NA)) + scale_x_continuous(limits = c(0, NA)) +
				geom_line(aes(x = N, y = rickerPredict), colour = 'blue') + ggtitle("ricker eqn")
		
		rickerTable <- tidy(ricker)
		rickerTable$type <- "ricker"
		
		
	}
	
	
	
	
	options(warn = 0) # turns warnings back on

	
	
	if (exists("rickerTable") == TRUE & exists("logisticTable") == TRUE){
		print ("both tables exist")
		nnplusoneCom <- rbind(rickerTable, logisticTable)
		

		
	} else{
		if(exists("rickerTable") == TRUE & exists("logisticTable") == FALSE){
		print("only ricker")	
		nnplusoneCom <- rickerTable
		
		

	} else{
		if(exists("rickerTable") == FALSE & exists("logisticTable") == TRUE){
			print("only the logistic table exists")
			nnplusoneCom <- logisticTable

	}else{	
		
		nnplusoneCom <- data.frame(term = NA, estimate = NA, std.error = NA,  p.value = NA,
				type = "both")

					
				}
			}
		}

		
		nnplusoneCom$Comp <- DF_list[1]
		nnplusoneCom$disp <- DF_list[2]
		nnplusoneCom$var <- DF_list[3]
		nnplusoneCom$meanK <- DF_list[4]
		nnplusoneCom$FileName<-fileName


	rm(ricker)
	rm(logistic)
	
	rm(nnplus1)
	
	#### Ind Files Graphs
	
	indFile <- read.csv(indFileToImport, quote = "")
	
	
	
	AdsByPopAgeAndCol<- ddply(subset(indFile, type =="Ad"), .(col_age, col_id, type), summarise, 
			AdSzeMax = max(food),
			AdSzeMin = min(food),
			AdSzeMean = mean(food)
	)
	
	AdsByPopAgeAndCol <- melt(AdsByPopAgeAndCol, id.vars = c("col_id", "col_age"), 
			measure.vars = c("AdSzeMax", "AdSzeMin", "AdSzeMean"), variable.name="MinMax", value.name="AdSize")
	
	AdsByPopAgeAndCol <-merge(ColInfo, AdsByPopAgeAndCol, by = c("col_id", "col_age"))
	
	p14 <- ggplot(data = AdsByPopAgeAndCol, aes(x= numAdsB4dis, y = AdSize, colour = MinMax)) + geom_point() +
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
	
	
	p15 <- ggplot(data = JuvsByPopAgeAndCol, aes(x= numAdsB4dis, y = JuvSize, colour = MinMax)) + geom_point() +
			stat_smooth(se = FALSE) + mytheme + scale_y_continuous(limits = c(0, 1)) + ggtitle("juv size vs ads before dispersal")
	

	rm(JuvsByPopAgeAndCol)	

	rm(indFile)

	print(grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15,  ncol = 1, main = mytitle))
	
	dev.off()
	
	returnList <- list(DF_list, nnplusoneCom)
	
	return(returnList)

}


#for (i in 1:nrow(fileNames)){
for (i in 14:15){
	print(i)	
	theFileName <-fileNames[i,1]
		
	#fileToImport <- paste(theFileName, ".py.csv", sep = "")
	fileToImport <- paste(folder, theFileName, ".py.csv", sep = "")
	print(fileToImport)	
	
	if(file.exists(fileToImport) == "TRUE"){
		print ("the file does exist which is good!")
		returnList <- graphFunction(folder, theFileName)
		DF[i,] <- returnList[[1]]
		N_NPlus1_Vars <- rbind(N_NPlus1_Vars, returnList[[2]])
		} else {
	print ("file does not exist")
}}


write.table(DF, paste(folder, "PopAge.csv", sep = ""), sep=",", row.names = FALSE)

write.table(N_NPlus1_Vars,paste(folder, "PopMapVars.csv", sep = ""), sep=",", row.names = FALSE)


