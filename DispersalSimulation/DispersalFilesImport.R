# TODO: Add comment
# test
# Author: Ruth
###############################################################################
library (plyr)
library(ggplot2)
library(gridExtra)
library(reshape2)

folder <- "DisperalSimulationOutput/"
#folder <- "R_Graphs/"

fileNames<-read.csv(paste(folder, "FilesCreated.csv", sep = ""), quote="")# import file names csv file

fileNames[] <- lapply(fileNames, as.character) # making factors into strings

DF <- data.frame(Comp = numeric(0), disp = numeric(0), var = numeric(0), meanK = integer(0), pop_age = integer(0))#, fileName = character(0))

## TO test the function
#fileName <- theFileName

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
	
	ColInfo <- data.frame(col_age = File$colony_age, col_id = File$colony_ID, numAdsB4dis = File$num_adsB4_dispersal)
	
	# graph variables
	mytitle = textGrob(label = fileName)
	
	png(pngTitle,  width = 1300, height = 4400, units = "px", pointsize = 16) # height = 400* num graphs
	
	print("png title")
	print (pngTitle)
	mytheme = theme(text = element_text(size=16))

	
	
	DF_list <- c(as.numeric(File$Comp_slope[1]), File$disp_rsk[1], File$input_var[1], File$meanK[1], max(File$pop_age))#, filetoImport)
	print(DF_list)
	
	
	ByPopAge<- ddply(File, .(pop_age), summarise,
			NCols = length(!is.na(colony_ID)),
			TotNumInd = sum(num_ads)
		)
	
	ByPopAgeAndCol<- ddply(File, .(pop_age, colony_ID, colony_age), summarise, 
			TotNumAds = sum(num_ads)
		)
	
	ByPopAgeAndCol$factorAge <- as.factor(ByPopAgeAndCol$pop_age)
	
	File$pcntDisperse <- File$dispersers / File$num_adsB4_dispersal
	File$pcntMoult <- File$num_juvs_moulting/File$numjuvs
	
	
	#pop age by total number of individuals
	p1 <- ggplot(data = ByPopAge, aes(x = pop_age, y = TotNumInd)) + geom_line() + geom_point() +  mytheme
	
	#pop age by number of colonies
	p2 <- ggplot(data = ByPopAge, aes(x = pop_age, y = NCols)) + geom_line() + geom_point() +  mytheme
	
	#number of adults per nest
	p3 <- ggplot(data = ByPopAgeAndCol, aes(x= factorAge, y = TotNumAds)) + geom_point() + geom_boxplot() +  mytheme
	
	#average age
	p4 <- ggplot(data = ByPopAgeAndCol, aes(x= factorAge, y = colony_age)) + geom_point() + geom_boxplot() +  mytheme
	
	# next size vs dispersers
	p5 <- ggplot(data = File, aes(x= num_adsB4_dispersal, y = dispersers)) + geom_point() +  mytheme + 
			scale_y_continuous(limits = c(0, NA))
	
	# Nest size vs food per adult
	p6 <- ggplot(data = File, aes(x=num_adsB4_dispersal, y = colony_food/num_ads )) + geom_point() + stat_smooth(se = FALSE) +  
			mytheme + scale_y_continuous(limits = c(0, NA))
	
	# number juvs moulting 
	p7 <- ggplot(data = File, aes(x=num_adsB4_dispersal, y = pcntMoult)) + stat_smooth(se = FALSE)  + geom_point() +  mytheme
	
	# percentage of adults dispersing by nest size
	p8 <- ggplot(data = File, aes(x= num_adsB4_dispersal, y = pcntDisperse)) + geom_point() + stat_smooth(se = FALSE) + mytheme +
			scale_y_continuous(limits = c(0, 1))
	
	p9 <- ggplot(data = File, aes(x=num_adsB4_dispersal, y = ave_food)) + geom_point() +
			mytheme + scale_y_continuous(limits = c(0, 1)) + stat_smooth(se = FALSE)
	
	rm(ByPopAge)
	rm(ByPopAgeAndCol)
	rm(File)
	
	
	
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
	
	p10 <- ggplot(data = AdsByPopAgeAndCol, aes(x= numAdsB4dis, y = AdSize, colour = MinMax)) + geom_point() +
			stat_smooth(se = FALSE) + mytheme + scale_y_continuous(limits = c(0, 1))
	
	rm(AdsByPopAgeAndCol)
	
	JuvsByPopAgeAndCol<- ddply(subset(indFile, type =="juv"), .(col_age, col_id, type), summarise, 
			JuvSzeMax = max(food),
			JuvSzeMin = min(food),
			JuvSzeMean = mean(food)
	)
	
	JuvsByPopAgeAndCol <- melt(JuvsByPopAgeAndCol, id.vars = c("col_id", "col_age"), 
			measure.vars = c("JuvSzeMax", "JuvSzeMin", "JuvSzeMean"), variable.name="MinMax", value.name="JuvSize")
		
	JuvsByPopAgeAndCol <-merge(ColInfo, JuvsByPopAgeAndCol, by = c("col_id", "col_age"))
	
	
	p11 <- ggplot(data = JuvsByPopAgeAndCol, aes(x= numAdsB4dis, y = JuvSize, colour = MinMax)) + geom_point() +
			stat_smooth(se = FALSE) + mytheme + scale_y_continuous(limits = c(0, 1))
	

	rm(JuvsByPopAgeAndCol)	

	rm(indFile)

	print(grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11,  ncol = 1, main = mytitle))
	
	dev.off()
	
	return(DF_list)

}


#for (i in 1:nrow(fileNames)){
for (i in 1:2){
	print(i)	
	theFileName <-fileNames[i,1]
		
	#fileToImport <- paste(theFileName, ".py.csv", sep = "")
	fileToImport <- paste(folder, theFileName, ".py.csv", sep = "")
	print(fileToImport)	
	
	if(file.exists(fileToImport) == "TRUE"){
		print ("the file does exist which is good!")
		list <- graphFunction(folder, theFileName)
		DF[i,] <- list
		} else {
	print ("file does not exist")
}}


write.table(DF, paste(folder, "PopAge.csv", sep = ""), sep=",", row.names = FALSE)


