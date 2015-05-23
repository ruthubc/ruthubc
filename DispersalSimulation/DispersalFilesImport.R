# TODO: Add comment
# test
# Author: Ruth
###############################################################################
library (plyr)
library(ggplot2)
library(gridExtra)

folder <- "DisperalSimulationOutput/"
#folder <- "R_Graphs/"

fileNames<-read.csv(paste(folder, "FilesCreated.csv", sep = ""), quote="")# import file names csv file

fileNames[] <- lapply(fileNames, as.character) # making factors into strings

DF <- data.frame(Comp = numeric(0), disp = numeric(0), var = numeric(0), meanK = integer(0), pop_age = integer(0))#, fileName = character(0))

## TO test the function
fileName <- theFileName

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
	indFile <- read.csv(indFileToImport, quote = "")
	
	
	DF_list <- c(as.numeric(File$Comp_slope[1]), File$disp_rsk[1], File$input_var[1], File$meanK[1], max(File$pop_age))#, filetoImport)
	print(DF_list)
	
	
	ByPopAge<- ddply(File, .(pop_age), summarise, # need to discount trials where no feeding obs and eve
			NCols = length(!is.na(colony_ID)),
			TotNumInd = sum(num_ads)
		)
	
	ByPopAgeAndCol<- ddply(File, .(pop_age, colony_ID, colony_age), summarise, # need to discount trials where no feeding obs and eve
			TotNumAds = sum(num_ads)
		)
	
	ByPopAgeAndCol$factorAge <- as.factor(ByPopAgeAndCol$pop_age)
	
	mytitle = fileName
	
	png(pngTitle,  width = 1300, height = 2800, units = "px", pointsize = 16) # height = 400* num graphs
	
	print("png title")
	print (pngTitle)
	mytheme = theme(text = element_text(size=16))
	
	
	#pop age by total number of individuals
	p1 <- ggplot(data = ByPopAge, aes(x = pop_age, y = TotNumInd)) + geom_line() + geom_point() +  mytheme # + mytitle)
	
	#pop age by number of colonies
	p2 <- ggplot(data = ByPopAge, aes(x = pop_age, y = NCols)) + geom_line() + geom_point() +  mytheme # + mytitle)
	
	#number of adults per nest
	p3 <- ggplot(data = ByPopAgeAndCol, aes(x= factorAge, y = TotNumAds)) + geom_point() + geom_boxplot() +  mytheme
	
	#average age
	p4 <- ggplot(data = ByPopAgeAndCol, aes(x= factorAge, y = colony_age)) + geom_point() + geom_boxplot() +  mytheme
	
	
	
	
	# next size vs dispersers
	p5 <- ggplot(data = File, aes(x= num_ads, y = dispersers)) + geom_point() +  mytheme
	
	# Nest size vs 
	p6 <- ggplot(data = File, aes(x=num_ads, y = colony_food/num_ads )) + geom_point() +  mytheme
	
	# number juvs moulting 
	p7 <- ggplot(data = File, aes(x=num_ads, y = num_juvs_moulting/numjuvs)) + geom_point() +  mytheme
	
	#print(grid.arrange(p5, p6, p7, ncol = 1))#, main = mytitle)

	print(grid.arrange(p1, p2, p3, p4, p5, p6, p7,  ncol = 1, main = mytitle))
	
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


#write.table(DF, paste(folder, "PopAge.csv", sep = ""), sep=",", row.names = FALSE)

