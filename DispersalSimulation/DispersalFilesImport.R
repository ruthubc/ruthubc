# TODO: Add comment
# test
# Author: Ruth
###############################################################################
library (plyr)
library(ggplot2)
library(gridExtra)

folder <- "DisperalSimulationOutput/"

fileNames<-read.csv(paste(folder, "FilesCreated.csv", sep = ""), quote="")# import file names csv file

fileNames[] <- lapply(fileNames, as.character) # making factors into strings

DF <- data.frame(Comp = numeric(0), disp = numeric(0), var = numeric(0), meanK = integer(0), pop_age = integer(0))#, fileName = character(0))

#graph making function
graphFunction <- function(folder, fileName){
	filetoImport <- paste(folder, fileName, ".py.csv", sep = "")
	indFileToImport <- paste(folder, fileName, ".py_inds.csv", sep = "")
	pdfTitle <- paste(folder, fileName, "_graph", ".pdf", sep = "")
	print(file.exists(filetoImport))
	print(file.exists(indFileToImport))

	File <- read.csv(filetoImport, quote = "")
	indFile <- read.csv(indFileToImport, quote = "")
	
	
	DF_list <- c(as.numeric(File$Comp_slope[1]), File$disp_rsk[1], File$input_var[1], File$meanK[1], max(File$pop_age))#, filetoImport)
	print(DF_list)
	return(DF_list)
	ByPopAge<- ddply(File, .(pop_age), summarise, # need to discount trials where no feeding obs and eve
			NCols = length(!is.na(colony_ID)),
			TotNumInd = sum(num.ads)
	)
	
	ByPopAgeAndCol<- ddply(File, .(pop_age, colony_ID, colony_age), summarise, # need to discount trials where no feeding obs and eve
			TotNumAds = sum(num.ads)
	
	)
	
	ByPopAgeAndCol$factorAge <- as.factor(ByPopAgeAndCol$pop_age)
	
	mytitle = ggtitle(fileName)
	
	pdf(pdfTitle, width =10, height =10)
	
	#pop age by total number of individuals
	p1 <- ggplot(data = ByPopAge, aes(x = pop_age, y = TotNumInd)) + geom_line() + geom_point()# + mytitle
	
	#pop age by number of colonies
	p2 <- ggplot(data = ByPopAge, aes(x = pop_age, y = NCols)) + geom_line() + geom_point()# + mytitle
	
	#number of adults per nest
	p3 <- ggplot(data = ByPopAgeAndCol, aes(x= factorAge, y = TotNumAds)) + geom_point() + geom_boxplot()
	
	#average age
	p4 <- ggplot(data = ByPopAgeAndCol, aes(x= factorAge, y = colony_age)) + geom_point() + geom_boxplot()
	
	
	grid.arrange(p1, p2, p3, p4, ncol = 1)#, main = mytitle)
	
	# next size vs dispersers
	p5 <- ggplot(data = File, aes(x= num.ads, y = dispersers)) + geom_point()
	
	# Nest size vs 
	p6 <- ggplot(data = File, aes(x=num.ads, y = colony_food/num.ads )) + geom_point()
	
	# number juvs moulting 
	p7 <- ggplot(data = File, aes(x=num.ads, y = num_juvs_moulting/numjuvs)) + geom_point()
	
	grid.arrange(p5, p6, p7, ncol = 1)#, main = mytitle)
	
	dev.off()

}


for (i in 1:nrow(fileNames)){
#for (i in 1:7){
	print(i)
	theFileName <-fileNames[i,1]
	print(theFileName)
	list <- graphFunction(folder, theFileName)
	DF[i,] <- list
	#dev.off()


}

write.table(DF, paste(folder, "PopAge.csv", sep = ""), sep=",", row.names = FALSE)

