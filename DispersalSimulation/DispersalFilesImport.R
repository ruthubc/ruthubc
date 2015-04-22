# TODO: Add comment
# 
# Author: Ruth
###############################################################################
library (plyr)
library(ggplot2)

folder <- "DisperalSimulationOutput/"

fileNames<-read.csv(paste(folder, "FilesCreated.csv", sep = ""), quote="")# import file names csv file

fileNames[] <- lapply(fileNames, as.character) # making factors into strings

#graph making function
graphFunction <- function(folder, fileName){
	filetoImport <- paste(folder, fileName, ".py.csv", sep = "")
	pdfTitle <- paste(folder, fileName, "_graph", ".png", sep = "")
	
	File <- read.csv(filetoImport, quote = "")
	print(head(File))
	ByPopAge<- ddply(File, .(pop_age), summarise, # need to discount trials where no feeding obs and eve
			NCols = length(!is.na(colony_ID)),
			TotNumInd = sum(num.ads)
	)
	
	print (ByPopAge)
	mytitle = ggtitle(fileName)
	
	png(pdfTitle, width =480, height =480)
	
	ggplot(data = ByPopAge, aes(x = pop_age, y = TotNumInd)) + geom_line() + geom_point() + mytitle
	
	ggplot(data = ByPopAge, aes(x = pop_age, y = NCols)) + geom_line() + geom_point() + mytitle
	
	#dev.off()
	
}


#for (i in 1:nrow(fileNames)){
for (i in 4:7){
	print(i)
	theFileName <-fileNames[i,1]
	print(theFileName)
	graphFunction(folder, theFileName)
	dev.off()


}


