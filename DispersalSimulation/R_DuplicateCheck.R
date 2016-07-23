# Author: Ruth
###############################################################################


library (plyr)
source("FilesExist.R")


filesCSV <- "FilesCreated.csv"

outputFile <- "DispersalTest.csv"

#folder <- "R_Graphs/"
folder <- "DisperalSimulationOutput/"


fileNames<-read.csv(paste(folder, filesCSV, sep = ""), quote="")# import file names csv file

fileNames[] <- lapply(fileNames, as.character) # making factors into string



files <- fileExistsFn(fileNames)

numFiles<- length(files)

print ("the number of files that exist:")
print (numFiles)

outputTable <- data.frame(FileName = character(numFiles), numDuplicates = numeric(numFiles), stringsAsFactors = FALSE)


for (i in 1:numFiles){
	print(i)	
	
	num <- files[i]
	
	theFileName <-fileNames[num,1]
	numGens <- fileNames[num, 3]
	
	fileToImport <- paste(folder, theFileName, ".py.csv", sep = "") 
	File <- read.csv(fileToImport, quote = "")
	
	uniqueCheck <- subset(File, select = c(colony_age, colony_ID))
	
	
	
	isthereDups <- uniqueCheck[duplicated(uniqueCheck[,1:2]),]
	
	numDups <- nrow(isthereDups)
	
	
	outputTable$FileName[i] <- theFileName
	outputTable$numDuplicates[i] <- numDups
	
	
	
}




write.csv(outputTable, "AreThereDups.csv")