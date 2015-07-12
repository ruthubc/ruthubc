# TODO: Add comment
# 
# Author: user
###############################################################################


#folder <- "R_Graphs/"
folder <- "DisperalSimulationOutput/"

fileNames<-read.csv(paste(folder, "FilesCreated.csv", sep = ""), quote="")# import file names csv file

fileNames[] <- lapply(fileNames, as.character) # making factors into strings

for (i in 16:16){


	fileName <-fileNames[i,1]
	#fileName <- fileNames[16,1]

	#filetoImport <- paste(fileName, ".py.csv", sep = "")
	#indFileToImport <- paste(fileName, ".py_inds.csv", sep = "")
	filetoImport <- paste(folder, fileName, ".py.csv", sep = "")
	indFileToImport <- paste(folder, fileName, ".py_inds.csv", sep = "")


	File <- read.csv(filetoImport, quote = "")
	
	File$prevDisp <- "n"
	
	cols<- as.numeric(levels(as.factor(File$colony_ID)))
	
	for (i in 1:length(cols)){
		
		thisCol <- cols[i]
		print(thisCol)
		age_FstDisp <- File$colony_age[which(File$dispersers > 0 & File$colony_ID == thisCol)]
		if (length(age_FstDisp) > 0){
			min_ageFstDisp <- min(age_FstDisp)
			print (min_ageFstDisp)
			File$prevDisp[which(File$colony_age >= min_ageFstDisp & File$colony_ID == thisCol)] <- "y"
			
		}else{
			print("no dispersers")
		}
	}
	
	ColInfo <- data.frame(pop_age = File$pop_age, col_age = File$colony_age, col_id = File$colony_ID, 
			numAdsB4dis = File$num_adsB4_dispersal, dispersers = File$dispersers, prevDisp = File$prevDisp)
	ColInfo <- unique(ColInfo)
	
	ColInfo$colDisp <- 0
	
	ColInfo$colDisp[ColInfo$dispersers > 0 ] <- 1 # marking nests that had dispersers
	
	rm(File)
	
	indFile <- read.csv(indFileToImport, quote = "")
	
	indFile <- merge(indFile, ColInfo, by = c("col_id", "col_age") )
	indFile$disperse[indFile$colDisp  == 0 & indFile$disperse == 0 ] <- NA
	
	indFile$colDisp[indFile$disperse == 1] <- 1
	
	indFile <- subset(indFile, type == "Ad" & colDisp == 1)
	
	csvTitle<- paste(folder, "IndFileMerge", fileName, ".csv", sep = "")
	
	write.table(indFile, csvTitle, sep=",", row.names = FALSE)
}
	
