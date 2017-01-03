# TODO: Add comment
# 
# Author: user
###############################################################################


#### NB actual file in owncloud#####################
###<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>########## LOOK ABOVE

fileExistsFn <- function(filesCreatedcsv){ 	#checking whether files exist and returning a list of existing files
	
	filesThatExist <- c()
	
	
	for (i in 1:nrow(fileNames)){
		theFileName <-fileNames[i,1]
		
		fileToImport <- paste(folder, theFileName, ".py.csv", sep = "")
		#fileToImport <- paste(theFileName, ".py.csv", sep = "")	##############################################################	
		
		
		if(file.exists(fileToImport) == "TRUE"){
			
			#print (paste("The file exists! Yay!. File:", fileToImport))
			filesThatExist <- c(filesThatExist,  i)
			
		}else{
			print (paste("file doesn't exist. File:", fileToImport))
		}
		
	}	
	
	print(paste("Number of files that exist:", length(filesThatExist)))
	return (filesThatExist)
}
