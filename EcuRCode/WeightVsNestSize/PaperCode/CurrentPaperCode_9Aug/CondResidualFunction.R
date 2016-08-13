# Author: Ruth
###############################################################################


condition_residuals <- function(TheData, bodyLenVar) {
	
	inputData <- TheData
	
	column_index <- which(names(inputData) == bodyLenVar)
	
	if(length(column_index) == 0){ stop('variable not found - check spelling')}
	
	inputData$variable <- inputData[,column_index]
	
	inputData<- subset(inputData, !is.na(logWt) )
	inputData<- subset(inputData, !is.na(variable) )
	
	model <- lm(logWt ~ variable, inputData )
	
	inputData$condResiduals <- resid(model)  # putting the residuales into the dable
	
	inputData <- subset(inputData, select = c(ID, condResiduals))
	
	mergeTable <- merge(TheData, inputData, by = "ID", all = TRUE)
	
	
	return(mergeTable)
	
}


printOutput <- function(model) {
	
	print("")
	print("NEW MODEL")
	print(model@call)
	print("")
	print("Anova on single model")
	print(anova(model))
	print("")
	
	
}

