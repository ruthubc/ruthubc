# Author: Ruth
###############################################################################


require(reshape2)
library (ggplot2)
library (grid)
library (lme4)
library(lmerTest)
library(visreg)

condition_residuals <- function(inputData, bodyLenVar, subset_multiple) {
	
	column_index <- which(names(inputData) == bodyLenVar)
	
	if(length(column_index) == 0){ stop('variable not found - check spelling')}
	
	inputData$variable <- inputData[,column_index]
	
	inputData<- subset(inputData, !is.na(logWt) )
	inputData<- subset(inputData, !is.na(variable) )
	
	model <- lm(logWt ~ variable, inputData )
	
	inputData$condResiduals <- resid(model)  # putting the residuales into the dable
	
	if (subset_multiple == "y") {
		inputData <- subset(inputData, type == "multiple")
		
	}

	
	return(inputData)
	
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

