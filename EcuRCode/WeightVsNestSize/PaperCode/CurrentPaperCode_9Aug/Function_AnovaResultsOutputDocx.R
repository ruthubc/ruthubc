# TODO: Add comment
# 
# Author: user
###############################################################################


outputResultsWord <- function(FullModel, RedModel) {
	
	outputModel <- anova(FullModel, RedModel)
	
	lowDF <- outputModel[[1L]][1]
	
	highDF <- outputModel[[1L]][2]
	
	ChiSqr <- round(outputModel[[6L]][2], digits = 2)
	
	pValue <- round(outputModel[[8L]][2], digits = 3)
	
	if (pValue <= 0.001) {
		pValue <- "< 0.001"
		stars <- "*** "
	}else if (pValue <= 0.01) {
		stars <- "** "
		
	}else if (pValue <= 0.05) {
		stars <- "* "
	
	}else{
		stars <-""
	}
	
	textOutput <- paste("chisqr", lowDF, highDF, "=" , ChiSqr, ", p =",
			pValue, stars, sep = "")
	outputList <- list(lowDF, highDF, ChiSqr, pValue, stars, textOutput)
	
	
}

