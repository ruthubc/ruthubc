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
	

	#print(paste("FullModel:", docxFmula(FullModel)))
	#print(paste("RedModel:", docxFmula(RedModel)))
	

	
	textOutput <- paste("chisqr", lowDF, highDF, "=" , ChiSqr, ", p =",
			pValue, stars, sep = "")
	outputList <- list(lowDF, highDF, ChiSqr, pValue, stars, textOutput)
	
	
}


docxFmula <- function(model) {
	
	txForm <- as.character(formula(model))
	txForm <- gsub("[]^ _()`|]", "",  txForm)
	txForm <- gsub("1", "",  txForm)
	txForm <- gsub("[+]", " + ",  txForm)
	txForm <- sub("NestID", "Nest", txForm)
	txForm <- gsub("IInstarNumber2", "sqr(InstarAge)", txForm)
	txForm <- gsub("OrigNest", "(1|OrigNst)", txForm)
	txForm <- gsub("Nest", "(1|Colony)", txForm)
	txForm <- gsub("InstarNumber", "InstarAge", txForm)
	txForm <- gsub("bootVarTrans", "Variance", txForm)
	txForm <- gsub("logCtFm", "log(ColonySize)", txForm)
	txForm <- gsub("condResiduals", "Condition", txForm)
	txForm <- gsub("type", "ColonyType", txForm)
	txForm <- gsub("OrigNst", "SourceColony", txForm)
	txForm <- gsub("logLeg", "log(LegLength)", txForm)
	txForm <- gsub("~", "=", txForm)
	return(txForm)
	
	
}

docxFmulaNoRan <- function(model) {
	
	txForm <- formula(model)
	txForm <- gsub("[]^ _()`|]", "",  txForm)
	txForm <- gsub("1", "",  txForm)
	txForm <- gsub("[+]", " + ",  txForm)
	txForm <- sub("NestID", "Nest", txForm)
	txForm <- gsub("IInstarNumber2", "sqr(InstarAge)", txForm)
	txForm <- gsub("InstarNumber", "InstarAge", txForm)
	txForm <- gsub("~", "=", txForm)
	return(txForm)
	
	
}




