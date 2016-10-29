# TODO: Add comment
# 
# Author: Ruth
###############################################################################



runGLMMPQR <- function(input_formula, myDataSet, forPDF = "n"){
	
	DataSetGLMMRPQRFn <<- myDataSet # have to do this otherwise data not found

	outputModel<- glmmPQL(input_formula, ~1|NestID, family = gaussian(link = "log") ,
			data = DataSetGLMMRPQRFn, weights = lmrWgts, niter = 10)	
	

	results <- Anova(outputModel)
	
	if (forPDF == "n") {
		
		print(Anova(outputModel))
		
	} else {
		stargazer(results,  summary = FALSE, title = "Anova of full model alone", header = FALSE)
	}
	
	
	results$names<-rownames(results)
	
	
	
	highTerm <- results[which(results$`Pr(>Chisq)` == max(results$`Pr(>Chisq)`)), ]$names # gets the variable with the highest pvalue
	
	cat(paste("term with highest p value, ", max(results$`Pr(>Chisq)`), "is:", highTerm))
	
	
	return(list(outputModel, highTerm, max(results$`Pr(>Chisq)`)))

}

reduceFormula <- function(oldFormula, varToRemove){
	 # make sure you have formula.tools package loaded

	newFormula <- as.formula(paste(as.character(oldFormula), "-", varToRemove))
	return(newFormula)
	
	
	
}

