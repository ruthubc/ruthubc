# TODO: Add comment
# 
# Author: Ruth
###############################################################################



runGLMMPQR <- function(input_formula){
	
	
	outputModel<- glmmPQL(input_formula, ~1|NestID, family = gaussian(link = "log") ,
			data = condBootVar, weights = lmrWgts, niter = 10)
	

	results <- Anova(outputModel)
	stargazer(results,  summary = FALSE, title = "Anova of full model alone", header = FALSE)
	
	results$names<-rownames(results)
	
	
	
	highTerm <- results[which(results$`Pr(>Chisq)` == max(results$`Pr(>Chisq)`)), ]$names # gets the variable with the highest pvalue
	
	cat(paste("term with highest p value, ", max(results$`Pr(>Chisq)`), "is:", highTerm))
	
	return(list(outputModel, highTerm, max(results$`Pr(>Chisq)`)))
}

reduceFormula <- function(oldFunction, varToRemove){
	

	
	newFormula <- as.formula(paste(as.character(oldFunction), "-", varToRemove))
	return(newFormula)
	
	
	
}