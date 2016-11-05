# TODO: Add comment
# 
# Author: Ruth
###############################################################################

addStars_anova <- function(input_anova){
	
	# Extract the p-values
	pvals <- input_anova$`Pr(>Chisq)`
	
# Use the symnum function to produce the symbols
	sigSymbols <- symnum(pvals, na = FALSE, 
        	cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
        	symbols = c("***", "**", "*", ".", " "), legend = FALSE)
	
	stars <- as.matrix(unclass(sigSymbols))
	
	binded <- cbind(input_anova, stars)
	binded$p_value <- paste(as.character(round(binded$`Pr(>Chisq)`, 3)), as.character(binded$stars))
	
	binded <- subset(binded, select = -c(`Pr(>Chisq)`, stars) )
	
	binded$Chisq <- round(binded$Chisq, 3)
	
	return(binded)
	
	
}



runGLMMPQR <- function(input_formula, myDataSet, forPDF = "n"){
	
	DataSetGLMMRPQRFn <<- myDataSet # have to do this otherwise data not found

	outputModel<- glmmPQL(input_formula, ~1|NestID, family = gaussian(link = "log") ,
			data = DataSetGLMMRPQRFn, weights = lmrWgts, niter = 10)	
	

	results <- Anova(outputModel)
	
	if (forPDF == "n") {
		
		print(Anova(outputModel))
		
	} else {
		results_stars <- addStars_anova(results)
		print(xtable(results_stars,  summary = FALSE, title = "Anova of full model alone", latex.environments = "left", align = 'llll'))
		#x <- gsub("cccc", "lccc", x)
		#print("text")
		#x
	}
	
	
	results$names<-rownames(results)
	
	
	
	highTerm <- results[which(results$`Pr(>Chisq)` == max(results$`Pr(>Chisq)`)), ]$names # gets the variable with the highest pvalue
	
	print(paste("term with highest p value, ", max(results$`Pr(>Chisq)`), "is:", highTerm))
	
	
	return(list(outputModel, highTerm, max(results$`Pr(>Chisq)`)))

}

reduceFormula <- function(oldFormula, varToRemove){
	 # make sure you have formula.tools package loaded

	newFormula <- as.formula(paste(as.character(oldFormula), "-", varToRemove))
	return(newFormula)
	
	
	
}

