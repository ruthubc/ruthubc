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
	binded$stars <- ifelse(binded$`Pr(>Chisq)`== max(binded$`Pr(>Chisq)`) & binded$`Pr(>Chisq)` > 0.05 , " RMVD", as.character(binded$stars))
	
	binded$p_value <- paste(sprintf("%.3f", round(binded$`Pr(>Chisq)`,3)), binded$stars, sep="")
	
	binded <- subset(binded, select = -c(`Pr(>Chisq)`, as.character(stars)))
	binded$Chisq <- round(binded$Chisq, 3)
	
	
	return(binded)
	
	
}

addStarsOnly_anova <- function(input_model){
	
	input_anova <- Anova(input_model)
	# Extract the p-values
	pvals <- input_anova$`Pr(>Chisq)`
	
# Use the symnum function to produce the symbols
	sigSymbols <- symnum(pvals, na = FALSE, 
			cutpoints = c(0, 0.001, 0.01, 0.05, 0.1), 
			symbols = c("*** ", "** ", "* ", " "),
					legend = FALSE)
	
	stars <- as.matrix(unclass(sigSymbols))
	
	input_anova$parameters<-rownames(input_anova)
	
	binded <- cbind(input_anova, stars)
	
	binded$Chisq <- sprintf("%.3f", round(binded$Chisq,3))
	
	binded$pValue <- ifelse(input_anova$`Pr(>Chisq)` <0.001, "< 0.001", paste("=", sprintf("%.3f", round(binded$`Pr(>Chisq)`,3)) ))
	binded$stars <- as.character(binded$stars)
	
	return(binded)
}



runGLMMPQR <- function(input_formula, myDataSet, forPDF = "n"){
	
	DataSetGLMMRPQRFn <<- myDataSet # have to do this otherwise data not found

	outputModel<- glmmPQL(input_formula, ~1|NestID, family = gaussian(link = "log") ,
			data = DataSetGLMMRPQRFn, weights = lmrWgts, niter = 10)	
	

	results <- Anova(outputModel)
	
	if (forPDF == "n") {
		
		print(summary(outputModel))
		
	} else {
		results_stars <- addStars_anova(results)
		print(xtable(results_stars,  summary = FALSE, title = "Anova of full model alone", latex.environments = "left", align = 'llll'))
		#x <- gsub("cccc", "lccc", x)
		#print("text")
		#x
	}
	
	
	results$names<-rownames(results)
	
	
	
	highTerm <- results[which(results$`Pr(>Chisq)` == max(results$`Pr(>Chisq)`)), ]$names # gets the variable with the highest pvalue
	
	print(paste("term with highest p value is:", highTerm))
	
	
	return(list(outputModel, highTerm, max(results$`Pr(>Chisq)`)))

}

reduceFormula <- function(oldFormula, varToRemove){
	 # make sure you have formula.tools package loaded

	newFormula <- as.formula(paste(as.character(oldFormula), "-", varToRemove))
	return(newFormula)
	
	
	
}

