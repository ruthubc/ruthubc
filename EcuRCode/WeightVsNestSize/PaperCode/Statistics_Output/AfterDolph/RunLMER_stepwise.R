# TODO: Add comment
# 
# Author: Ruth
###############################################################################

addStars_anova_F <- function(input_anova){
	
	# Extract the p-values
	pvals <- input_anova$`Pr(>F)`
	
# Use the symnum function to produce the symbols
	sigSymbols <- symnum(pvals, na = FALSE, 
			cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
			symbols = c("***", "**", "*", ".", " "), legend = FALSE)
	
	stars <- as.matrix(unclass(sigSymbols))
	
	binded <- cbind(input_anova, stars)
	binded$stars <- ifelse(binded$`Pr(>F)`== max(binded$`Pr(>F)`) & binded$`Pr(>F)` > 0.05 , " RMVD", as.character(binded$stars))
	
	binded$p_value <- paste(sprintf("%.3f", round(binded$`Pr(>F)`,3)), binded$stars, sep="")
	
	binded <- subset(binded, select = -c(`Pr(>F)`, stars))
	binded$F.value <- round(binded$F.value, 3)
	
	
	
	return(binded)
	
	
}



runLMER <- function(input_formula, myDataSet, forPDF = "n"){
	
	DataSetLMERfn <<- myDataSet # have to do this otherwise data not found
	
	outputModel<- lmer(input_formula, data = DataSetLMERfn, REML = FALSE)
		
	
	results <- anova(outputModel)
	
	#results <- aov(outputModel)
	
	if (forPDF == "n") {
		
		print(summary(outputModel))
		
	} else {
		results_stars <- addStars_anova_F(results)
		print(xtable(results_stars,  summary = FALSE, title = "Anova of full model alone", latex.environments = "left"))
		#x <- gsub("cccc", "lccc", x)
		#print("text")
		#x
	}
	
	
	results$names<-rownames(results)
	
	
	
	highTerm <- results[which(results$`Pr(>F)` == max(results$`Pr(>F)`)), ]$names # gets the variable with the highest pvalue
	
	#print("highterm")
	#print(highTerm)
	
	maxP_val <- max(results$`Pr(>F)`)
	
	i = 0
		
	while(highTerm == "logCtFm" | highTerm == "InstarNumber"){	
				
		i <- i + 1
		
		print(paste("i =", i))
				
		maxPValue <- max(results$`Pr(>F)`)
		
		x <- results$`Pr(>F)`
		
		n <- length(x)
		
		newMax <- sort(x,partial=n-1)[n-i]
		
		#newMax <- max( x[x!=max(x)] )
		
		highTerm <- results[which(results$`Pr(>F)` == newMax), ]$names
		
		maxP_val <- newMax

		
	}
	
	
	
	print(paste("term with highest p value is:", highTerm))
	
	
	return(list(outputModel, highTerm, maxP_val))
	
	print("output model")
	print(outputModel)
	
}


reduceFormula_lmer <- function(oldFormula, varToRemove){
	# make sure you have formula.tools package loaded
	
	newFormula <- as.formula(paste(as.character(oldFormula), "-", varToRemove))
	return(newFormula)
	
	
	
}


stepWiseRedLmerFn <- function(input_formula, myDataSet, forPDF = "n"){

	
	numTerms <- 5
	pVal <- 1
	#while(numTerms > 3){
	while(numTerms > 3 && pVal > 0.1){
		
		print("number of terms")
		print(numTerms)
		print("pvalue")
		print(pVal)
		
		modelOutput <- runLMER(input_formula, dataset, forPDF)
		input_formula <- reduceFormula_lmer(input_formula, modelOutput[[2]])
		pVal <- modelOutput[[3]]
		numTerms <- length(attr(terms(input_formula), "term.labels"))
		
	}
	return(input_formula)
}


pdfAnovaOutput <- function(model, forPDF = "n"){
	
results <- anova(model)
	
	if (forPDF == "n") {
		
		print(results)
		
	} else {
		results_stars <- addStars_anova_F(results)
		print(xtable(results,  summary = FALSE, title = "Anova of final model", latex.environments = "left"))
		#x <- gsub("cccc", "lccc", x)
		#print("text")
		#x
	}
	
}



