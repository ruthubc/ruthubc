# TODO: Add comment
# 
# Author: Ruth
###############################################################################

source("RunLMER_stepwise.R")

addStars_anovaTwoModel <- function(inputAnova){
	
	# Extract the p-values
	pvals <- inputAnova$`Pr(>Chisq)`
	
# Use the symnum function to produce the symbols
	sigSymbols <- symnum(pvals, na = FALSE, 
			cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
			symbols = c("***", "**", "*", ".", " "), legend = FALSE)
	
	stars <- as.matrix(unclass(sigSymbols))
	
	binded <- cbind(inputAnova, stars)
	
	format(binded, digits = 3)

	
	return(binded)
	
	
}

printAnovaTable <- function(formula1, formula2, inputData,  forPDF ='n'){
	
	cat(as.character(formula1))
	cat("vs")
	cat(as.character(formula2))
	
	
	lm1 <- lmer(formula1, data = inputData, REML = FALSE)
	
	lm2 <- lmer(formula2, data = inputData, REML = FALSE)
	
	inputAnova <- anova(lm1, lm2)
	
	
	if (forPDF == "n") {
		
		print(inputAnova)
		
	} else {
		results_stars <- addStars_anovaTwoModel(inputAnova)
		print(xtable(results_stars,  summary = FALSE, title = "ANOVA", latex.environments = "left"))
		#x <- gsub("cccc", "lccc", x)
		#print("text")
		#x
	}
}




upStepWiseLmer <- function(data, yVar, forPDF = 'n'){
	
	stringFmla <- paste(yVar, " ~ logCtFm + InstarNumber + (1|NestID)")	
	
	base_formula <- as.formula(stringFmla)	
	
	scnd_formula <- update(base_formula,    ~ . + InstarNumber:InstarSex )
	
	printAnovaTable(base_formula, scnd_formula, data, forPDF)
	
	thrd_formula <-  update(scnd_formula, ~. + I(logCtFm^2) )
	
	printAnovaTable(scnd_formula, thrd_formula, data, forPDF)
	
	forth_formula <- update(thrd_formula, ~. + logCtFm:InstarNumber)
	
	printAnovaTable(thrd_formula, forth_formula, data, forPDF)
	
	fift_formula <- update(forth_formula, ~. + logCtFm:InstarNumber:InstarSex)
	
	printAnovaTable(forth_formula, fift_formula, data, forPDF)
	
}

################## TESTING ###############

#upStepWiseLmer(spidersMul, "logLeg", forPDF = "y")

#inputData <- spidersMul

#yVar <- "logLeg"

#formula1 <- as.formula(stringFmla)	
#formula2 <- scnd_formula
