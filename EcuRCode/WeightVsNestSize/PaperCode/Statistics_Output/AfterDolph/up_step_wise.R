# TODO: Add comment
# 
# Author: Ruth
###############################################################################

#source("RunLMER_stepwise.R")



addStars_anovaTwoModel <- function(inputAnova){
	
	# Extract the p-values
	pvals <- inputAnova$`Pr(>Chisq)`
	
# Use the symnum function to produce the symbols
	sigSymbols <- symnum(pvals, na = FALSE, 
			cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
			symbols = c("***", "**", "*", ".", " "), legend = FALSE)
	
	stars <- as.matrix(unclass(sigSymbols))
	
	binded <- cbind(inputAnova, stars)
	
	format(binded, digits = 4)
	
	if (binded$`Pr(>Chisq)`[2] < 0.051) {
		cat("\r\n SIGNIFICANT \r\n")
	} else {
		
		cat("\r\n *****************NOT SIGNIFICANT --- STOP HERE ************** \r\n")
	}

	

	
	return(binded)
	
}

printAnovaTable <- function(formula1, formula2, inputData,  forPDF ='n'){
	
	cat(as.character(rhs(formula1)), "\r\n vs \r\n", as.character(rhs(formula2)))
	
	
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
	
	parameter_order <- c( ~ logCtFm:InstarNumber, 
			~ InstarNumber:InstarSex , 
			~ logCtFm:InstarNumber:InstarSex, 
			~ I(logCtFm ^2))
	
	stringFmla <- paste(yVar, " ~ logCtFm + InstarNumber + (1|NestID)")	
	
	base_formula <- as.formula(stringFmla)	
	
	scnd_formula <- merge.formula(base_formula, parameter_order[[1]])

	
	printAnovaTable(base_formula, scnd_formula, data, forPDF)
	
	thrd_formula <- merge.formula(scnd_formula, parameter_order[[2]])
	
	printAnovaTable(scnd_formula, thrd_formula, data, forPDF)
	
	forth_formula <- merge.formula(thrd_formula, parameter_order[[3]])
	
	printAnovaTable(thrd_formula, forth_formula, data, forPDF)
	
	fift_formula <- merge.formula(forth_formula, parameter_order[[4]])
	
	printAnovaTable(forth_formula, fift_formula, data, forPDF)
	
}

################## TESTING ###############

#upStepWiseLmer(spidersMul, "logLeg", forPDF = "y")

#inputData <- spidersMul

#yVar <- "logLeg"

#formula1 <- as.formula(stringFmla)	
#formula2 <- scnd_formula
