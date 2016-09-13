# TODO: Add comment
# 
# Author: user
###############################################################################
library(stargazer)

MRAnovaFun<- function(FullModel, RedModels) {  # RedModels Is A List
#cat("Anova of full model alone",  paste("-Model:", FullModFormula),  sep = '\n\r')
anv1 <- anova(FullModel)
#print(kable(anv1, format = 'latex'))
FullModFormula <- sub("I(InstarNumber^2)", "InstarNoSqrd", as.character(FullModel@call[2]))

cat("Full Model:", FullModFormula)
stargazer(anv1, summary = FALSE, title = "Anova of full model alone", header = FALSE)

cat("\\par", "Testing Individual Variables by preforming an Anova of full vs reduced model","",  sep = "\n\r")

numTests <- length(RedModels)

for (i in seq(1:numTests)) { 
	
	Title <- RedModels[[i]][[1]]

	RedLM <- RedModels[[i]][[2]]
	
	RedModFormula <- sub("~", "=", as.character(RedLM@call[2]))
	RedModFormula <- gsub("[[:punct:]]", " ", RedModFormula)
	

	
	
	
	anv2 <- anova(FullModel, RedLM)
	
	pValue <- round(anv2[[8L]][2], digits = 3)
	
	if (pValue <= 0.001) {
		stars <- " p < 0.001 SIGNIFICANT *** "
	}else if (pValue <= 0.01) {
		stars <- "p < 0.01 SIGNIFICANT ** "
		
	}else if (pValue <= 0.05) {
		stars <- "p < 0.05 SIGNIFICANT * "
		
	}else{
		stars <-"NOT significant"
	}
	
	Title <- paste(Title, " against full model. -  ", stars)
	#cat("", paste("-Full Model:", FullModFormula), paste("-Reduced Model:", RedModFormula), sep = '\n\r')
	myNotes <- paste("Reduced Model:", RedModFormula, sep = " ")
	#print(kable(anv2, format = 'latex'))
	stargazer(anv2, summary = FALSE, p.auto = FALSE, title = Title, notes = myNotes, header = FALSE)
	
}


}



