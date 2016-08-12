# TODO: Add comment
# 
# Author: user
###############################################################################



MRAnovaFun<- function(FullModel, RedModels) {  # RedModels Is A List
	


FullModFormula <- as.character(FullModel@call[2])

cat("Anova of full model alone",  paste("-Model:", FullModFormula),  sep = '\n\r')
anv1 <- anova(FullModel)
print(kable(anv1, format = 'latex'))

cat("\\par", "---Testing Individual Variables, (Anova of full vs reduced model)---","",  sep = "\n\r")

numTests <- length(RedModels)

for (i in seq(1:numTests)) {
	
	Title <- RedModels[[i]][[1]]
	RedLM <- RedModels[[i]][[2]]
	cat(Title)
	
	RedModFormula <- as.character(RedLM@call[2])
	anv2 <- anova(FullModel, RedLM)
	cat("", paste("-Full Model:", FullModFormula), paste("-Reduced Model:", RedModFormula), sep = '\n\r')
	print(kable(anv2, format = 'latex'))
	
}





}



