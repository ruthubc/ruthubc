# TODO: Add comment
# 
# Author: Ruth
###############################################################################


upStepWiseLmer <- function(data, yVar){
	
	base_formula <- yVar ~ logCtFm + InstarNumber + (1|NestID)	
	
	scnd_formula <- update(base_formula,    ~ . + InstarNumber:InstarSex )
	
	anova(base_formula, scnd_formula)
	
	thrd_formula <-  update(scnd_formula, ~. + I(logCtFm^2) )
	
	anova(scnd_formula, thrd_formula)
	
	forth_formula <- update(thrd_formula, ~. + logCtFm:InstarNumber)
	
	anova(thrd_formula, forth_formula)
	
	fift_formula <- update(frth_formula, ~. + logCtFm:InstarNumber:InstarSex)
	
	anova(forth_formula, fift_formula)

	
	
}
