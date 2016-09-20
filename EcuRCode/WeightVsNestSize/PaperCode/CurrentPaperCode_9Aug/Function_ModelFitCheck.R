# TODO: Add comment
# 
# Author: user
###############################################################################


model_Check <- function(model) {
	

	cat(docxFmula(model))
	
	qqnorm(resid(model), main = "")
	plot(fitted(model), resid(model), xlab = "Fitted", ylab = "Residuals")
	abline(0,0)
	#plotList <- list(plot1, plot2)
	#return(plotList)
	
	
}
