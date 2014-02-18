# TODO: Add comment
# 
# Author: Ruth
###############################################################################


overdisp_fun <- function(model) {
	
	
	## number of variance parameters in 
	##   an n-by-n variance-covariance matrix
	vpars <- function(m) {
		nrow(m)*(nrow(m)+1)/2
	}
	model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
	rdf <- nrow(model.frame(model))-model.df
	rp <- residuals(model,type="pearson")
	Pearson.chisq <- sum(rp^2)
	prat <- Pearson.chisq/rdf
	pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
	dev <- deviance(model)
	myRatio <- dev/rdf	
	c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval, dev = dev, myRatio = myRatio)
	
}

modelPlot <- function(model){
	on.exit(par(ask=FALSE))
	par(ask=TRUE)
	qqnorm(resid(model))
	qqnorm(resid(model))
	plot(model)
	plot(model) # need dummy plot for some reason!
	


}


multipleModel <- function(model, nullModel){
	
TheModel <- as.character(model@call[2])
AIC <- AIC(model)
BIC <- BIC(model)
pValue <- as.numeric(anova(nullModel, model)$`Pr(>Chisq)`[2])
return(c(TheModel, AIC, BIC, pValue))
}
