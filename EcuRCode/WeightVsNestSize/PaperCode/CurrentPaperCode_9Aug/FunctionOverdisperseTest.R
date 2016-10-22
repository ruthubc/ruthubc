# TODO: Add comment
# 
# Author: Ruth
###############################################################################

# from http://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html

overdisp_fun <- function(model) {
	## number of variance parameters in an n-by-n variance-covariance matrix
	vpars <- function(m) {
		nrow(m) * (nrow(m) + 1)/2
	}
	# The next two lines calculate the residual degrees of freedom
	model.df <- sum(sapply(VarCorr(model), vpars)) + length(fixef(model))
	rdf <- nrow(model.frame(model)) - model.df
	# extracts the Pearson residuals
	rp <- residuals(model, type = "pearson")
	Pearson.chisq <- sum(rp^2)
	prat <- Pearson.chisq/rdf
	# Generates a p-value. If less than 0.05, the data are overdispersed.
	pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
	c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval)
}
library(MASS)

summary(model)
overdisp_fun(PQLMod)
length(fixef(model))
ranef(model)
model.frame(model)

model <- PQLMod
Anova(model)

dim(condVar)

a_output <- VarCorr(model)
getVarCov(model)
an_model <- vcov(model) # fixed effect variance covariance matrix

model.df <- sum(sapply(VarCorr(model), vpars)) + length(fixef(model))

a_output2 <- lapply(VarCorr(model), vpars)

sum(a_output)

sum

unlist(a_output)

an_output <- (unlist(lapply(a_output, function(x) sum(x)))

mapply(a_output)

sum(an_output, na.rm = TRUE)

typeof(an_output)
a_output[,2]
