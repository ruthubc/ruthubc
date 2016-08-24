# TODO: Add comment
# 
# Author: user
###############################################################################
### From here http://rpubs.com/kaz_yos/exhaustive


## Create vectors for outcome and predictors


allModelsAIC <- function(outcome, predictors, dataset) {
	
	list.of.models <- lapply(seq_along((predictors)), function(n) {
				
				left.hand.side  <- outcome
				right.hand.side <- apply(X = combn(predictors, n), MARGIN = 2, paste, collapse = " + ")
				right.hand.side <- paste(right.hand.side, "+ Instar + (1|NestID)")
				
				paste(left.hand.side, right.hand.side, sep = "  ~  ")
			})
	
	## Convert to a vector
	vector.of.models <- unlist(list.of.models)
	
	
	## Fit coxph to all models
	list.of.fits <- lapply(vector.of.models, function(x) {
				
				formula    <- as.formula(x)
				fit        <- lmer(formula, data = dataset, REML = FALSE)
				result.AIC <- extractAIC(fit)
				
				data.frame(num.predictors = result.AIC[1],
						AIC            = result.AIC[2],
						model          = x)
			})
	
	## Collapse to a data frame
	result <- do.call(rbind, list.of.fits)
	
	result$modLen <- nchar(as.character(result$model))
	
	result <- orderBy(~ -modLen, result)
	
	result$dup <- duplicated(result[,c('num.predictors', 'AIC')])
	
	result <- orderBy(~ AIC, result)	
	
	
	result <- result[(result$dup == "FALSE"), ]
	
	lowestAIC <- min(result$AIC)
	result$AIC_Diff <- result$AIC - lowestAIC
			
	result <- result[, c('AIC_Diff', 'AIC', 'model', 'num.predictors')]
	
	
	return(result)
	
	
}

allModelsAICWithSex <- function(outcome, predictors, dataset) {
	
	list.of.models <- lapply(seq_along((predictors)), function(n) {
				
				left.hand.side  <- outcome
				right.hand.side <- apply(X = combn(predictors, n), MARGIN = 2, paste, collapse = " + ")
				right.hand.side <- paste(right.hand.side, "+ InstarNumber + InstarSex +  (1|NestID)")
				
				paste(left.hand.side, right.hand.side, sep = "  ~  ")
			})
	
	## Convert to a vector
	vector.of.models <- unlist(list.of.models)
	
	
	## Fit coxph to all models
	list.of.fits <- lapply(vector.of.models, function(x) {
				
				formula    <- as.formula(x)
				fit        <- lmer(formula, data = dataset, REML = FALSE)
				result.AIC <- extractAIC(fit)
				
				data.frame(num.predictors = result.AIC[1],
						AIC            = result.AIC[2],
						model          = x)
			})
	
	## Collapse to a data frame
	result <- do.call(rbind, list.of.fits)
	
	result$modLen <- nchar(as.character(result$model))
	
	result <- orderBy(~ -modLen, result)
	
	result$dup <- duplicated(result[,c('num.predictors', 'AIC')])
	
	result <- orderBy(~ AIC, result)	
	
	
	result <- result[(result$dup == "FALSE"), ]
	
	lowestAIC <- min(result$AIC)
	result$AIC_Diff <- result$AIC - lowestAIC
	
	result <- result[, c('AIC_Diff', 'AIC', 'model', 'num.predictors')]
	
	
	return(result)
	
	
}



