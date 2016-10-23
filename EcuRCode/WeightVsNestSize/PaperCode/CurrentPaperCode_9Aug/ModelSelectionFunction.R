# TODO: Add comment
# 
# Author: user
###############################################################################
### From here http://rpubs.com/kaz_yos/exhaustive


## Create vectors for outcome and predictors


allModelsAIC <- function(outcome, predictors, dataset, weights = "n", nnLnr = "n") {
	
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
				
				if (nnLnr != "n"){
					print("nmle test")
					fit <- lme(formula, random = ~1|NestID, weights = ~I(1/lmrWgts), data = dataset, method = "ML")
				}
					else if(weights == "n") {
					
					fit  <- lmer(formula, data = dataset, REML = FALSE)
					
				} else {
					fit  <- lmer(formula, data = dataset, weights = lmrWgts, REML = FALSE)
					
				}
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
	result <- result[(result$dup == "FALSE"), ]
	result <- orderBy(~ AIC, result)	
	
	
	
	lowestAIC <- min(result$AIC)
	result$AIC_Diff <- result$AIC - lowestAIC
			
	result <- result[, c('AIC_Diff', 'AIC', 'model', 'num.predictors')]
	
	
	return(result)
	
	
}

allModelsAICWithSex <- function(outcome, predictors, dataset, weights = "n", nnLnr = "n") {
	
	if (weights != "n") {print("Using a standardized sample size as weight in model")}
	
	list.of.models <- lapply(seq_along((predictors)), function(n) {
				
				left.hand.side  <- outcome
				right.hand.side <- apply(X = combn(predictors, n), MARGIN = 2, paste, collapse = " + ")
				right.hand.side <- paste("logCtFm +",  right.hand.side)
				
				paste(left.hand.side, right.hand.side, sep = "  ~  ")
			})
	
	## Convert to a vector
	vector.of.models <- unlist(list.of.models)
	
	
	## Fit coxph to all models
	list.of.fits <- lapply(vector.of.models, function(x) {
				
				formula    <- as.formula(x)
				
				if (nnLnr != "n"){
					print("nlr test test")
					
					formula <- update(formula, .~. +  (1|NestID))
					print(formula)

					try(fit <-  glmer(formula, family = gaussian(link = "log"), data = dataset, weights = lmrWgts, nAGQ = 10))
				}
				else if(weights == "n") {
					
					fit  <- lmer(formula  + (1|NestID), data = dataset, REML = FALSE)
					
				} else {
					fit  <- lmer(formula + (1|NestID), data = dataset, weights = lmrWgts, REML = FALSE)
					
				}
				
				result.AIC <- c(NA, NA)
				try(result.AIC <- extractAIC(fit))
				
				data.frame(num.predictors = result.AIC[1],
						AIC            = result.AIC[2],
						model          = x)
			})
	
	## Collapse to a data frame
	result <- do.call(rbind, list.of.fits)
	
	result$modLen <- nchar(as.character(result$model))
	
	result <- orderBy(~ -modLen, result)
	
	result$dup <- duplicated(result[,c('num.predictors', 'AIC')])
	
	
	result <- result[(result$dup == "FALSE"), ]
	
	result <- orderBy(~ AIC, result)	
	lowestAIC <- min(result$AIC)
	result$AIC_Diff <- result$AIC - lowestAIC
	
	result <- result[, c('AIC_Diff', 'AIC', 'model', 'num.predictors')]
	
	
	return(result)
	
	
}



