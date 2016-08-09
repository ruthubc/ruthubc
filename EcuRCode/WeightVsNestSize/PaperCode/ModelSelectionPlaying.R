# TODO: Add comment
# 
# Author: user
###############################################################################
### From here http://rpubs.com/kaz_yos/exhaustive

library(doBy)
library(knitr)
library(rmarkdown)


## Create vectors for outcome and predictors
outcome    <- c("logLeg")
predictors <- c("logCtFm", "Instar", "logCtFm:Instar", "I(logCtFm^2)", "I(logCtFm^2):Instar")
dataset    <- spidersMul


## The lines below should not need modification.

## Create list of models
list.of.models <- lapply(seq_along((predictors)), function(n) {
			
			left.hand.side  <- outcome
			right.hand.side <- apply(X = combn(predictors, n), MARGIN = 2, paste, collapse = " + ")
			right.hand.side <- paste(right.hand.side, "+ (1|NestID)")
			
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

print(result)

