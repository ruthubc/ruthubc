# TODO: Add comment
# 
# Author: Ruth
###############################################################################



number <- 5

test<- tryCatch({7+ numfber})

inputs = list(1, 2, 4, -5, 'oops', 0, 10)

for(input in inputs) {
	try(print(paste("log of", input, "=", log(input))))
	}
	

for(input in inputs) {
	tryCatch(print(paste("log of", input, "=", log(input))),
	warning = function(w) {print(paste("negative argument", input)); log(-input)},
	error = function(e) {print(paste("non-numeric argument", input)); NaN})
	}
	

	robustLog = function(x) {
		tryCatch(log(x), warning = function(w) {print(paste("negative argument", x)); log(-x)},
		error = function(e) {print(paste("non-numeric argument", x)); NaN}) 
		}

for(input in inputs) {
	print(paste("robust log of", input, "=", robustLog(input)))
}

robustLog("fd")