# TODO: Add comment
# 
# Author: Ruth
###############################################################################


# testing to see if inter-individual distance would be better than

list = c(1,2,3,7,5,6)
results = c()

for (i in 1:length(list)){
	
	newlist <- list[-i]
	element <- list[i]
	print (i)
	
	for (j in newlist){
		print (j)
		difference <- abs(element - j)
		results <- c(results, difference )
		
		
	}
	
	
	print ("nest item in list")
}

results

sum(results)
