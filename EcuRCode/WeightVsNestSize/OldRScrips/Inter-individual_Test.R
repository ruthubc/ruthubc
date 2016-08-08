# TODO: Add comment
# 
# Author: Ruth
###############################################################################


# testing to see if inter-individual distance would be better than regular cv,
#BUT I think this may actually be variance.

list = c(1,2,3,7,5,166)
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
	
	
	print ("next item in list")
}

results

tot <- sum(results)
num <- length(results)

newCV = tot/num

print (newCV)