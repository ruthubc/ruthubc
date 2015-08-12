# TODO: Add comment
# 
# Author: Ruth
###############################################################################

# info from here http://michaeljkoontz.weebly.com/uploads/1/9/9/4/19940979/parallel.pdf


library("doParallel")

print ("numCores")
detectCores()

cl <- makeCluster(2)
# Register cluster
registerDoParallel(cl)
# Find out how many cores are being used
getDoParWorkers()


x <- foreach(i = 1:4) %dopar% {
	sqrt(i)
	print (i)}
x

