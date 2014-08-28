# TODO: Add comment
# 
# Author: user
###############################################################################


# getting bootstrap samples for the coeficient of variation to try and remove
# the influence of large sample sizes

library('boot') # not sure if I actually want to use boot

list1 <- c(2,5,9,7,5,6,3,9)
noinsamp <- 8

nboots <- 10
#bootvals<- vector(mode = "numeric", length = nboots)
bootvals <- c()

all <- data.frame(matrix(NA, nrow = 0, ncol = noinsamp))

for(i in seq_len(nboots)){
	bootvals <-  (sample(list1, size = noinsamp, replace = TRUE)) # var = variace
	all[i,] <- bootvals
}


all