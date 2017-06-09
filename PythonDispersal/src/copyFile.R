# TODO: Add comment
# 
# Author: Ruth
###############################################################################

setwd("C:/Users/Ruth/git/ruthubc/PythonDispersal/src")

fileNames <- read.csv("filesToDelete.csv", stringsAsFactors = FALSE)

nameList <- (fileNames[,1])


pyList <- paste(nameList, ".py", sep = "")

file.copy(from=pyList, to="Grex", recursive = FALSE, overwrite = TRUE)


csvList <- paste(nameList, ".csv", sep = "")

file.remove(csvList)