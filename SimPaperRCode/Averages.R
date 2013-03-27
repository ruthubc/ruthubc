# TODO: Add comment
# 
# Author: Ruth
###############################################################################


fileNames<-read.csv("kinshipEvolution/DataAnalysis/fileNames.csv", quote="", col.names="filenames")

averages<-as.data.frame(t(data.frame(colMeans(file))))

for (i in 1:nrow(fileNames)){
	#for (i in 1:10){
	

	
file<-read.delim(as.character(fileNames[i,]))

file <- file[which (file$tick >=10000),]

averages[i,]<-as.data.frame(t(data.frame(colMeans(file))))

print (i)

}

write.csv(averages, "kinshipEvolution/DataAnalysis/averages10000.csv")