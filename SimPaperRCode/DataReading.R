# TODO: Add comment
# 
# Author: Ruth
###############################################################################

# imports the file names
fileNames<-read.csv("kinshipEvolution/DataAnalysis/fileNames.csv", quote="", col.names="filenames")


for (i in 1:nrow(fileNames)){
	
for (i in 1:3){
	
	file<-read.delim(as.character(fileNames[i,]))
	
	file <- file[which (file$tick >=10000),]
	
	print (i)
	
	R <- file[1, 4]
	
	Beta <- file[1, 5]
	
	C <- file[1, 6]
	
	
	paste(i, "test", sep = "") <- file
}
	###########spline
	for (j in c(8,10,12,13)){ # spline smoothing the time series
		
		file[,j]<- (fnSpline(lam, file$tick, file[,j]))$y
	}
	
	
	
	a<-ccf(file$kinPref, file$avgCoop, lag.max=0, plot = FALSE)
	
	b<-ccf(file$kinPref, file$avgGrSize, lag.max=0, plot = FALSE)
	
	c<-ccf(file$kinPref, file$rel, lag.max=0, plot = FALSE)
	
	d<-ccf(file$avgGrSize, file$rel, lag.max=0, plot = FALSE)
	
	e<-ccf(file$avgGrSize, file$avgCoop, lag.max=0, plot = FALSE)
	
	f<-ccf(file$avgCoop, file$rel, lag.max=0, plot = FALSE)
	
	list<-c(R, Beta, C,  as.numeric(a$acf), as.numeric(b$acf), as.numeric(c$acf), as.numeric(d$acf), as.numeric(e$acf), as.numeric(f$acf))
	
	DF[i,]<-list # adding row
	
}