# TODO: Add comment
# 
# Author: Ruth
###############################################################################


library (plyr)
library(reshape2)
library(broom)
library(ggplot2)
source("FilesExist.R")

filesCSV <- "FilesCreated.csv"

outputFile <- "NNTPlus1Test.csv"

#folder <- "R_Graphs/" ##########################################################################
folder <- "DisperalSimulationOutput/"

fileNames<-read.csv(paste(folder, filesCSV, sep = ""), quote="")# import file names csv file

fileNames[] <- lapply(fileNames, as.character) # making factors into strings

## Change this to the correct number i want to remove
min_pop_age <-50 # the number of generations to discount from the start of the calculations

Log_output <- data.frame(FileNum = numeric(),
		Comp = numeric(), # making empty data frame
		meanK=numeric(),
		Fd_ln=numeric(), 
		Variance=numeric(),
		disp_risk = numeric(),
		ad_dsp_fd = numeric(),
		min_juv_fd = numeric(),
		max_no_off = numeric(),
		max_pop_age = numeric(),
		log_a = numeric(),
		log_b = numeric(),
		log_c = numeric(),
		stringsAsFactors=FALSE) 




FilesExist <- fileExistsFn(fileNames) # move down


## Logistic Function function
logisticFn = function(nnplus1){ # actually Ricker model from Hart & Aviles eqn 1
	
	logistic <- nls(NPlus1 ~ I((N^(1+a)) * exp(b) * (exp(-c * N))) , data = nnplus1, start = list(a=0.4, b=1.5, c=0.02), 
			lower=list(a=0, b=0,c=0), algorithm= "port", trace = T)
	
	#logistic <- nls(NPlus1 ~ (N * exp(b) * (exp(-c * N))) , data = nnplus1, start = list(b=1.5, c=0.02), 
	#algorithm= "port", trace = T)
	return (logistic) #list(a=0.4, b=1.5, c=0.02)
}	

tryCatchLogistic= function(x) {
	tryCatch(logisticFn(x), warning = function(w) {print("warning")},
			error = function(e) {print("error")}) 
}


## Ricker equation function, not using now 15th Jan
rickerFn = function(nnplus1){ # eqn 2 from Hart & Aviles
	#ricker <- nls(NPlus1 ~ I((N^(1+a)) * b * (1-(N/K))) , data = nnplus1, start = list(a=0.4, b=1.5, K=100), 
	#algorithm= "port", trace = T)
	
	ricker <- nls(NPlus1 ~ I((N * lam) * ((1- capA * exp((-a * N )/oth))/((1 + a *N )^b))),
			data = nnplus1, start = list(capA = 70, a = 0.01, b=1, lam = 1, oth = 1.0), 
			algorithm= "port", trace = T)
	
	return(ricker) # start = list(a=0.4, b=1.5, K=100)
}


tryCatchRicker= function(x) {
	tryCatch(rickerFn(x), warning = function(w) {print("warning")},
			error = function(e) {print("error")}) 
}



nntplus1Fun <- function(fileName, min_pop_age, numGens){
	
	fileToImport <- paste(folder, theFileName, ".py.csv", sep = "")
	#fileToImport <- paste(theFileName, ".py.csv", sep = "")##################################	
	
	File <- read.csv(fileToImport, quote = "")
	
	File <- subset(File, num_adsB4_dispersal > 1)  # removing single female colonies to see if this improves the estimates
	
	maxPopAge <- max(File$pop_age)
	
	if ((maxPopAge + 50) < min_pop_age){ File <- subset(File, pop_age >= min_pop_age) }	
	
	fileNum <- print (substr(fileName, 0, 4))
	
	
	rowVars <- c(fileNum, File$Comp_slope[1], File$meanK[1], File$Fd_ln[1], File$input_var[1], File$disp_rsk[1], File$ad_dsp_fd[1], 
			File$min_juv_fd[1], File$max_no_off[1], maxPopAge)
	
	if (sum(File$dispersers) == 0){
			File$prevDisp <- "n"
		}else{
	
			## making a table of dispersal information for each nest so can colour nests that had dispersed and had not
			firstDisp <- ddply(subset(File, dispersers > 0), .(colony_ID), summarise, firstDisp = min(pop_age))
			File <- merge(File, firstDisp, by = "colony_ID", all.x = TRUE)
	
			File$prevDisp <-  ifelse(File$firstDisp > File$pop_age | is.na(File$firstDisp), "n", "y" )
			File$prevDisp <- ifelse(File$dispersers > 0, "now", File$prevDisp)
		}

	File$ColAgePlus1 <- File$colony_age

	ColInfoPlus1 <- subset(File, select = c("colony_ID", "num_adsB4_dispersal", "colony_age"))
	
	nnplus1 <- subset(File, select = c("colony_ID", "num_adsB4_dispersal", "colony_age", "prevDisp"))

	
	colnames(ColInfoPlus1)[3] <- "ColAgePlus1"
	colnames(ColInfoPlus1)[2] <- "numAdsPlus1"

	nnplus1$ColAgePlus1 <- nnplus1$colony_age + 1

	nnplus1 <- merge(nnplus1, ColInfoPlus1,  by =c("ColAgePlus1", "colony_ID"), all.x = TRUE)
	
	nnplus1<-nnplus1[!(nnplus1$colony_age==numGens),] # removes colonies that may still have been alive after sim done
	
	nnplus1$numAdsPlus1[is.na(nnplus1$numAdsPlus1)] <- 0 # replacing the NA's with zeros
	#nnplus1merge <- subset(nnplus1merge, colony_ID == 1) # for testing
	#write.csv(nnplus1merge, file = "nnplus1merge.csv") # for testing

	nnplus1 <- subset(nnplus1, prevDisp != "now")  # removing colonies that have just dispersed
	
	#nnplus1 <- subset(nnplus1, pop_age < numGens - 1 ) # removing nests at the end of generations that might not have died, not sure why i did -1
	
	colnames(nnplus1)[4] <- "N" # changing names so will work in logistic function
	colnames(nnplus1)[1] <- "NPlus1"

	logistic <- tryCatchLogistic(nnplus1)
	
	options(warn= -1)
	
	if (logistic == "error"){
		print ("error in logistic calculation")
		log_list <- c(NA, NA, NA)
	}else{
		logisticTable <- tidy(logistic)
		a <- logisticTable$estimate[(logisticTable$term=="a")]
		b <- logisticTable$estimate[(logisticTable$term=="b")]
		c <- logisticTable$estimate[(logisticTable$term=="c")]
		log_list <- c(a, b, c)
		
	} 
	
	options (warn = 0)
	rowApp<- append(rowVars, log_list)
	
	Log_output[nrow(Log_output) + 1, ] <- rowApp
	return(Log_output)	

}


for (i in (1:length(FilesExist))){	
	print(i)	
	
	num <- FilesExist[i]
	numGens <- fileNames[num, 3]
	print(num)
	
	theFileName <-fileNames[num,1]
	
	print (theFileName)
	
	Log_output <- nntplus1Fun(theFileName, min_pop_age, numGens)
	
	
	
}


write.table(Log_output, paste(folder, outputFile, sep = ""), sep=",", row.names = FALSE)

#curve(I((x^(1+0.4)) * exp(1.5) * (exp(-0.02 * x))), 0, 400) # plots the function

