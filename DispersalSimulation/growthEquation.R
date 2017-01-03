# TODO: Add comment
# 
# Author: user
###############################################################################


# trying to get the constants for the growth equation

# look up getInital as it supplies some initial values 

# using num_adults instead of num_adsB4_dispersal

library(nlme)
library(ggplot2)

source("FilesExist.R")
filesCSV <- "FilesCreated.csv"

folder <- "DisperalSimulationOutput/Testing/"

fileNames<-read.csv(paste(folder, filesCSV, sep = ""), quote="")# import file names csv file

fileNames[] <- lapply(fileNames, as.character) # making factors into strings

files <- fileExistsFn(fileNames)

min_popAge <-50


## Make empty df to output numbers to

R_output <- data.frame(FileNum = numeric(),
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
		log_c = numeric())


i = 3
print(i)	

num <- files[i]

theFileName <-fileNames[num,1]
numGens <- fileNames[num, 3]
		
		
		
		



rGrowth <- function(fileName, min_pop_age, numGens){
	
	fileToImport <- paste(folder, theFileName, ".py.csv", sep = "")
	File <- read.csv(fileToImport, quote = "")
	
	maxPopAge <- max(File$pop_age)
	
	if ((maxPopAge + 50) < min_popAge){ File <- subset(File, pop_age >= min_popAge) }	## DOESN"T WORK
	
	fileNum <- print (substr(theFileName, 0, 5))
	
	
	rowVars <- c(fileNum, File$Comp_slope[1], File$meanK[1], File$Fd_ln[1], File$input_var[1], File$disp_rsk[1], File$ad_dsp_fd[1], 
			File$min_juv_fd[1], File$max_no_off[1], maxPopAge) # for output to file
	
	
	File$ColAgePlus1 <- File$colony_age
	
	ColInfoPlus1 <- subset(File, select = c("colony_ID", "num_adsB4_dispersal", "colony_age"))
	
	nnplus1 <- subset(File, select = c("colony_ID", "num_adsB4_dispersal", "colony_age"))
	
	
	colnames(ColInfoPlus1)[3] <- "ColAgePlus1"
	colnames(ColInfoPlus1)[2] <- "numAdsPlus1"
	
	nnplus1$ColAgePlus1 <- nnplus1$colony_age + 1
	
	nnplus1 <- merge(nnplus1, ColInfoPlus1,  by =c("ColAgePlus1", "colony_ID"), all.x = TRUE)
	
	nnplus1<-nnplus1[!(nnplus1$colony_age==numGens),] # removes colonies that may still have been alive after sim done
	
	nnplus1$numAdsPlus1[is.na(nnplus1$numAdsPlus1)] <- 0 # replacing the NA's with zeros
	
	
	nnplus1$growth <- (nnplus1$numAdsPlus1 - nnplus1$num_ads) /nnplus1$num_ads
	
	nnplus1$r_trans <- log(nnplus1$growth + 2)
	
	ggplot(data = nnplus1, aes(x=num_adsB4_dispersal, y = growth)) + stat_smooth(se = FALSE)  + geom_point()
	
	
	grwthRte <- nls(NPlus1 ~ EQNHERE , data = nnplus1, start = list(a=0.4, b=1.5, c=0.02), 
			lower=list(a=0, b=0,c=0), algorithm= "port", trace = T)


	
	
}	