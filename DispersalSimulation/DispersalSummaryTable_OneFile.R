# TODO: Add comment
# 
# Author: Ruth
###############################################################################


library (plyr)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(survival)
source("FilesExist.R")

### Something to think about. Are there some variables where I don't want to include colonies that are alive at the end of the simulation?

filesCSV <- "FilesCreated.csv"

outputFile <- "DispersalTest.csv"

#folder <- "R_Graphs/"
folder <- "DisperalSimulationOutput/" #########################################################################################################################

fileNames<-read.csv(paste(folder, filesCSV, sep = ""), quote="")# import file names csv file

fileNames[] <- lapply(fileNames, as.character) # making factors into strings

## Change this to the correct number i want to remove
min_popAge <-50 # the number of generations to discount from the start of the calculations ###########################################################################


summaryFun <- function(theFileName, min_pop_age, numGens){

		fileNum <- print (substr(theFileName, 0, 4))

		fileToImport <- paste(folder, theFileName, ".py.csv", sep = "") ###########################################################################################
		#fileToImport <- paste(theFileName, ".py.csv", sep = "")

		
		File <- read.csv(fileToImport, quote = "")
		print (fileToImport)
		
		maxPopAge <- max(File$pop_age)
		
		
		######################## Single female nests ##################
		# seeing if single female nests were able to grow
		
		sigNestsLst <- subset(File, num_adsB4_dispersal == 1 & pop_age != numGens & pop_age != 1)$colony_ID  # list of colony_ID of all single female nests
		
		if(length(sigNestsLst) == 0){
			propSigNestNotGrow <- NA
		
		}else{
		
		sigNestsLst <- unique(sigNestsLst)  # check this works
		
		numSigNest <- length(sigNestsLst)  # number of nests that were at one time single
		
		sigNestsDF <- File[,c("colony_ID","num_adsB4_dispersal")]  # makes two column table containing everything
		
		sigNestsDF <- sigNestsDF[sigNestsDF$colony_ID %in% sigNestsLst,] # subsets the file for those nests that were single once i.e. in sigNestsLst
		
		## gets the max colony size of each nest that stated out as a single nest
		maxRows <- by(sigNestsDF, sigNestsDF$colony_ID, function(X) X[which.max(X$num_adsB4_dispersal),])
		maxRows <- do.call("rbind", maxRows)
		
		numNotGrow <- nrow(subset(maxRows, maxRows$num_adsB4_dispersal == 1))  # this is the number of colonies that never grew beyond one female
		
		propSigNestNotGrow <- numNotGrow/numSigNest
		
		
		rm(maxRows)
		rm(sigNestsDF)
		
		}
		
		
		rm(sigNestsLst)
		
		# getting data on dispersers
		DispersersAfAge <- subset(File, dispersers >0 & pop_age >= min_popAge)
		
		
		if (nrow(DispersersAfAge) == 0){
			
			DispAves <- data.frame(	
					age_first_disp = NA,
					se_age_first_disp = NA,
					mean_num_col_disp = NA,
					se_num_col_disp = NA,
					mean_fstDis_col_size = NA,
					se_fstDis_col_size = NA
			) # makes a tale of NA's if no inds disp
			
			DispersersAll <- subset(File, dispersers >0)
			
			if(nrow(DispersersAll) > 0 ){
				print("only disperses below min pop age")
				anyDisp <- 0.5 # 0.5 if only dispersers when pop below min pop age
			}else{
				anyDisp <- 0
				print("no dispersers at all")
			}
			
			
		}else{
			print ("some dispersers")
			
			anyDisp <- 1
			
			DispersersAfAge$ID <- seq.int(nrow(DispersersAfAge))
			
			DispAvesByColID <- ddply(DispersersAfAge, .(colony_ID), summarise,
					min_disp_age = min(colony_age),
					fstDis_col_size = (num_adsB4_dispersal[colony_age == min_disp_age]),
					count_col_disp = length(ID)
			)
			
			
			DispAves <- ddply(DispAvesByColID, .(), summarise,
					age_first_disp = mean(min_disp_age),
					se_age_first_disp = sd(min_disp_age)/sqrt(length(min_disp_age)),
					mean_num_col_disp = mean(count_col_disp),
					se_num_col_disp = sd(count_col_disp), sqrt(length(count_col_disp)),
					mean_fstDis_col_size = mean(fstDis_col_size),
					se_fstDis_col_size = sd(fstDis_col_size), sqrt(length(fstDis_col_size))
			)
			
			DispAves <- subset(DispAves, select = c(
							age_first_disp,
							se_age_first_disp,
							mean_num_col_disp,
							se_num_col_disp,
							mean_fstDis_col_size,
							se_fstDis_col_size
					
					))
			
		}
		
		
		
		
		##################################
		
		# getting averages for all records, no generations removed
		FileAves_All<- ddply(File, .(Comp_slope, meanK, Fd_ln, input_var, disp_rsk, ad_dsp_fd, min_juv_fd, min_no_off,
						max_no_off, min_ad_sze_off, max_ad_sze_off), summarise,
				pop_age = max(pop_age),
				all_tot_num_cols = max(colony_ID),
				all_ave_colAge = mean(colony_age),
				all_se_colAge = sd(colony_age)/sqrt(length(colony_age)),
				all_min_colAge = min(colony_age),
				all_max_colAge = max(colony_age),
				all_ave_num_disp= mean(dispersers[dispersers!=0]),
				all_se_num_disp = sd(dispersers[dispersers!=0])/sqrt(length(dispersers[dispersers!=0])),
				all_ave_perDisp= mean((dispersers/num_adsB4_dispersal)[dispersers!=0]),
				all_se_perDisp = sd((dispersers/num_adsB4_dispersal)[dispersers!=0])/sqrt(length(dispersers[dispersers!=0])),
				all_ave_colSizeB4Disp = mean(num_adsB4_dispersal),
				all_se_colSizeB4Disp  = sd(num_adsB4_dispersal)/sqrt(length(num_adsB4_dispersal)),
				all_max_colSizeB4Disp = max(num_adsB4_dispersal),
				all_ave_colSize_Death =  mean(num_adsB4_dispersal[colAlive=="dead"]),
				all_se_colSize_Death = sd(num_adsB4_dispersal[colAlive=="dead"])/sqrt(length(num_adsB4_dispersal[colAlive=="dead"])),
				all_ave_colAge_Death = mean(colony_age[colAlive=='dead']),
				all_se_colAge_Death = sd(colony_age[colAlive=="dead"])/sqrt(length(colony_age[colAlive=="dead"])),
				all_col_size_disp = mean(num_adsB4_dispersal[dispersers>0]),
				all_se_age_first_disp = sd(num_adsB4_dispersal[dispersers>0])/sqrt(length(num_adsB4_dispersal[dispersers>0])),
				all_mean_ad_sze = mean(adSz_B4_mean, na.rm = TRUE),
				all_mean_juv_sze = mean(jvSz_B4_mean, na.rm = TRUE)
		)
		
		# getting survival mean for all colonies
		
		survivalSub <- subset(File, colAlive == "dead" | pop_age == numGens)
		msurv <- with(survivalSub, Surv(colony_age, colAlive =="dead")) # getting the survival thing
		survivalMean <- as.data.frame(mean(msurv[,1])) # don't use mean without the [,1] by itself, wrong!
		
		names(survivalMean)[1] <- "survivalMean_all"

	
		
		
		if(maxPopAge < min_popAge){ # removing first 100 generations

			print("pop did not survive to input number of generations") # inputs NA's as the population did not survive long enough
			
			FileAves <- data.frame(
					tot_num_cols = NA,
					ave_colAge = NA,
					se_colAge = NA,
					min_colAge = NA,
					max_colAge = NA,
					ave_num_disp= NA,
					se_num_disp = NA,
					ave_perDisp= NA,
					se_perDisp = NA,
					ave_colSizeB4Disp = NA,
					se_colSizeB4Disp  = NA,
					max_colSizeB4Disp = NA,
					ave_colSize_Death =  NA,
					se_colSize_Death = NA,
					ave_colAge_Death = NA,
					se_colAge_Death = NA,
					col_size_disp =NA,
					se_age_first_disp = NA,
					mean_ad_sze = NA,
					mean_juv_sze = NA 
			)
		}else{

			File <- subset(File, pop_age >= min_popAge) # removing the first x number of gens before do cals

		
		
		FileAves<- ddply(File, .(), summarise,
				tot_num_cols = max(colony_ID),
				ave_colAge = mean(colony_age),
				se_colAge = sd(colony_age)/sqrt(length(colony_age)),
				min_colAge = min(colony_age),
				max_colAge = max(colony_age),
				ave_num_disp= mean(dispersers[dispersers!=0]),
				se_num_disp = sd(dispersers[dispersers!=0])/sqrt(length(dispersers[dispersers!=0])),
				ave_perDisp= mean((dispersers/num_adsB4_dispersal)[dispersers!=0]),
				se_perDisp = sd((dispersers/num_adsB4_dispersal)[dispersers!=0])/sqrt(length(dispersers[dispersers!=0])),
				ave_colSizeB4Disp = mean(num_adsB4_dispersal),
				se_colSizeB4Disp  = sd(num_adsB4_dispersal)/sqrt(length(num_adsB4_dispersal)),
				max_colSizeB4Disp = max(num_adsB4_dispersal),
				ave_colSize_Death =  mean(num_adsB4_dispersal[colAlive=="dead"]),
				se_colSize_Death = sd(num_adsB4_dispersal[colAlive=="dead"])/sqrt(length(num_adsB4_dispersal[colAlive=="dead"])),
				ave_colAge_Death = mean(colony_age[colAlive=='dead']),
				se_colAge_Death = sd(colony_age[colAlive=="dead"])/sqrt(length(colony_age[colAlive=="dead"])),
				col_size_disp = mean(num_adsB4_dispersal[dispersers>0]),
				se_age_first_disp = sd(num_adsB4_dispersal[dispersers>0])/sqrt(length(num_adsB4_dispersal[dispersers>0])),
				mean_ad_sze = mean(adSz_B4_mean, na.rm = TRUE),
				mean_juv_sze = mean(jvSz_B4_mean, na.rm = TRUE)
		)
		
	}	
		
	
		
		rm(File)
		average <- cbind(FileAves_All, FileAves, DispAves, anyDisp, survivalMean, propSigNestNotGrow)
		average$.id <- NULL
		average$fileNum <- fileNum
		return(average)
	
	
}


files <- fileExistsFn(fileNames)

numFiles<- length(files)

print ("the number of files that exist:")
print (numFiles)

			
for (i in 1:numFiles){
	print(i)	

	num <- files[i]
	
	theFileName <-fileNames[num,1]
	numGens <- fileNames[num, 3]
	
	#print ("num gens")
	#print (numGens)

	#print (theFileName)
			
	if (i ==1){
	output <- summaryFun(theFileName, min_popAge, numGens)
	} else {
	output1 <- summaryFun(theFileName, min_popAge, numGens)
	output <- rbind(output, output1)
	}
	
}


#stopCluster(cl)
write.table(output, paste(folder, outputFile, sep = ""), sep=",", row.names = FALSE)


