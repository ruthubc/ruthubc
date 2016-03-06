# TODO: Add comment
# 
# Author: Ruth
###############################################################################


library (plyr)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(doParallel)

cl <- makeCluster(2, outfile = "")# setting up the cluster
# Register cluster
registerDoParallel(cl)


filesCSV <- "FilesCreated.csv"

outputFile <- "DispersalAvesTest.csv"

#folder <- "R_Graphs/"
folder <- "DisperalSimulationOutput/"

fileNames<-read.csv(paste(folder, filesCSV, sep = ""), quote="")# import file names csv file

fileNames[] <- lapply(fileNames, as.character) # making factors into strings

# for testing
#theFileName <- fileNames[16,1]


min_popAge <-100 # the number of generations to discount from the start of the calculations




fileExistsFn <- function(filesCreatedcsv){ 	#checking whether files exist and returing a list of existing files
	
	filesThatExist <- c()
	
	
	for (i in 1:nrow(fileNames)){
		theFileName <-fileNames[i,1]
		
		#fileToImport <- paste(theFileName, ".py.csv", sep = "")
		#indFileToImport <- paste(theFileName, ".py_inds.csv", sep = "")
		fileToImport <- paste(folder, theFileName, ".py.csv", sep = "")
		indFileToImport <- paste(folder, theFileName, ".py_inds.csv", sep = "")
		
		
		if(file.exists(fileToImport) == "TRUE"){
			
			print (paste("file exists:", fileToImport))
			filesThatExist <- c(filesThatExist,  i)
			
		}else{
			print (paste("file doesn't exist. File:", fileToImport))
		}
		
	}	
	return (filesThatExist)
}


summaryFun <- function(fileName, min_popAge){

		
	
		fileToImport <- paste(folder, theFileName, ".py.csv", sep = "")

		#fileToImport <- paste(theFileName, ".py.csv", sep = "")

	
	## Aves without first gens removed
	
	FileAvesAllGens<- ddply(File, .(Comp_slope, meanK, Fd_ln, input_var, disp_rsk, ad_dsp_fd, min_juv_fd, min_no_off,
					max_no_off, min_ad_sze_off, max_ad_sze_off), summarise,
			all_pop_age = max(pop_age),
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
			all_se_age_first_disp = sd(num_adsB4_dispersal[dispersers>0])/sqrt(length(num_adsB4_dispersal[dispersers>0]))
	)
	
		
		File <- read.csv(fileToImport, quote = "")
		print (fileToImport)
		
		### TODO: make ave without gens removed
		
		maxPopAge <- max(File$pop_age)
		
		if(maxPopAge < min_popAge){ # removing first 100 generations
			fn_min_popAge <- 0
			print("pop did not survive to set num generations")
			
			FileAves <- data.frame(	tot_num_cols = NA,
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
					se_age_first_disp = NA
			)
			
			#TODO: Make all field names 
		}else{
			fn_min_popAge <- min_popAge
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
				se_age_first_disp = sd(num_adsB4_dispersal[dispersers>0])/sqrt(length(num_adsB4_dispersal[dispersers>0]))
		)
		
	}
		# getting data on dispersers
		Dispersers <- subset(File, dispersers >0)
		
		if (nrow(Dispersers) == 0){
			
			print ("no dispersers")
			DispAves <- data.frame(	
					age_first_disp = NA,
					se_age_first_disp = NA,
					mean_num_col_disp = NA,
					se_num_col_disp = NA,
					mean_fstDis_col_size = NA,
					se_fstDis_col_size = NA
			) # makes a tale of NA's if no inds disp
			
			
		}else{
			print ("some dispersers")
			
			Dispersers$ID <- seq.int(nrow(Dispersers))
			
			DispAvesByColID <- ddply(Dispersers, .(colony_ID), summarise,
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
			
			rm(DispAvesByColID)		
			rm(Dispersers)
			
		}
		
		rm(File)
		
		## Individual Files Summary
		
		indFile <- read.csv(indFileToImport, quote = "")
		
		indAves <- ddply(indFile, .(), summarise,
				ave_juvSze = mean(food[type == "juv"]),
				ave_adSze = mean(food[type == "Ad"])
		)
		
		rm(indFile)
		average <- cbind(FileAves, FileAvesAllGens, DispAves, indAves)
		
		average$.id <- NULL
	
		return(average)
	
	
}


files <- fileExistsFn(fileNames)


#loop <- foreach(i=1:length(files), .combine = "rbind",
			#	.packages= c("ggplot2", "plyr", "gridExtra", "reshape2")) %dopar%{

for (i in (1:length(files))){	
		print(i)	

	num <- files[i]
	
	theFileName <-fileNames[num,1]
	
	print (theFileName)
	output <- summaryFun(theFileName)

	
}


stopCluster(cl)
write.table(loop, paste(folder, outputFile, sep = ""), sep=",", row.names = FALSE)


