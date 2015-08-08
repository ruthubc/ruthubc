# TODO: Add comment
# 
# Author: Ruth
###############################################################################


library (plyr)
library(ggplot2)
library(gridExtra)
library(reshape2)

#folder <- "R_Graphs/"
folder <- "DisperalSimulationOutput/"

fileNames<-read.csv(paste(folder, "FilesCreated.csv", sep = ""), quote="")# import file names csv file

fileNames[] <- lapply(fileNames, as.character) # making factors into strings

# for testing
theFileName <- fileNames[16,1]

counter <-0

min_popAge <-100 # the number of generations to discount from the start of the calculations

#for (i in 1:nrow(fileNames)){
for (i in 1:16){
	print(i)	
	theFileName <-fileNames[i,1]

	fileToImport <- paste(folder, theFileName, ".py.csv", sep = "")
	#fileToImport <- paste(theFileName, ".py.csv", sep = "")
	
	print (fileToImport)
	
	if(file.exists(fileToImport) == "TRUE"){
		print ("file exists")
		counter <- counter + 1 # to make sure that rbind is not run on the first loop
		
		File <- read.csv(fileToImport, quote = "")
		
		maxPopAge <- max(File$pop_age)
		print ("maxPop age")
		print (maxPopAge)
		
		if(maxPopAge < 200){
			fn_min_popAge <- 0
			print("pop did not survive to 200 generations")
		}else{
			fn_min_popAge <- min_popAge
			File <- subset(File, pop_age >= min_popAge) # removing the first x number of gens before do cals
		}
		
		FileAves<- ddply(File, .(Comp_slope, meanK, Fd_ln, input_var, disp_rsk, ad_dsp_fd, min_juv_fd, min_no_off,
						max_no_off, min_ad_sze_off, max_ad_sze_off), summarise,
				pop_age = max(pop_age),
				tot_num_cols = max(colony_ID),
				ave_colAge = mean(colony_age),
				se_colAge = sd(colony_age)/sqrt(length(colony_age)),
				min_colAge = min(colony_age),
				max_colAge = max(colony_age),
				ave_disp= mean(dispersers[dispersers!=0]),
				se_disp = sd(dispersers[dispersers!=0])/sqrt(length(dispersers[dispersers!=0])),
				ave_perDisp= mean((dispersers/num_adsB4_dispersal)[dispersers!=0]),
				se_perDisp = sd((dispersers/num_adsB4_dispersal)[dispersers!=0])/sqrt(length(dispersers[dispersers!=0])),
				ave_colSizeB4Disp = mean(num_adsB4_dispersal),
				se_colSizeB4Disp  = sd(num_adsB4_dispersal)/sqrt(length(num_adsB4_dispersal)),
				max_colSizeB4Disp = max(num_adsB4_dispersal),
				ave_colSize_Death =  mean(num_adsB4_dispersal[colAlive=="dead"]),
				se_colSize_Death = sd(num_adsB4_dispersal[colAlive=="dead"])/sqrt(length(num_adsB4_dispersal[colAlive=="dead"]))
		)
		rm(File)
		
		indFileToImport <- paste(folder, theFileName, ".py_inds.csv", sep = "")
		#indFileToImport <- paste(theFileName, ".py_inds.csv", sep = "")
		indFile <- read.csv(indFileToImport, quote = "")
		
		indAves <- ddply(indFile, .(), summarise,
				ave_juvSze = mean(food[type == "juv"]),
				ave_adSze = mean(food[type == "Ad"])
		)

		rm(indFile)
		average <- cbind(FileAves, indAves)

		average$.id <- NULL
		
		print(counter)
		
		if (counter > 1){ # to make sure that the first run doesn't go to cbind

			dis_aves <- rbind(average, dis_aves)
		} else {
			dis_aves <- average
		}
		
		}else{ print ("file does not exist")}
	}

write.table(dis_aves, paste(folder, "DispersalAves.csv", sep = ""), sep=",", row.names = FALSE)

