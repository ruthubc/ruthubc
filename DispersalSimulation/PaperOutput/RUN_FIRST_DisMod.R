

library(ggplot2)
library(plyr)
library(gridExtra)
library(grid) # not sure if I need this or not.

#NOTE:
## When running this with new  competition slopes, make sure that it updates the comp for the graphs correctly with the lookup table
CompLookUp <- data.frame (Comp_slope = c(0, 0.2, 0.4, 0.6, 0.8, 1, 1.25, 1.33, 2.5, 5, 10), 
		Comp_meas = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))


# importing the data
comp_name <- Sys.info()["nodename"]

if (comp_name == "DELL-1545") {
	print("You are using your home computer")
	setwd("C:/Work/ownCloud/")

}else if (comp_name == "MACPC") {
	print("You are using your mac PC")
	setwd("C:/Users/Ruth/ownCloud")
	
	


}else{
	print("You are using your school computer")
	setwd("G:/ownCloud")
	
}


folder <- "DisperalSimulationOutput/"

dis_aves <- read.csv(paste(folder, "DispersalSummary27Sept_DataJulyCombine.csv", sep = ""))

## Number of genearation to remove from the start of the simulations
numGensRmv <- 50


dis_aves$full <- ifelse(dis_aves$pop_age > numGensRmv, 1, 0) # marking which colonies survived over num generations and which didn't

#Marking if colony produced dispersers
dis_aves$any_disp <- ifelse(dis_aves$num_cols_disp > 0, 1, 0)

## Making binary measurement of colony death
dis_aves$all_death_bin<- ifelse(dis_aves$num_cols_die > 0, 1, 0)


nrow(dis_aves[duplicated(dis_aves$fileNum),]) # checking there are no duplicated file numbers


dis_aves <- merge(dis_aves, CompLookUp, by = "Comp_slope") # making new comp variable with more sensible numbering

## changing colony age at death to 500 for those populations that had no deaths
dis_aves$ave_colAge_Death[is.na(dis_aves$ave_colAge_Death) & dis_aves$pop_age > numGensRmv] <- 500

dis_aves$numIndsDispersing <- dis_aves$all_ave_num_disp * dis_aves$all_tot_num_cols

dis_aves$numIndsDispersing <- ifelse(is.na(dis_aves$numIndsDispersing) == TRUE, 0,  dis_aves$numIndsDispersing)

dis_ply<- ddply(dis_aves, .(Comp_slope, meanK, input_var, disp_rsk, ad_dsp_fd, Comp_meas, max_no_off), summarise, # need to discount trials where no feeding obs and eve
		N_all = length(!is.na(fileNum)),
		N = length(fileNum[which(!is.na(tot_num_cols))]),
		PopAge.Mean = mean(pop_age, na.rm=TRUE),
		PopAge.SE = sd(pop_age, na.rm = TRUE) / sqrt(N_all), 
		maxColAge.mean = mean(max_colAge, na.rm = TRUE),
		maxColAge.SE = sd(max_colAge, na.rm = TRUE)/sqrt(N),
		col_size_disp.mean = mean(col_size_disp, na.rm  = TRUE),
		col_size_disp.SE = sd(col_size_disp, na.rm = TRUE)/sqrt(N),
		colSizeB4Disp.mean = mean(ave_colSizeB4Disp, na.rm = TRUE),
		colSizeB4Disp.SE = sd(ave_colSizeB4Disp, na.rm = TRUE) / sqrt(N),
		colAgeDeath_all.mean = mean(all_ave_colAge_Death, na.rm = TRUE),
		colAgeDeath_all.SE = sd(all_ave_colAge_Death, na.rm = TRUE)/ sqrt(N_all),
		colDeath_bin_all.mean = mean(all_death_bin),
		colSizeDeath_all.mean = mean(all_ave_colSize_Death, na.rm = TRUE),
		pcntDisp.mean = mean(ave_perDisp, na.rm = TRUE),
		pcntDisp.SE = sd(ave_perDisp, na.rm = TRUE), sqrt(N),
		colSizeDeath.mean = mean(ave_colSize_Death, na.rm = TRUE),
		colSizeDeath.SE = sd(ave_colSize_Death, na.rm = TRUE)/ sqrt(N),
		survival_all.mean = mean(survivalMean_all, na.rm = TRUE),
		survival_all.SE = sd(survivalMean_all, na.rm = TRUE)/ sqrt(N_all),
		anyDisp.mean = mean(any_disp, na.rm = TRUE),
		propSigNestsGrow.mean = mean((1-propSigNestNotGrow), na.rm = TRUE),
		MeanPopSize = mean(MeanNoCols, na.rm = TRUE),
		MeanDispFreq = mean(mean_freq_disp, na.rm = TRUE),
		MeanPerColsDisp = mean(perc_cols_disp, na.rm = TRUE),
		MeanPerDieNoDsp = mean(perc_die_no_disp, na.rm = TRUE),
		MeanPopSizeVar = mean(var_pop_size, na.rm = TRUE),
		MeanPopSizeVarNoSig = mean(var_pop_size_noSig, na.rm = TRUE),
		TotNumDispersingAll = mean(numIndsDispersing, na.rm = TRUE)
)



dis_ply$dipBinary <- ifelse(dis_ply$TotNumDispersingAll > 0, "SomeDispersers", "NoDispersers")
