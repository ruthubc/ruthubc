
# Author: Ruth
###############################################################################

library(ggplot2)
library(plyr)
library(gridExtra)
library(grid) # not sure if I need this or not.

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


importDispFile <- read.csv("DisperalSimulationOutput/SummaryCombine_17Feb2017.csv", na.strings = "NA")

importDispFile <- merge(importDispFile, CompLookUp, by = "Comp_slope") # making new comp variable with more sensible numbering

importDispFile$metPopAgeMax <- importDispFile$pop_age # changing the name to more sensible one

importDispFile$any_disp <- ifelse(importDispFile$num_cols_disp > 0, 1, 0)


xtabs(~ input_var + Comp_slope + meanK + disp_rsk + ad_dsp_fd + max_no_off,  data = importDispFile)







