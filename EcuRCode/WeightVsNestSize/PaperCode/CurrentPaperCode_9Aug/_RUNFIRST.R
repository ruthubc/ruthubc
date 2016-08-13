# TODO: Add comment
# 
# Author: user
###############################################################################

library(lmerTest)
library(plyr)
library(dplyr)
require(reshape)
library(SciencesPo)
library(stringr)
library(qdap)
library(tidyr)
library(MASS)
library(visreg)
library(grid)
library(knitr)
library(doBy)
library(xtable)
library(pander)



comp_name <- Sys.info()["nodename"]


if (comp_name == "DELL-1545") {
	print("You are using your home computer")
	path <- "C:/Work/EclipseNeonWorkspace/ruthubc/EcuRCode/WeightVsNestSize/PaperCode/CurrentPaperCode_9Aug/"
	

	
	print("data loaded")
}else{
	print("You are using your school computer")
	setwd("G:/Dropbox/")
	path <- "G:/EclipseNeonWorkspace/R_Code/ruthubc/EcuRCode/WeightVsNestSize/PaperCode/CurrentPaperCode_9Aug/"

}


source(paste(path, "NestSizeData-Paper.R", sep = ""))
source(paste(path, "CondResidualFunction.R", sep = ""))
source(paste(path, "FunctionCalculateVariance_overMax.R", sep = "")) # importing function
source(paste(path,"InstarSizeGridGraphFunction.R", sep = ""))
source(paste(path,"ModelSelectionFunction.R", sep = ""))
source(paste(path, "Function_MDAnovaOutput.R", sep = ""))

print("data imported")