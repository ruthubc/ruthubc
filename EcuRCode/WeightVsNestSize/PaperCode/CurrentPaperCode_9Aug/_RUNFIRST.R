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
	
	source("C:/Work/EclipseNeonWorkspace/ruthubc/EcuRCode/WeightVsNestSize/PaperCode/CurrentPaperCode_9Aug/NestSizeData-Paper.R")
	source("C:/Work/EclipseNeonWorkspace/ruthubc/EcuRCode/WeightVsNestSize/PaperCode/CurrentPaperCode_9Aug/CondResidualFunction.R")
	source("C:/Work/EclipseNeonWorkspace/ruthubc/EcuRCode/WeightVsNestSize/PaperCode/CurrentPaperCode_9Aug/FunctionCalculateVariance_overMax.R") # importing function
	source("C:/Work/EclipseNeonWorkspace/ruthubc/EcuRCode/WeightVsNestSize/PaperCode/CurrentPaperCode_9Aug/InstarSizeGridGraphFunction.R")
	
	print("data loaded")
}else{
	print("You are using your school computer")
	source("G:/EclipseWorkspace/Python/ruthubc/EcuRCode/WeightVsNestSize/PaperCode/NestSizeData-Paper.R")
	source("G:/EclipseWorkspace/Python/ruthubc/EcuRCode/WeightVsNestSize/PaperCode/CondResidualFunction.R")
	source("G:/EclipseWorkspace/Python/ruthubc/EcuRCode/WeightVsNestSize/PaperCode/FunctionCalculateVariance_overMax.R") # importing function
	source("G:/EclipseWorkspace/Python/ruthubc/EcuRCode/WeightVsNestSize/PaperCode/InstarSizeGridGraphFunction.R")
	
}


