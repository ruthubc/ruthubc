# TODO: Add comment
# 
# Author: user
###############################################################################

library(lmerTest)
library(plyr)
library(dplyr)
require(reshape)
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
library(gridExtra)
library(survival)
#library(muhaz)
library(ggplot2)
library(survminer)
library(stargazer)
library(formula.tools)
library(MuMIn)
library(car)
library(texreg)





comp_name <- Sys.info()["nodename"]


if (comp_name == "DELL-1545") {
	print("You are using your home computer")
	setwd("C:/Work/Dropbox/")
	path <- "C:/Work/EclipseNeonWorkspace/ruthubc/EcuRCode/WeightVsNestSize/PaperCode/CurrentPaperCode_9Aug/"
	
}else{
	print("You are using your school computer")
	setwd("G:/Dropbox/")
	path <- "G:/EclipseNeonWorkspace/R_Code/ruthubc/EcuRCode/WeightVsNestSize/PaperCode/CurrentPaperCode_9Aug/"

}



source(paste(path, "CondResidualFunction.R", sep = ""))
source(paste(path, "FunctionCalculateVariance_overMax.R", sep = "")) # importing function
source(paste(path,"InstarSizeGridGraphFunction.R", sep = ""))
source(paste(path,"ModelSelectionFunction.R", sep = ""))
source(paste(path, "Function_MDAnovaOutput.R", sep = ""))
source(paste(path, "NestSizeData-Paper.R", sep = ""))
source(paste(path, "Function_AnovaResultsOutputDocx.R", sep = ""))
source(paste(path, "Function_ModelFitCheck.R", sep = ""))
source(paste(path, "Function_glmmPQL.R", sep = ""))

mytheme <-theme(plot.title = element_text(vjust=2), panel.margin= unit(0.75, "lines"), axis.title.y = element_text(vjust=0),
		plot.margin=unit(c(1,1,1.5,1.2),"cm"), panel.border = element_rect(fill = NA, colour = "grey", linetype=1, size = 1), 
		panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Francisco's insect biomass data
#Biomass <- read.csv("RuthSync/EggManipulation/FranciscoData_insectBiomass.csv")
#Biomass <- subset(Biomass, AdFemales > 0)  # removing the nests with no females
#Biomass <- subset(Biomass, Treatment == "Control")
#Biomass <- subset(Biomass, OverallID != 5) # removing an outlier that must be a mistake
#Biomass$log10AdFm <- log10(Biomass$AdFemales)

#Biomass$biomass_mg <- Biomass$Tl_insect_biomass *1000
#Biomass$Lg10_biomass <- log10(Biomass$biomass_mg+1)

#Biomass$BiomsPerAdFm <- Biomass$Lg10_biomass/Biomass$log10AdFm

# Condition variance
condBootVar <- read.csv("RuthEcuador2013/NestSize/bootSampCondPython_cond_combined.csv")
legBootVar <- read.csv("RuthEcuador2013/NestSize/bootSampCondPython_leg_combined.csv")


print("data imported")