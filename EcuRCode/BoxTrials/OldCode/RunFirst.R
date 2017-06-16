# Statistical analysis of box trial data
# 
# Author: Ruth
###############################################################################
library (lme4)
library(lmerTest) # not sure what excatly this does
library(glmmADMB)
library(ICC)



library(plyr)
library(ggplot2)
require(reshape2)
library(nlme)
library(gridExtra)
require(scales)

mytheme <-theme_bw(base_size=30)  + theme(plot.title = element_text(vjust=2), panel.margin= unit(0.75, "lines"), 
		axis.title.y = element_text(vjust=0), plot.margin=unit(c(1,1,1.5,1.2),"cm"), 
		panel.border = element_rect(fill = NA, colour = "grey", linetype=1, size = 1)) +  theme(strip.background = element_rect(fill = 'white'))




#source("G:/PhDWork/EclipseWorkspace/R/EcuRCode/BoxTrials/BoxTrialsData.R")
#source("G:/PhDWork/EclipseWorkspace/R/EcuRCode/OverDispersionFunction.R")


#source("G:/PhDWork/EclipseWorkspace/R/EcuRCode/BoxTrials/BoxTrialsData.R")
#source("G:/PhDWork/EclipseWorkspace/R/EcuRCode/OverDispersionFunction.R")
######### Overdisperson function from 'http://glmm.wikidot.com/faq'






















