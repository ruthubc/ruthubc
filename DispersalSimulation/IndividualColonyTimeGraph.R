# TODO: Add comment
# 
# Author: Ruth
###############################################################################

# Graph of one colony over it's age

folder <- "DisperalSimulationOutput/"

file <- read.csv(paste(folder, "3972_slp1_Rsk0.3_K300_var0_dslm0.6_maxOff4.py.csv", sep = ""))

colony <- subset(file, colony_ID == 120)


