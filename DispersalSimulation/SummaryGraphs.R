# TODO: Add comment
# 
# Author: Ruth
###############################################################################

library(ggplot2)

folder <- "DisperalSimulationOutput/"

dis_aves <- read.csv(paste(folder, "DispersalAves.csv", sep = ""))


# colony age vs stuff - will have to think about whether or how to include colonies that are still alive when the simulation ends\
# could also a survival analysis for indivdual colonies
ggplot(dis_aves, aes(x = Comp_slope, y = ave_colAge, colour = as.factor(input_var) )) + geom_point()