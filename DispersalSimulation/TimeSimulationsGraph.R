# Checking how long the python simulations take to work so I can request shorter time so they run faster
# 
# Author: user
###############################################################################
library (plyr)
library(ggplot2)
library(gridExtra)
library(xlsx)

Times <-read.xlsx("RuthSync/DisperalSimulation/Simulations_Need_More_TimeHERMES.xlsx", sheetIndex = 1)


ggplot(Times, aes(x = Slp, y = RunOutTimeBinary, colour = as.factor(dslm))) + geom_point(size = 3, position = position_jitter(w = 0.03, h = 0.0)) + 
		facet_grid(MaxNoOff~var) +  ggtitle("More time needed? ( 0 = no, 1 = yes)") + geom_line() + ylim(0, 1)  + 
		scale_colour_discrete()