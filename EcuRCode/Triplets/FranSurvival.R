# TODO: Add comment
# 
# Author: user
###############################################################################


library(ggplot2)
library(plyr)
library(data.table)
library(survival)


Survival <- read.csv("RuthSync/EggManipulation/RuthDataFiles/Extinction.csv", na.strings = NA)

Survival$Rcens <- ifelse(Survival$censored==0, 1, 0)

Survival <- Survival[(Survival$Natural == 1), ]

surv <- survfit(Surv(Survival$ColSurviveWeeks, Survival$Rcens) ~ Survival$Treatment, type = "kaplan-meier") #(It is also reasonable to use several variables on the right side of the equation.

summary(surv)

plot(surv)

survdiff(Surv(Survival$ColSurviveWeeks, Survival$Rcens) ~ Survival$Treatment, rho=0)