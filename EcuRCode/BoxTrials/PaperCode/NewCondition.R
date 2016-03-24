# Author: Ruth
###############################################################################
# Residual Index Condition

require(reshape2)
library (ggplot2)
library (grid)
library (lme4)
library(lmerTest)
library(visreg)

#calculating the residual index
ggplot(spiders, aes(logLeg, logWt)) + geom_point() + geom_smooth(method=lm)# + facet_wrap(~Instar,  scales = "free")
#ggplot(spiders, aes(lnLeg, lnWt)) + geom_point() + geom_smooth(method=lm, fullrange = TRUE) # log or nat log doesn't make a difference

model <- lm(logWt ~ logLeg, spiders ) # doesn't matter which way round this is
#model <- lm(logWt ~ logLeg + Instar + logLeg:Instar, spiders ) # whichever one i use doesn't make a difference

visreg(model, xvar = "logLeg", by = "Instar" )


summary(model)
spiders$condResiduals <- resid(model)  # putting the residuales into the dable
spidersMult <- subset(spiders, type== 'multiple') # remving single nests

ggplot(spiders, aes(condResiduals, fill = Instar)) + geom_histogram() # normal

# plotting res condition against leg length
ggplot(spiders, aes(x = condResiduals, y = logLeg, colour = Instar)) + geom_point()

# plotting res condition against weight
ggplot(spiders, aes(x = condResiduals, y = logWt, colour = Instar)) + geom_point() + geom_smooth(method=lm, fullrange = TRUE)


