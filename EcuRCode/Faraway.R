# TODO: Add comment
# 
# Author: Ruth
###############################################################################

library('faraway') #p167

data(irrigation)
# if an interaction term has a random effect in it, it is a random effect

lmod <- lmer (yield~ irrigation * variety + (1|field)  + (1|field:variety) ,data = irrigation)
 logLik(lmod)
 
 data(eggs)
 
 # technician is nested within lab
# samples are nested withni technician

cmod <- lmer(Fat ~ 1 + (1|Lab) + (1|Lab:Technician) + (1|Lab:Technician:Sample), data = eggs)
summary(cmod)

data(jsp)

jspr<-jsp[jsp$year==2,]

jspr$craven <- jspr$raven - mean(jspr$raven)

mmod <- lmer(math ~ craven *social + (1|school) + (1|school:class), jspr)

summary(mmod)

ranef(mmod)$"school:class"[[1]]

schraven <- lm(raven ~ school, jspr)$fit

