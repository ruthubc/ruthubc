# TODO: Add comment
# 
# Author: Ruth
###############################################################################

#info from here http://anson.ucdavis.edu/~hiwang/teaching/11fall/R_tutorial%204.pdf

library(survival)

survivalTest <-  read.csv(paste("DisperalSimulationOutput/SurvivalTest.csv", sep = ""))

#check number of colonies
max(survivalTest$colony_ID)

# remember in the real data I remove the first 100 generations so have to think about how to deal with that, perhpas not remove generations??

survivalSub <- subset(survivalTest, colony_ID > 10000 , select = c(pop_age, colony_ID, colony_age, colAlive))

#write.csv(survivalSub, file = "DisperalSimulationOutput/SurvivalTestSubset.csv" )

survivalSub <- subset(survivalSub, colAlive == "dead" | pop_age ==500  )

msurv <- with(survivalSub, Surv(colony_age, colAlive =="alive")) # creates survival object, check i have it the write way around
print(msurv)

mean(msurv[,1]) # don't use mean by itself, wrong!

summary(msurv)

fit <- survfit(data = survivalSub, Surv(colony_age, colAlive == "Alive"))

fit <- survfit(msurv ~1)

plot(fit)