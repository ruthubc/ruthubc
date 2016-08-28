# TODO: Add comment
# 
# Author: user
###############################################################################


BootVar <- read.csv("RuthSync/BootStrapVarianceForCluster/OutputGrex19Aug/combined25Aug.csv")

BootVar$bootSD_cond_trans <- log(BootVar$bootSD_cond + 0.001) # much more normal

ggplot(BootVar, aes(bootSD_cond_trans)) + geom_histogram()

ggplot(BootVar, aes(bootSD_cond_trans, sd_data)) + geom_point()

ggplot(BootVar, aes(N, bootSD_cond_trans)) + geom_point()


InstarGridGraph(subset(BootVar, type = "multiple"), "bootSD_cond_trans", "condition variance", "n", "", "", "n")

model <- lmer(bootSD_cond_trans ~ InstarSex + InstarNumber + logCtFm + (1|NestID), data = BootVar)

anova(model)

modPred <- c("InstarSex:InstarNumber", "InstarSex:logCtFm", "logCtFm", "InstarNumber:logCtFm", "InstarNumber:logCtFm:InstarSex")

allModels <- allModelsAICWithSex("bootSD_cond_trans", modPred, BootVar)

mySpiders <- subset(spiders, !is.na(condResiduals))

mySpiders <- ddply(mySpiders, "Instar", transform, maxCond = max(condResiduals), minCond = min(condResiduals)) # calculating max and min

