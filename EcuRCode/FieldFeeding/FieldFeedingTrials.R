# TODO: Add comment
# 
# Author: Ruth
###############################################################################
##Field feeding trials

Fld_Feed <- read.csv("RuthEcuador2013/FieldFeedingTrials/FeedingData.csv", na.strings = NA)
Fld_Trials<-read.csv("RuthEcuador2013/FieldFeedingTrials/IndTrials.csv", na.strings = NA)
Fld_Prey <- read.csv("RuthEcuador2013/FieldFeedingTrials/PreyCapture.csv", na.strings = NA)
Fld_Census <- read.csv("RuthEcuador2013/FieldFeedingTrials/FieldFeedingCensus.csv", na.strings = NA)

Fld_Combo<- merge(Fld_Feed, Fld_Trials, by = (c("TrialID")))

Fld_Feed$Instar <- factor(Fld_Feed$Instar, levels =c("AdFem", "Sub2", "Sub1", "Juv34", "Juv12", "Male"))
levels(Fld_Feed$Instar)

ggplot(Fld_Combo, aes(x= Instar, y = TotalTimeFeed)) + geom_boxplot() + facet_wrap(~Size, scales = "free_y")