# Author: Ruth
###############################################################################

##### formatting data for data acessability 

cols2Incd <- c("TrialID", "DateTrail", "SpiderID", "TotalTimeEating", "IndBoxID", "Instar", "Treatment", "LegLen.mm", "AbdmLen.mm", "HeadLen.mm", "Weight.1")

########## Remove boxes that didn't feed


BoxTrialsDataAcc <- subset(BoxCombo, select = cols2Incd)



## Rename col names to more sensible

colnames(BoxTrialsDataAcc)[which(names(BoxTrialsDataAcc) == "TotalTimeEating")] <- 'TotalTimeEating.mins'

colnames(BoxTrialsDataAcc)[which(names(BoxTrialsDataAcc) == "IndBoxID")] <- 'FeedingGroupID'

colnames(BoxTrialsDataAcc)[which(names(BoxTrialsDataAcc) == "Treatment")] <- 'PreySize'

colnames(BoxTrialsDataAcc)[which(names(BoxTrialsDataAcc) == "Weight.1")] <- 'Weight.mm'
