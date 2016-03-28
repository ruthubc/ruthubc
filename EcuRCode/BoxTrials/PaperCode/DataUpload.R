# Author: Ruth
###############################################################################

##### formatting data for data acessability 

cols2Incd <- c("TrialID", "DateTrail", "TimeOfDay", "SpiderID", "TotalTimeEating", "IndBoxID", "Instar", "Treatment", "LegLen.mm", "AbdmLen.mm", "HeadLen.mm", "Weight.1")

########## Remove boxes that didn't feed


BoxTrialsDataAcc <- subset(BoxCombo, select = cols2Incd)



## Rename col names to more sensible

colnames(BoxTrialsDataAcc)[which(names(BoxTrialsDataAcc) == "TotalTimeEating")] <- 'TotalTimeEating.mins'

colnames(BoxTrialsDataAcc)[which(names(BoxTrialsDataAcc) == "IndBoxID")] <- 'FeedingGroupID'

colnames(BoxTrialsDataAcc)[which(names(BoxTrialsDataAcc) == "Treatment")] <- 'PreySize'

colnames(BoxTrialsDataAcc)[which(names(BoxTrialsDataAcc) == "Weight.1")] <- 'Weight.mg'


# put time eating and capture to NA if evening
# Only include trials where they actually ate or where prey capture was observed, some boxes captured but did not eat.