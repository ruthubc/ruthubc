# Author: Ruth
###############################################################################

##### formatting data for data acessability 

cols2Incd <- c("BoxAtePrey", "BoxFeedObs", "TrialID", "DateTrial", "TimeOfDay", "SpiderID", "TotalTimeEating", "IndBoxID", 
		"Instar", "Treatment", "LegLen.mm",  "HeadLen.mm", "Weight.1", "IndCapture", "BoxCapture")

########## Remove boxes that didn't feed


BoxTrialsDataAcc <- subset(BoxCombo, select = cols2Incd, BoxAtePrey == "y")  ##WRONG< This exclues eve traisl  

BoxTrialsDataAcc$IndCapture <- as.character(BoxTrialsDataAcc$IndCapture)

BoxTrialsDataAcc<-BoxTrialsDataAcc[!(BoxTrialsDataAcc$BoxFeedObs =="n" & BoxTrialsDataAcc$TimeOfDay	=="morn"),]



## Rename col names to more sensible

colnames(BoxTrialsDataAcc)[which(names(BoxTrialsDataAcc) == "TotalTimeEating")] <- 'TotalTimeEating.mins'

colnames(BoxTrialsDataAcc)[which(names(BoxTrialsDataAcc) == "IndBoxID")] <- 'FeedingGroupID'

colnames(BoxTrialsDataAcc)[which(names(BoxTrialsDataAcc) == "Treatment")] <- 'PreySize'

colnames(BoxTrialsDataAcc)[which(names(BoxTrialsDataAcc) == "Weight.1")] <- 'Weight.mg'

colnames(BoxTrialsDataAcc)[which(names(BoxTrialsDataAcc) == "BoxCapture")] <- 'PreyCaptureObserved'


# put time eating and capture to NA if evening
# Only include trials where they actually ate or where prey capture was observed, some boxes captured but did not eat.


BoxTrialsDataAcc$TotalTimeEating.mins <- ifelse(BoxTrialsDataAcc$TimeOfDay == 'eve', NA, BoxTrialsDataAcc$TotalTimeEating.mins)

BoxTrialsDataAcc$IndCapture <- ifelse(BoxTrialsDataAcc$PreyCaptureObserved == 'n', NA, BoxTrialsDataAcc$IndCapture)
