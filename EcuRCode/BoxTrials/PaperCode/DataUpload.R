# Author: Ruth
###############################################################################

##### formatting data for data acessability 

cols2Incd <- c("TrialID", "DateTrial", "IndBoxID", "Instar", "Treatment", "BoxFeedObs", "BoxCapture",  "SpiderID", 
		"HeadLen.mm", "Weight.1", "TotalTimeEating",  "CaptureIndPos" )

########## Remove boxes that didn't feed


BoxTrialsDataAcc <- subset(BoxComboMorn, select = cols2Incd)  ##WRONG< This exclues eve traisl  






## Rename col names to more sensible

colnames(BoxTrialsDataAcc)[which(names(BoxTrialsDataAcc) == "TotalTimeEating")] <- 'IndTimeEating.mins'

colnames(BoxTrialsDataAcc)[which(names(BoxTrialsDataAcc) == "BoxFeedObs")] <- 'PreyConsumedByBox?'

colnames(BoxTrialsDataAcc)[which(names(BoxTrialsDataAcc) == "Treatment")] <- 'PreySize'

colnames(BoxTrialsDataAcc)[which(names(BoxTrialsDataAcc) == "Weight.1")] <- 'Weight.mg'

colnames(BoxTrialsDataAcc)[which(names(BoxTrialsDataAcc) == "BoxCapture")] <- 'PreyCaptureObservedInBox?'

colnames(BoxTrialsDataAcc)[which(names(BoxTrialsDataAcc) == "CaptureIndPos")] <- 'IndCapturedPrey?'




write.csv(BoxTrialsDataAcc, file = "RuthEcuador2013/BoxFeedingTrials/Sharpe_PreySizeCompetition.csv")
