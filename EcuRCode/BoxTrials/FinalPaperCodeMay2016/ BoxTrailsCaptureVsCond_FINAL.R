

#####################################################################################
## Condition vs feeding

sink('RuthEcuador2013/BoxFeedingTrials/StatsOutput/CondVsCapture.txt')
date()

BoxTest <- subset(BoxComboMorn, !is.na(residCond) & !is.na(CaptureIndPos))

print("Condition Vs. Capture")

print("SampleSize - num trials")
sampleSize <- xtabs(TrialID ~ Treatment + Instar, aggregate(TrialID~ Treatment + Instar, BoxTest, FUN = function(x) length(unique(x))))
addmargins(sampleSize)
print("")

##### MEANS ETC.###############
print("Cond  Means and StdDevs")
xtabs(residCond ~ Treatment + Instar + CaptureIndPos, aggregate(residCond ~ Treatment + Instar + CaptureIndPos, BoxTest, FUN = function(x) c(mean = mean(x), StdDev = sd(x))))


### Getting the difference in the mean condition between feeders and non-feeders and the standard error of that.

#### LARGE PREY
lenLFd <- length(BoxTest$residCond [BoxTest$CaptureIndPos == "y" & BoxTest$Treatment == "large"])
lenLNonFd <- length(BoxTest$residCond [BoxTest$CaptureIndPos == "n" & BoxTest$Treatment == "large"])

condLFdMn <-mean(BoxTest$residCond [BoxTest$CaptureIndPos == "y" & BoxTest$Treatment == "large"], na.rm = TRUE)
condLNonFdMn <-mean(BoxTest$residCond [BoxTest$CaptureIndPos == "n" & BoxTest$Treatment == "large"], na.rm = TRUE)

print("difference in mean condition for large prey")
condLNonFdMn - condLFdMn # difference in mean condition for large prey

condLFdSD <-(sd(BoxTest$residCond [BoxTest$CaptureIndPos == "y" & BoxTest$Treatment == "large"], na.rm = TRUE) ) / 
		sqrt(lenLFd)
condLNonFdSD <-sd(BoxTest$residCond [BoxTest$CaptureIndPos == "n" & BoxTest$Treatment == "large"], na.rm = TRUE) /
		sqrt(lenLNonFd)
print("Standard error of difference")
sqrt(condLFdSD^ 2 +  condLNonFdSD ^ 2)


#### Small PREY
lenSFd <- length(BoxTest$residCond [BoxTest$CaptureIndPos == "y" & BoxTest$Treatment == "small"])
lenSNonFd <- length(BoxTest$residCond [BoxTest$CaptureIndPos == "n" & BoxTest$Treatment == "small"])

condSFdMn <-mean(BoxTest$residCond [BoxTest$CaptureIndPos == "y" & BoxTest$Treatment == "small"], na.rm = TRUE)
condSNonFdMn <-mean(BoxTest$residCond [BoxTest$CaptureIndPos == "n" & BoxTest$Treatment == "small"], na.rm = TRUE)
print("difference in mean condition for small prey")
condSNonFdMn - condSFdMn # difference in mean condition for small prey

condSFdSD <-(sd(BoxTest$residCond [BoxTest$CaptureIndPos == "y" & BoxTest$Treatment == "small"], na.rm = TRUE) ) / 
		sqrt(lenSFd)
condSNonFdSD <-sd(BoxTest$residCond [BoxTest$CaptureIndPos == "n" & BoxTest$Treatment == "small"], na.rm = TRUE) /
		sqrt(lenSNonFd)
print("Standard error of difference")
sqrt(condSFdSD^ 2 +  condSNonFdSD ^ 2)
print("")



###################### STATISTICS #######################
# removing interactions as they either are insignificant or don't make sense


print("Statistics")
print("Full Model")
CapBinModFull <- glmer(CaptureIndPos ~ residCond + Treatment + Instar + Treatment:residCond +  (1|IndBoxID)+
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))

CapBinModFull@call

summary(CapBinModFull)$coefficients


print("")
print("testingTreatment:Hunger interaction")

CapBinRedModInt <- glmer(CaptureIndPos ~ residCond + Treatment + Instar + (1|IndBoxID) +
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))



anova(CapBinRedModInt, CapBinModFull) #very significant interaction effect

print("AIC difference")
AIC(CapBinRedModInt) - AIC(CapBinModFull)

print("")
print("testing treatment")

CapBinRedModTreatment <- glmer(CaptureIndPos ~ residCond  + Instar + (1|IndBoxID)+
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))


anova(CapBinRedModTreatment, CapBinModFull)

print("AIC difference")
AIC(CapBinRedModTreatment) - AIC(CapBinModFull)


print("")
print("testing Condition")
CapBinRedModCond <- glmer(CaptureIndPos ~ Treatment  + Instar + (1|IndBoxID) +
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))

anova(CapBinRedModCond, CapBinModFull)

print("AIC difference")
AIC(CapBinRedModCond) - AIC(CapBinModFull)



print("")
print("testing instar")
CapBinRedModInstar <- glmer(CaptureIndPos ~ Treatment  + residCond + (1|IndBoxID) +
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))

anova(CapBinRedModInstar, CapBinModFull)

print("AIC difference")
AIC(CapBinRedModInstar) - AIC(CapBinModFull)

## Ad hoc testing 


GLMERS_fun <- function(myData) {
	glmerFull <- glmer(CaptureIndPos ~ residCond + Instar + (1|IndBoxID)+ (1|IndBoxID:SpiderID), 
			data = myData, family = binomial(logit))
	
	glmerRed <- glmer(CaptureIndPos ~ Instar + (1|IndBoxID)+ (1|IndBoxID:SpiderID), 
			data = myData, family = binomial(logit))
	
	lstToReturn <-  list(glmerFull, glmerRed)
	
	names(lstToReturn)[1] <- "Full"
	names(lstToReturn)[2] <- "Reduced"
	
	return(lstToReturn)
	
}


treatGLMER <- dlply(BoxTest, .(Treatment),        
		function(x) GLMERS_fun(x))

printGLMER_fun <- function(glmLst){
	red_model <- glmLst[[2]]
	full_model <- glmLst[[1]]
	
	anv <- anova(full_model, red_model)
	AICs <- AIC(red_model) - AIC(full_model)
	lstToRet <- list(anv, AICs)
	names(lstToRet)[[1]] <- "Anova_RedVsFull"
	names(lstToRet)[[2]] <- "AIC_Diff"
	return(lstToRet)
	
	
}

print("")
print("AdHoc Testing split up by treatment")
test <- lapply(treatGLMER, FUN = function(x) printGLMER_fun(x))# anova(x[[1]], x[[2]]))
print(test)

sink()
