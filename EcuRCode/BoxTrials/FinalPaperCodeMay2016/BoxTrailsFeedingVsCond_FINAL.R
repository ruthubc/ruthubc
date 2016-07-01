
#####################################################################################
## Condition vs feeding



sink('RuthEcuador2013/BoxFeedingTrials/StatsOutput/CondVsFeeding.txt')
date()

BoxTest <- subset(BoxComboMorn, !is.na(residCond) & !is.na(FeedIndPos))

print("Feeding vs Condition")
print("")

print("SampleSize - num trials")
sampleSize <- xtabs(TrialID ~ Treatment + Instar, aggregate(TrialID~ Treatment + Instar, BoxTest, FUN = function(x) length(unique(x))))
addmargins(sampleSize)
print("")

##### MEANS ETC.###############
print("Condition  Means and StdDevs")
xtabs(residCond ~ Treatment + Instar + FeedIndPos, aggregate(residCond ~ Treatment + Instar + FeedIndPos, BoxTest, FUN = function(x) c(mean = mean(x), StdDev = sd(x))))


### Getting the difference in the mean condition between feeders and non-feeders and the standard error of that.

#### LARGE PREY
lenLFd <- length(BoxTest$residCond [BoxTest$FeedIndPos == "y" & BoxTest$Treatment == "large"])
lenLNonFd <- length(BoxTest$residCond [BoxTest$FeedIndPos == "n" & BoxTest$Treatment == "large"])

condLFdMn <-mean(BoxTest$residCond [BoxTest$FeedIndPos == "y" & BoxTest$Treatment == "large"], na.rm = TRUE)
condLNonFdMn <-mean(BoxTest$residCond [BoxTest$FeedIndPos == "n" & BoxTest$Treatment == "large"], na.rm = TRUE)

print("difference in mean condition for large prey")
condLNonFdMn - condLFdMn # difference in mean condition for large prey

condLFdSD <-(sd(BoxTest$residCond [BoxTest$FeedIndPos == "y" & BoxTest$Treatment == "large"], na.rm = TRUE) ) / 
		sqrt(lenLFd)
condLNonFdSD <-sd(BoxTest$residCond [BoxTest$FeedIndPos == "n" & BoxTest$Treatment == "large"], na.rm = TRUE) /
		sqrt(lenLNonFd)
print("Standard error of difference")
sqrt(condLFdSD^ 2 +  condLNonFdSD ^ 2)


#### Small PREY
lenSFd <- length(BoxTest$residCond [BoxTest$FeedIndPos == "y" & BoxTest$Treatment == "small"])
lenSNonFd <- length(BoxTest$residCond [BoxTest$FeedIndPos == "n" & BoxTest$Treatment == "small"])

condSFdMn <-mean(BoxTest$residCond [BoxTest$FeedIndPos == "y" & BoxTest$Treatment == "small"], na.rm = TRUE)
condSNonFdMn <-mean(BoxTest$residCond [BoxTest$FeedIndPos == "n" & BoxTest$Treatment == "small"], na.rm = TRUE)
print("difference in mean condition for small prey")
condSNonFdMn - condSFdMn # difference in mean condition for small prey

condSFdSD <-(sd(BoxTest$residCond [BoxTest$FeedIndPos == "y" & BoxTest$Treatment == "small"], na.rm = TRUE) ) / 
		sqrt(lenSFd)
condSNonFdSD <-sd(BoxTest$residCond [BoxTest$FeedIndPos == "n" & BoxTest$Treatment == "small"], na.rm = TRUE) /
		sqrt(lenSNonFd)
print("Standard error of difference")
sqrt(condSFdSD^ 2 +  condSNonFdSD ^ 2)
print("")


print("counts of numbers feeding")

counts<- xtabs(~ FeedIndPos + Instar, data = BoxTest)
counts

print("percentages")
colPerc(counts)
print("")

###################### STATISTICS #######################
# removing interactions as they either are insignificant or don't make sense

print("Statistics")
print("Full Model")
EatBinModFull <- glmer(FeedIndPos ~ residCond + Treatment + Instar + Treatment:residCond +  (1|IndBoxID)+
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))

EatBinModFull@call

summary(EatBinModFull)$coefficients


print("")


## Testing the interaction
EatBinRedModInt <- glmer(FeedIndPos ~ residCond + Treatment + Instar + (1|IndBoxID) +
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))


RedVsFull_fun("Testing Interaction", EatBinRedModInt, EatBinModFull)

## Testing Treatment
EatBinRedModTreatment <- glmer(FeedIndPos ~ residCond  + Instar + (1|IndBoxID)+
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))

RedVsFull_fun("Testing the treatment", EatBinRedModTreatment, EatBinModFull)


### Testing Condition
EatBinRedModCond <- glmer(FeedIndPos ~ Treatment  + Instar + (1|IndBoxID) +
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))


RedVsFull_fun("Testing condition", EatBinRedModCond, EatBinModFull)


## Testing Instar 
EatBinRedModInstar <- glmer(FeedIndPos ~ Treatment  + residCond + Treatment:residCond +  (1|IndBoxID) +
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))

RedVsFull_fun("Testing Instar", EatBinRedModInstar, EatBinModFull)

print("")
print("model AIC's")
model_list <- c(EatBinModFull, EatBinRedModCond, EatBinRedModInstar, EatBinRedModInt, EatBinRedModTreatment)
modelAIC(model_list)


## Ad hoc testing 


GLMERS_fun <- function(myData) {
	glmerFull <- glmer(FeedIndPos ~ residCond + Instar + (1|IndBoxID)+ (1|IndBoxID:SpiderID), 
			data = myData, family = binomial(logit))
	
	glmerRed <- glmer(FeedIndPos ~ Instar + (1|IndBoxID)+ (1|IndBoxID:SpiderID), 
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

