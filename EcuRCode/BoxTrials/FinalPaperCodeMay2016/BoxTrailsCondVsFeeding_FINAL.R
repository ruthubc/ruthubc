

#####################################################################################
## Condition vs feeding

sink('RuthEcuador2013/BoxFeedingTrials/StatsOutput/CondVsFeeding.txt')
##### MEANS ETC.###############
print("Cond  Means and StdDevs")
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



###################### STATISTICS #######################
# removing interactions as they either are insignificant or don't make sense

BoxTest <- subset(BoxTest, !is.na(residCond))



print("Statistics")
print("Full Model")
EatBinModFull <- glmer(IndFeed ~ residCond + Treatment + Instar + Treatment:residCond +  (1|IndBoxID)+
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))

EatBinModFull@call

summary(EatBinModFull)$coefficients


print("")
print("testingTreatment:Hunger interaction")

EatBinRedModInt <- glmer(IndFeed ~ residCond + Treatment + Instar + (1|IndBoxID) +
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))



anova(EatBinRedModInt, EatBinModFull) #very significant interaction effect

print("AIC difference")
AIC(EatBinRedModInt) - AIC(EatBinModFull)

print("")
print("testing treatment")

EatBinRedModTreatment <- glmer(IndFeed ~ residCond  + Instar + (1|IndBoxID)+
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))


anova(EatBinRedModTreatment, EatBinModFull)

print("AIC difference")
AIC(EatBinRedModTreatment) - AIC(EatBinModFull)


print("")
print("testing Condition")
EatBinRedModCond <- glmer(IndFeed ~ Treatment  + Instar + (1|IndBoxID) +
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))

anova(EatBinRedModCond, EatBinModFull)

print("AIC difference")
AIC(EatBinRedModCond) - AIC(EatBinModFull)



print("")
print("testing instar")
EatBinRedModInstar <- glmer(IndFeed ~ Treatment  + residCond + (1|IndBoxID) +
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))

anova(EatBinRedModInstar, EatBinModFull)

print("AIC difference")
AIC(EatBinRedModInstar) - AIC(EatBinModFull)

## Ad hoc testing 


GLMERS_fun <- function(myData) {
	glmerFull <- glmer(IndFeed ~ residCond + Instar + (1|IndBoxID)+ (1|IndBoxID:SpiderID), 
			data = myData, family = binomial(logit))
	
	glmerRed <- glmer(IndFeed ~ Instar + (1|IndBoxID)+ (1|IndBoxID:SpiderID), 
			data = myData, family = binomial(logit))
	
	lstToReturn <-  list(glmerFull, glmerRed)
	
	names(lstToReturn)[1] <- "Full"
	names(lstToReturn)[2] <- "Reduced"
	
	return(lstToReturn)
	
}


treatGLMER <- dlply(BoxTest, .(Treatment),        
		function(x) GLMERS_fun(x))

printGLMER_fun <- function(glmLst){
	anv <- anova(glmLst[[1]], glmLst[[2]])
	AICs <- AIC(glmLst[[2]]) - AIC(glmLst[[1]])
	lstToRet <- list(anv, AICs)
	names(lstToRet)[[1]] <- "Anova_RedVsFull"
	names(lstToRet)[[2]] <- "AIC_Diff"
	return(lstToRet)
	
	
}


print("AdHoc Testing split up by treatment")
test <- lapply(treatGLMER, FUN = function(x) printGLMER_fun(x))# anova(x[[1]], x[[2]]))
print(test)

sink()

###########################################
# Capture vs feeding

CapFeedMorn <- subset(BoxTest, BoxFeedObs == "y")

##numbers

BoxComboCap$FedWords<-ifelse(BoxComboCap$FeedIndPos == "y", "Fed", "Did Not Feed")
BoxComboCap$CapWords<-ifelse(BoxComboCap$CaptureIndPos == "y", "Cap", "Did Not Cap")

table(BoxTest$IndCapture, BoxTest$IndFeed)


### Stats Tests
CapFdGlmer <- glmer(IndCapture ~ IndFeed + Treatment + Instar + IndFeed:Treatment+  (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), CapFeedMorn, family = binomial(logit))

summary(CapFdGlmer)

CapFdGlmerInt <- glmer(IndCapture ~ IndFeed + Treatment + Instar +  (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), CapFeedMorn, family = binomial(logit))

anova(CapFdGlmer, CapFdGlmerInt)


CapFdGlmerFd <- glmer(IndCapture ~ Treatment + Instar +  (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), CapFeedMorn, family = binomial(logit))

anova(CapFdGlmer, CapFdGlmerFd)

anova(CapFdGlmerFd, CapFdGlmerInt)

################ Capture vs condition



############## Difference in mean scaled condition between capturers and non-capturers

#### LARGE PREY
lenLCp <- length(BoxTest$residCond [BoxTest$CaptureIndPos == "y" & BoxTest$Treatment == "large"])
lenLNonCp <- length(BoxTest$residCond [BoxTest$CaptureIndPos == "n" & BoxTest$Treatment == "large"])

condLCpMn <-mean(BoxTest$residCond [BoxTest$CaptureIndPos == "y" & BoxTest$Treatment == "large"], na.rm = TRUE)
condLNonCpMn <-mean(BoxTest$residCond [BoxTest$CaptureIndPos == "n" & BoxTest$Treatment == "large"], na.rm = TRUE)

condLNonCpMn - condLCpMn # difference in mean condition for large prey

condLCpSD <-(sd(BoxTest$residCond [BoxTest$CaptureIndPos == "y" & BoxTest$Treatment == "large"], na.rm = TRUE) ) / 
		sqrt(lenLCp)
condLNonCpSD <-sd(BoxTest$residCond [BoxTest$CaptureIndPos == "n" & BoxTest$Treatment == "large"], na.rm = TRUE) /
		sqrt(lenLNonCp)

sqrt(condLCpSD^ 2 +  condLNonCpSD ^ 2)


#### Small PREY
lenSCp <- length(BoxTest$residCond [BoxTest$CaptureIndPos == "y" & BoxTest$Treatment == "small"])
lenSNonCp <- length(BoxTest$residCond [BoxTest$CaptureIndPos == "n" & BoxTest$Treatment == "small"])

condSCpMn <-mean(BoxTest$residCond [BoxTest$CaptureIndPos == "y" & BoxTest$Treatment == "small"], na.rm = TRUE)
condSNonCpMn <-mean(BoxTest$residCond [BoxTest$CaptureIndPos == "n" & BoxTest$Treatment == "small"], na.rm = TRUE)

condSNonCpMn - condSCpMn # difference in mean condition for small prey

condSCpSD <-(sd(BoxTest$residCond [BoxTest$CaptureIndPos == "y" & BoxTest$Treatment == "small"], na.rm = TRUE) ) / 
		sqrt(lenSCp)
condSNonCpSD <-sd(BoxTest$residCond [BoxTest$CaptureIndPos == "n" & BoxTest$Treatment == "small"], na.rm = TRUE) /
		sqrt(lenSNonCp)

sqrt(condSCpSD^ 2 +  condSNonCpSD ^ 2)

############### Stats tests GLMER

CapConFull.glmer <- glmer(IndCapture ~ LogCond+Instar+Treatment + LogCond*Treatment + (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))
summary(CapConFull.glmer)

##Checking LogCond:Treatment

CapConInter.glmer <- glmer(IndCapture ~ LogCond+Instar+Treatment + (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))
anova(CapConFull.glmer, CapConInter.glmer)

###Checking LogCond
CapConCond.glmer <- glmer(IndCapture ~ Instar+Treatment + (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))
anova(CapConFull.glmer, CapConCond.glmer)

###Checking LogCond + Treatment
CapConTreat.glmer <- glmer(IndCapture ~ LogCond + Instar + (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))
anova(CapConFull.glmer, CapConTreat.glmer)

### Percentage difference

EatConDiff<- aggregate(BoxTest$Cond, by = list(BoxTest$CaptureIndPos, BoxTest$Treatment), 
		FUN = mean, na.rm=TRUE)

PerDiff(EatConDiff)

### Just small prey 

CapConCond.small <- glmer(IndCapture ~ Instar+LogCond + (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), subset(BoxTest, Treatment == 'small'), family = binomial(logit))

CapConCond.smallRed <- glmer(IndCapture ~ Instar + (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), subset(BoxTest, Treatment == 'small'), family = binomial(logit))

anova(CapConCond.small, CapConCond.smallRed)


### Just large prey 

CapConCond.large <- glmer(IndCapture ~ Instar+LogCond + (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), subset(BoxTest, Treatment == 'large'), family = binomial(logit))

CapConCond.largeRed <- glmer(IndCapture ~ Instar + (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), subset(BoxTest, Treatment == 'large'), family = binomial(logit))

anova(CapConCond.large, CapConCond.largeRed)


##### The removing non- participants , cheaters vs altrusits vs others.

table(BoxMornFeedOrCap$CapAndFeed, BoxMornFeedOrCap$Treatment)



## Small prey



GlmFeedAndCapSm<- glmer(CapAndFeed ~  LogCond + Instar + 
				(1|IndBoxID) + (1|IndBoxID:SpiderID), subset(BoxMornFeedOrCap, Treatment == "small"), family = binomial(logit))

summary(GlmFeedAndCapSm)

GlmFeedAndCapRedSm<- glmer(CapAndFeed ~  Instar + 
				(1|IndBoxID) + (1|IndBoxID:SpiderID), subset(BoxMornFeedOrCap, Treatment == "small" & IndFeedNum == 1), family = binomial(logit))

anova(GlmFeedAndCapSm, GlmFeedAndCapRedSm )


## large prey


GlmFeedAndCapLg<- glmer(CapAndFeed ~  LogCond + Instar + 
				(1|IndBoxID) + (1|IndBoxID:SpiderID), subset(BoxMornFeedOrCap, Treatment == "large"& IndFeedNum == 1), family = binomial(logit))

summary(GlmFeedAndCapLg)
visreg(GlmFeedAndCapLg)

GlmFeedAndCapRedLg<- glmer(CapAndFeed ~  Instar + 
				(1|IndBoxID) + (1|IndBoxID:SpiderID), subset(BoxMornFeedOrCap, Treatment == "large" && IndFeedNum == 1), family = binomial(logit))

anova(GlmFeedAndCapLg, GlmFeedAndCapRedLg )

########################################################################
### Testing if there is a difference in the average number in each catorary "CapNoEat"   "NoCapEat"   "CapEat"     "NoCapNoEat"

FrCtsGlm <- glmer(value ~ variable + Instar + Treatment + variable:Treatment + (1|IndBoxID), FdCapByTrial, family = poisson )

summary(FrCtsGlm)
overdisp_fun(FrCtsGlm) # fine. Not overdispersed

FrCtsGlmInt <- glmer(value ~ variable + Instar + Treatment  + (1|IndBoxID), FdCapByTrial, family = poisson )

anova(FrCtsGlmInt, FrCtsGlm)

FrCtsGlmTreat <- glmer(value ~ variable + Instar  + (1|IndBoxID), FdCapByTrial, family = poisson )

anova(FrCtsGlmTreat, FrCtsGlm)
#Levels: "CapNoEat"   "NoCapEat"   "CapEat"     "NoCapNoEat"
### PostHoc- testing Cap And Eat 

FrCtsGlmCE <- glmer(value ~  Instar + Treatment + (1|IndBoxID), 
		subset(FdCapByTrial, variable == "CapEat"), family = poisson )

summary(FrCtsGlmCE)
overdisp_fun(FrCtsGlmCE) # not over dispersed

FrCtsGlmCERed <- glmer(value ~  Instar  + (1|IndBoxID), 
		subset(FdCapByTrial, variable == "CapEat"), family = poisson )

anova(FrCtsGlmCE,FrCtsGlmCERed) # not significant

# Testing NoCapEat
FrCtsGlmNCE <- glmer(value ~  Instar + Treatment + (1|IndBoxID), 
		subset(FdCapByTrial, variable == "NoCapEat"), family = poisson )

summary(FrCtsGlmNCE)
overdisp_fun(FrCtsGlmNCE) # not overdispersed

FrCtsGlmNCERed <- glmer(value ~  Instar  + (1|IndBoxID), 
		subset(FdCapByTrial, variable == "NoCapEat"), family = poisson )

anova(FrCtsGlmNCE,FrCtsGlmNCERed) 


# Testing NoCapNoEat
FrCtsGlmNCNE <- glmer(value ~  Instar + Treatment + (1|IndBoxID), 
		subset(FdCapByTrial, variable == "NoCapNoEat"), family = poisson )

summary(FrCtsGlmNCNE)
overdisp_fun(FrCtsGlmNCNE) # not overdispersed

FrCtsGlmNCNERed <- glmer(value ~  Instar  + (1|IndBoxID), 
		subset(FdCapByTrial, variable == "NoCapNoEat"), family = poisson )

anova(FrCtsGlmNCNE,FrCtsGlmNCNERed) # not significant


# Testing CapNoEat
FrCtsGlmCNE <- glmer(value ~  Instar + Treatment + (1|IndBoxID), 
		subset(FdCapByTrial, variable == "CapNoEat"), family = poisson )

summary(FrCtsGlmCNE)
overdisp_fun(FrCtsGlmCNE) # not overdispersed

FrCtsGlmCNERed <- glmer(value ~  Instar  + (1|IndBoxID), 
		subset(FdCapByTrial, variable == "CapNoEat"), family = poisson )

anova(FrCtsGlmCNE,FrCtsGlmCNERed) 


table(AveFdOrCap$NoCapNoEat, AveFdOrCap$Treatment)

#the number of spiders
table(BoxTest$CapAndFeed)
# the number of boxes that had each of the four catorgies. 
tapply(BoxTest$SpiderID,BoxTest$CapAndFeed, function(x) length(unique(x)))
