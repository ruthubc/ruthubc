## Box Trials


###########################################
# Capture vs feeding

sink('RuthEcuador2013/BoxFeedingTrials/StatsOutput/CaptureVsFeeding.txt')
print("Capture Vs Feeding")
BoxTest <- subset(BoxComboMorn, !is.na(residCond) & !is.na(FeedIndPos) & !is.na(CaptureIndPos))

##numbers

xtabs(~ FeedIndPos + CaptureIndPos, data = BoxTest)



### Stats Tests
print("")
print("Testing Full Model")

CapFdGlmer <- glmer(IndCapture ~ IndFeed + Treatment + Instar + IndFeed:Treatment+  (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))


CapFdGlmer@call
summary(CapFdGlmer)$coefficients
print("interaction is not significant so removing it")
print("")

print("Model Without Interaction")
CapFdGlmerInt <- glmer(IndCapture ~ IndFeed + Treatment + Instar +  (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))

CapFdGlmerInt@call
summary(CapFdGlmerInt)$coefficients

########### Testing Individual feed ##########
CapFdGlmerFd <- glmer(IndCapture ~ Treatment + Instar +  (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))

RedVsFull_fun("Testing individual Feed", CapFdGlmerFd, CapFdGlmerInt )

########### Testing Treatment ##########
CapFdGlmerTreat <- glmer(IndCapture ~ Instar + IndFeed +  (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))

RedVsFull_fun("Testing  treatment", CapFdGlmerTreat, CapFdGlmerInt )

########### Testing Instar ##########
CapFdGlmerInstar <- glmer(IndCapture ~ Treatment + IndFeed +  (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))

RedVsFull_fun("Testing instar", CapFdGlmerInstar, CapFdGlmerInt )

sink()



