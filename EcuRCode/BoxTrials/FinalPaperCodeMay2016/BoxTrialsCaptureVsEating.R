## Box Trials


###########################################
# Capture vs feeding

sink('RuthEcuador2013/BoxFeedingTrials/StatsOutput/CaptureVsFeeding_noInstarFullModel.txt')
date()
print("Capture Vs Feeding")
BoxTest <- subset(BoxComboMorn, !is.na(residCond) & !is.na(FeedIndPos) & !is.na(CaptureIndPos))
print("")
print("SampleSize - num trials")
sampleSize <- xtabs(TrialID ~ Treatment + Instar, aggregate(TrialID ~ Treatment + Instar, BoxTest, FUN = function(x) length(unique(x))))
addmargins(sampleSize)
print("")


##numbers

counts <- xtabs(~ FeedIndPos + CaptureIndPos, data = BoxTest)


print("percentages")
colPerc(counts)
print("")

print("percentages the other way around")
rowPerc(counts)
print("")





### Stats Tests
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
print("")

########### Testing Individual feed:treatment interaction ##########
RedVsFull_fun("Testing Individual feed:treatment interaction", CapFdGlmerInt, CapFdGlmer)

########### Testing Instar ##########
CapFdGlmerInstar <- glmer(IndCapture ~ Treatment + IndFeed +  (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))

RedVsFull_fun("Testing instar", CapFdGlmerInstar, CapFdGlmerInt )


########### Testing Individual feed ##########
CapFdGlmerFd <- glmer(IndCapture ~ Treatment  +  (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))

RedVsFull_fun("Testing individual Feed", CapFdGlmerFd, CapFdGlmerInstar )

########### Testing Treatment ##########
CapFdGlmerTreat <- glmer(IndCapture ~ IndFeed +  (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))

RedVsFull_fun("Testing  treatment", CapFdGlmerTreat, CapFdGlmerInstar)



########### Testing Instar Interaction ##########
CapFdGlmerInstarInteraction <- glmer(IndCapture ~ Treatment + IndFeed + Instar:IndFeed +  (1|IndBoxID) + 
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))

RedVsFull_fun("Testing instar interaction", CapFdGlmerInstar, CapFdGlmerInstarInteraction)


print("")
print("model AIC's")
model_list <- c(CapFdGlmer, CapFdGlmerFd, CapFdGlmerInstar, CapFdGlmerInstarInteraction, CapFdGlmerInt, CapFdGlmerTreat)
modelAIC(model_list)

sink()



