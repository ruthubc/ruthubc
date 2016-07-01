## Box Trials


###########################################
# Capture vs feeding

sink('RuthEcuador2013/BoxFeedingTrials/StatsOutput/CaptureVsFeeding_FeedingDependentVariable.txt')
date()
print("Capture Vs Feeding_ Feeding Dependent Variable")
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

CapFdGlmer <- glmer(IndFeed ~ IndCapture + Treatment + Instar + IndCapture:Treatment+  
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))


CapFdGlmer@call
summary(CapFdGlmer)$coefficients
print("interaction is not significant so removing it")
print("")

print("Model Without Interaction")
CapFdGlmerInt <- glmer(IndFeed ~ IndCapture + Treatment + Instar +  
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))

CapFdGlmerInt@call
summary(CapFdGlmerInt)$coefficients
print("")

########### Testing Individual feed:treatment interaction ##########
RedVsFull_fun("Testing Individual feed:treatment interaction", CapFdGlmerInt, CapFdGlmer)

########### Testing Individual capture ##########
CapFdGlmerFd <- glmer(IndFeed ~ Treatment + Instar +  
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))

RedVsFull_fun("Testing individual Feed", CapFdGlmerFd, CapFdGlmerInt )

########### Testing Treatment ##########
CapFdGlmerTreat <- glmer(IndFeed ~ Instar + IndCapture +   
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))

RedVsFull_fun("Testing  treatment", CapFdGlmerTreat, CapFdGlmerInt )

########### Testing Instar ##########
CapFdGlmerInstar <- glmer(IndFeed ~ Treatment + IndCapture  + 
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))

RedVsFull_fun("Testing instar", CapFdGlmerInstar, CapFdGlmerInt )


########### Testing Instar Interaction ##########
CapFdGlmerInstarNoInt <- glmer(IndFeed ~ Treatment + IndCapture + Instar + 
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))

CapFdGlmerInstarInteraction <- glmer(IndFeed ~ Treatment + IndCapture + Instar:IndCapture +  
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))

RedVsFull_fun("Testing instar interaction", CapFdGlmerInstarNoInt, CapFdGlmerInstarInteraction)


print("")
print("model AIC's")
model_list <- c(CapFdGlmer, CapFdGlmerFd, CapFdGlmerInstar, CapFdGlmerInstarInteraction, CapFdGlmerInt, CapFdGlmerTreat)
modelAIC(model_list)

sink()



