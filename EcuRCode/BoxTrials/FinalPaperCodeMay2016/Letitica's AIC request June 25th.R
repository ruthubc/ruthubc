# TODO: Add comment
# 
# Author: user
###############################################################################


sink('RuthEcuador2013/BoxFeedingTrials/StatsOutput/LeticiasAIC.txt')
date()
print("AIC values 30 June")

BoxTest <- subset(BoxComboMorn, !is.na(residCond) & !is.na(FeedIndPos))

print("Feed vs condition, treatment and interaction")


EatBinModFull <- glmer(FeedIndPos ~ residCond + Treatment + Instar + Treatment:residCond +  (1|IndBoxID)+
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))

EatBinModFull@call

print("AIC Value")
print("")
print("Feed vs cond with no interaction")
EatBinModNoInt<- glmer(FeedIndPos ~ residCond + Treatment + Instar  +  (1|IndBoxID)+
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))

EatBinModNoInt@call

print("AIC Value")

AIC(EatBinModNoInt)
print("")

print("Capture vs condition, treatment and interaction")

BoxTest <- subset(BoxComboMorn, !is.na(residCond) & !is.na(CaptureIndPos))

CapBinModFull <- glmer(CaptureIndPos ~ residCond + Treatment + Instar + Treatment:residCond +  (1|IndBoxID)+
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))

CapBinModFull@call

print("AIC Value")

AIC(CapBinModFull)
print("")

print("Capture vs condition, treatment with no interaction")


CapBinModNoInt <- glmer(CaptureIndPos ~ residCond + Treatment + Instar + Treatment:residCond +  (1|IndBoxID)+
				(1|IndBoxID:SpiderID), BoxTest, family = binomial(logit))

CapBinModNoInt@call

print("AIC Value")

AIC(CapBinModNoInt)



sink()

