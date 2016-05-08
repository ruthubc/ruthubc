### Links can use for glmer http://www.heuschele.com/?page_id=6


#######################################################################################
# Evenness vs prey size

sink('RuthEcuador2013/BoxFeedingTrials/StatsOutput/EvennessVsPreySize.txt')
print("AsinPJ Means and StdDevs")
xtabs(AsinPJEven ~ Treatment + Instar, aggregate(AsinPJEven ~ Treatment + Instar, AveByTrial, FUN = function(x) c(mean = mean(x), StdDev = sd(x))))
print("SampleSize")
sampleSize <- xtabs(IndBoxID ~ Treatment + Instar, aggregate(IndBoxID~ Treatment + Instar, AveByTrial, FUN = function(x) length(unique(x))))
addmargins(sampleSize)
print("")


################ Statistics #############

print("Model with interaction")

PJModInteraction <-  lmer(AsinPJEven ~ Treatment +Instar + Treatment:Instar + (1|IndBoxID), AveByTrial, REML = FALSE)
formula(PJModInteraction)
print("")
anova(PJModInteraction)
print("")

print("Model without Interaction")

PJMod <-  lmer(AsinPJEven ~ Treatment +Instar+ (1|IndBoxID), AveByTrial, REML = FALSE)
formula(PJMod)

print("")
anova(PJMod)
print("")

print("Anova model comparison Treatment")
print("")

PJRedModTreat <-  lmer(AsinPJEven ~ Instar+ (1|IndBoxID), AveByTrial, REML = FALSE)
anova(PJMod, PJRedModTreat)
print("")

print("AIC difference")
AIC(PJRedModTreat) - AIC(PJMod) 

print("")
print("Anova model comparison Instar")
print("")

##### testing instar
PJRedModInstar <-  lmer(AsinPJEven ~ Treatment + (1|IndBoxID), AveByTrial, REML = FALSE)
anova(PJMod, PJRedModInstar)

print("")
print("AIC difference")
AIC(PJRedModInstar) - AIC(PJMod) 

sink()


