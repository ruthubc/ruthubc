### Links can use for glmer http://www.heuschele.com/?page_id=6


#######################################################################################
# Evenness vs prey size

sink('RuthEcuador2013/BoxFeedingTrials/StatsOutput/EvennessVsPreySize.txt')
date()
print("Box Evenness")
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
print("Treatment:Instar not significant so removed from full model")
print("")

print("Model without Interaction")

PJMod <-  lmer(AsinPJEven ~ Treatment +Instar+ (1|IndBoxID) + (1|OrgNest), AveByTrial, REML = FALSE)
formula(PJMod)
anova(PJMod)
print("")

RedVsFull_fun("Testing Interaction", PJMod, PJModInteraction)

PJModTreat <-  lmer(AsinPJEven ~ Instar+ (1|IndBoxID), AveByTrial, REML = FALSE)
RedVsFull_fun("Testing Treatment", PJMod, PJModTreat)

##### testing instar
PJModInstar <-  lmer(AsinPJEven ~ Treatment + (1|IndBoxID), AveByTrial, REML = FALSE)
RedVsFull_fun("Testing Instar", PJMod, PJModInstar)


print("")
print("model AIC's")
model_list <- c(PJMod, PJModInstar, PJModInteraction, PJModTreat)
modelAIC(model_list)

sink()


