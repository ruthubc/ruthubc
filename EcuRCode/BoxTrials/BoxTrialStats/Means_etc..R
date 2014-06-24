# TODO: Add comment
# 
# Author: Ruth
###############################################################################


by(AveByTrial$noFeed, AveByTrial$Treatment mean)
by(AveByTrial$noCap, AveByTrial$Treatment, mean)

BoxRatioStats = function(x) c(mean = mean(x), se = (sd(x)/sqrt(length(x))), n = length(x), max = max(x))
tapply(AveByTrial$noFeed, paste(AveByTrial$Treatment, AveByTrial$Instar),  BoxRatioStats)
tapply(AveByTrial$noCap, paste(AveByTrial$Treatment, AveByTrial$Instar), BoxRatioStats)

#testing diff mean eat between large and small prey

EatNoFullMod <- lmer(noFeed ~ Treatment + Instar+  (1|IndBoxID), AveByTrial, REML = FALSE )

EatNoRedTreat <- lmer(noFeed ~Instar+  (1|IndBoxID), AveByTrial, REML = FALSE )

anova(EatNoFullMod, EatNoRedTreat)

### testing diff between the numbers that captured prey and those that didn't
CapNoFullMod <- lmer(noCap ~ Treatment + Instar+  (1|IndBoxID), AveByTrial, REML = FALSE )

CapNoRedTreat <- lmer(noCap ~Instar+  (1|IndBoxID), AveByTrial, REML = FALSE )

anova(CapNoFullMod, CapNoRedTreat)