# Testing whether lag is correlation with max correlation
# 
# Author: Ruth
###############################################################################


### Kin Preference vs. Cooperation

lm1 <- lmer(KPvsCoopLag ~ KPvsCoopMax+  (1|Run),  Corrs, REML = FALSE)
lmRed <-lmer(KPvsCoopLag ~ (1|Run),  Corrs, REML = FALSE)

anova(lm1, lmRed)

## KP vs Group Size

lm1 <- lmer(KPvsGSLag ~ KPvsGSMax+  (1|Run),  Corrs, REML = FALSE)
lmRed <-lmer(KPvsGSLag ~ (1|Run),  Corrs, REML = FALSE)

anova(lm1, lmRed)

## KP vs relat

lm1 <- lmer(KPvsRelLag ~ KPvsRelMax+  (1|Run),  Corrs, REML = FALSE)
lmRed <-lmer(KPvsRelLag ~ (1|Run),  Corrs, REML = FALSE)

anova(lm1, lmRed)

##  Group size vs relatedness

lm1 <- lmer(GSvsRelLag ~ GSvsRelMax+  (1|Run),  Corrs, REML = FALSE)
lmRed <-lmer(GSvsRelLag ~ (1|Run),  Corrs, REML = FALSE)

anova(lm1, lmRed)

##  Group size vs Cooperation

lm1 <- lmer(GSvsCoopLag ~ GSvsCoopMax+  (1|Run),  Corrs, REML = FALSE)
lmRed <-lmer(GSvsCoopLag ~ (1|Run),  Corrs, REML = FALSE)

anova(lm1, lmRed)

##  Cooperation vs Rel

lm1 <- lmer(CoopvsResLag ~ CoopvsResMax+  (1|Run),  Corrs, REML = FALSE)
lmRed <-lmer(CoopvsResLag ~ (1|Run),  Corrs, REML = FALSE)

anova(lm1, lmRed)