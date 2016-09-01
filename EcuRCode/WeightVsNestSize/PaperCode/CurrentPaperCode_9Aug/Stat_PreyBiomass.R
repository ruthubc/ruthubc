# TODO: Add comment
# 
# Author: user
###############################################################################


Biomass <- read.csv("RuthSync/EggManipulation/FranciscoData_insectBiomass.csv")

Biomass <- subset(Biomass, AdFemales > 0)

Biomass <- subset(Biomass, Treatment == "Control")

Biomass <- subset(Biomass, OverallID != 5)

Biomass$log10AdFm <- log10(Biomass$AdFemales)

Biomass$BiomsPerAdFm <- Biomass$Tl_insect_biomass/Biomass$AdFemales



ggplot(data = Biomass, aes(x = log10AdFm)) + geom_histogram()

Biomass$log10_BiomsPerAdFm <- log10((Biomass$BiomsPerAdFm*100000)+1)

ggplot(data = Biomass, aes(x = log10_BiomsPerAdFm)) + geom_histogram()


model <- lmer(log10_BiomsPerAdFm ~ log10AdFm + (1|ColonyID) + (1|Date), data = Biomass, REML = FALSE)
AIC(model)
anova(model)

p <- ggplot(data = Biomass, aes(x = log10AdFm, y = log10_BiomsPerAdFm)) + geom_point() + geom_smooth(method = "lm")
p


Vis_fit <- (visreg(model, "log10AdFm", plot = FALSE))$fit						
p <- p +  geom_line(data = Vis_fit, aes(x = log10AdFm, y= visregFit))

p

RedModel <- lmer(log10_BiomsPerAdFm ~ (1|ColonyID) + (1|Date), data = Biomass, REML = FALSE)

anova(RedModel, model)

levels(Biomass$Date)

plot(RedModel)

model <- lmer(log10_BiomsPerAdFm ~ I(log10AdFm^2) + log10AdFm + (1|ColonyID) + (1|Date), data = Biomass, REML = FALSE)
AIC(model)
anova(model)