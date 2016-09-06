# TODO: Add comment
# 
# Author: user
###############################################################################

# Note biomass per capita normal if log transform insect biomass then sqrt biomass/adNum BUT this results in the opposite distribution with food decreasing with colony size

# From http://seismo.berkeley.edu/~kirchner/eps_120/Toolkits/Toolkit_03.pdf about log (x+1)
# One way to deal with this problem is to use x'=log(x/mean(x)+k), where k is a small constant (k<<1). In this
#transformation, the mean x will be transformed to near x'=0 and k will function as
#a shape factor (small k will make x' more left-skewed, larger k will make it less so). 

Biomass <- read.csv("RuthSync/EggManipulation/FranciscoData_insectBiomass.csv")

Biomass <- subset(Biomass, AdFemales > 0)

Biomass <- subset(Biomass, Treatment == "Control")

Biomass <- subset(Biomass, OverallID != 5)  # this appears to be a massive outlier

Biomass$log10AdFm <- log10(Biomass$AdFemales)

ggplot(data = Biomass, aes(x = log10AdFm)) + geom_histogram()

Biomass$biomass_mg <- Biomass$Tl_insect_biomass *1000

Biomass$Lg10_biomass <- log10(Biomass$biomass_mg+1)

ggplot(Biomass, aes(Lg10_biomass)) + geom_histogram()


ggplot(data = Biomass, aes(x = log10AdFm, y = biomass_mg)) + geom_point() + geom_smooth()
ggplot(data = Biomass, aes(x = log10AdFm, y = Lg10_biomass)) + geom_point() + geom_smooth()
ggplot(data = Biomass, aes(x = Tl_adults, y = biomass_mg)) + geom_point() + geom_smooth()
ggplot(data = Biomass, aes(x = Tl_adults, y = Lg10_biomass)) + geom_point() + geom_smooth()

Biomass$BiomsPerAdFm <- Biomass$Tl_insect_biomass/Biomass$AdFemales
Biomass$trans_BiomsPerAdFm <- sqrt(Biomass$BiomsPerAdFm) # this makes it normal
# OR
Biomass$BiomsPerAdFm <- Biomass$Lg10_biomass/Biomass$log10AdFm


ggplot(data = Biomass, aes(x = BiomsPerAdFm)) + geom_histogram()



p <- ggplot(data = Biomass, aes(x = log10AdFm, y = BiomsPerAdFm)) + geom_point() + geom_smooth(method = "lm")
p




model <- lmer(BiomsPerAdFm ~ log10AdFm + (1|ColonyID) + (1|Date), data = Biomass, REML = FALSE)
AIC(model)
anova(model)



Vis_fit <- (visreg(model, "log10AdFm", plot = FALSE))$fit						
p <- p +  geom_line(data = Vis_fit, aes(x = log10AdFm, y= visregFit))

p

RedModel <- lmer(BiomsPerAdFm ~ (1|ColonyID) + (1|Date), data = Biomass, REML = FALSE)

anova(RedModel, model)

levels(Biomass$Date)

plot(RedModel)

model <- lmer(log10_BiomsPerAdFm ~ I(log10AdFm^2) + log10AdFm + (1|ColonyID) + (1|Date), data = Biomass, REML = FALSE)
AIC(model)
anova(model)