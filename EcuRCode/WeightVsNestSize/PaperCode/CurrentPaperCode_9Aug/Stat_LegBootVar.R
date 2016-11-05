# TODO: Add comment
# 
# Author: Ruth
###############################################################################

require(car)
require(MASS)


legBootVar <- read.csv("RuthEcuador2013/NestSize/bootSampCondPython_leg_combined.csv")

totSpis<- sum(legBootVar$N)
legBootVar$lmrWgts <- legBootVar$N/totSpis


InstarGridGraph(legBootVar, "bootSD_var", "Leg Boot Variance" )


ggplot(aes(bootSD_var), data = legBootVar) + geom_density()

legBootVar$SD_trans <- asin(sqrt(legBootVar$bootSD_var))

nrow(legBootVar) - sum(is.finite(legBootVar$SD_trans)) # checking the transformation worked for all numbers


ggplot(aes(SD_trans), data = legBootVar) + geom_density()

InstarGridGraph(legBootVar, "SD_trans", "Leg Boot Variance" )



qqp(legBootVar$SD_trans, "norm")
# lnorm means lognormal
qqp(legBootVar$SD_trans, "lnorm")
qqp(legBootVar$bootSD_var, "lnorm")

legBootVar$SD_trans2 <- (legBootVar$SD_trans /2) + 0.001

qqp(legBootVar$SD_trans, "lnorm")

mean(legBootVar$SD_trans)

ozone = ozone[!is.na(ozone)]

gamma.quantiles = qgamma(legBootVar$SD_trans2, shape =.86)


plot(sort(gamma.quantiles), sort(legBootVar$bootSD_var), xlab = 'Theoretical Quantiles from Gamma Distribution', ylab = 'Sample Quantiles of Ozone', main = 'Gamma Quantile-Quantile Plot of Ozone')
abline(0,1)


library(fitdistrplus)

descdist(legBootVar$bootSD_var, discrete = FALSE)

n_samp <- nrow(legBootVar)


legBootVar$SD_trans_beta <- (legBootVar$bootSD_var * (n_samp -1) + 0.5)/n_samp # removes zeros

legBootVar$SD_trans_beta_asin <- asin(sqrt(legBootVar$SD_trans_beta))
nrow(legBootVar) - sum(is.finite(legBootVar$SD_trans_beta_asin)) # checking the transformation worked for all numbers

ggplot(aes(SD_trans_beta_asin), data = legBootVar) + geom_density()

descdist(legBootVar$SD_trans_beta_asin, discrete = FALSE)

descdist(sqrt(legBootVar$bootSD_var+0.001), discrete = FALSE)

summary(fitdist(legBootVar$bootSD_var + 0.001, "beta"))

plot(fitdist(legBootVar$SD_trans + 0.001, "lnorm"))
 summary(fitdist(legBootVar$SD_trans + 0.001, "lnorm"))

plot(fitdist(legBootVar$SD_trans_beta_asin, "lnorm"))

symbox(legBootVar$bootSD_var, powers=c(3,2,1,0.5,0,-0.5,-1,-2), start = 0.0001) # ladder of powers transformation
symbox(legBootVar$SD_trans_beta, powers=c(3,2,1,0.5,0,-0.5,-1,-2))

require(HH)
ladder(logCtFm~SD_trans_beta, data=legBootVar)

legBootVar$SD_trans_beta_ladder <- legBootVar$SD_trans_beta^(-1)

nrow(legBootVar) - sum(is.finite(legBootVar$SD_trans_beta_ladder)) # checking the transformation worked for all numbers

ggplot(aes(SD_trans_beta_ladder), data = legBootVar) + geom_density()

descdist(legBootVar$SD_trans_beta_ladder, discrete = FALSE)

################## Final stat version as of 28th Friday Oct 2016 #########
#although maybe use beta transformation thing

legBootVar$bootVarTrans <- asin(sqrt(legBootVar$bootSD_var))
legBootVar$bootVarTrans <- legBootVar$bootVarTrans*100
legBootVar$bootVarTrans <- legBootVar$bootVarTrans +1



myFormula <- bootVarTrans~ logCtFm +  I(logCtFm^2) + InstarNumber + InstarNumber:InstarSex + logCtFm:InstarNumber + logCtFm:InstarNumber:InstarSex + 
		I(InstarNumber^2) + I(InstarNumber^2):InstarSex + logCtFm:I(InstarNumber^2) + logCtFm:I(InstarNumber^2):InstarSex



outputModel<- glmmPQL(myFormula, ~1|NestID, family = gaussian(link = "log") ,
		data = legBootVar, weights = lmrWgts, niter = 20)


runThing <- runGLMMPQR(myFormula, legBootVar)

legBootVar_two <- subset(legBootVar, Instar != "Adult" & bootVarTrans < 160)


myFormula <- bootVarTrans~ logCtFm +  I(logCtFm^2) + InstarNumber + InstarNumber:InstarSex + logCtFm:InstarNumber + logCtFm:InstarNumber:InstarSex + 
		I(InstarNumber^2) + I(InstarNumber^2):InstarSex + logCtFm:I(InstarNumber^2) + logCtFm:I(InstarNumber^2):InstarSex + I(logCtFm^2):InstarNumber +
		I(logCtFm^2):InstarNumber:InstarSex + I(logCtFm^2):I(InstarNumber^2) +  I(logCtFm^2):I(InstarNumber^2):InstarSex

pVal <- 1
rhsFormula <- "something"

sink("LegBootVar_home.txt")
while(pVal > 0.01 & rhsFormula != "1"){
	
	modelOutput <- runGLMMPQR(myFormula, legBootVar)
	myFormula <- reduceFormula(myFormula, modelOutput[[2]])
	
	rhsFormula <- rhs(simplify.formula(myFormula))
	cat("\n\n\rformula:", as.character(simplify.formula(myFormula)))
	
	if(rhsFormula == "1"){
		cat("\n\rNo terms were significant")
	}
	
	pVal <- modelOutput[[3]]
}

sink()
closeAllConnections()
SigFormula <- bootVarTrans ~ InstarNumber + logCtFm:InstarNumber + logCtFm:I(InstarNumber^2) + I(logCtFm^2):InstarNumber + I(logCtFm^2):I(InstarNumber^2)
SigFormula

a_runThing <- runGLMMPQR(SigFormula, legBootVar)

plot(a_runThing[[1]])
addStars_anova(Anova(a_runThing[[1]]))
stargazer(Anova(a_runThing[[1]]))
a_anova <- Anova(a_runThing[[1]])
a_aov <- aov(a_runThing[[1]])
attr(a_anova, "heading")

# Extract the p-values
pvals <- a_anova$`Pr(>Chisq)`

# Use the symnum function to produce the symbols
sigSymbols <- symnum(pvals, na = FALSE, 
        cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
        symbols = c("***", "**", "*", ".", " "), legend = FALSE)

as.data.frame(sigSymbols)
a_thing <- c(sigSymbols, recursive = FALSE)
a_thing <- print(sigSymbols)
stars <- as.matrix(unclass(sigSymbols))

ap <- cbind(a_anova, stars)

InstarGridGraph(legBootVar, "bootVarTrans", "leg boot var", "n", "", a_runThing[[1]], "y")

methods(summary)
methods(class = "aov")
methods("[[")    # uses C-internal dispatching
methods("$")
methods("$<-")   # replacement function
methods("+")     # binary operator
methods("Math")  
methods(class = "Matrix")# nothing
showMethods(class = "Matrix")# everything
methods(Anova)
methods(plot)
methods(class = "glmmPQL")
showMethods(class = "glmmPQL")

testModel <- modelOutput[[1]]
aov(testModel)
deviance(testModel)
summary(testModel)$deviance
a_output <- dredge(testModel)
QAIC(testModel, chat = 1)

results_stars <- addStars_anova(Anova(testModel))