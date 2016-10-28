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


legBootVar$bootVarTrans <- asin(sqrt(legBootVar$bootSD_var))
legBootVar$bootVarTrans <- legBootVar$bootVarTrans +1
legBootVar$bootVarTrans <- legBootVar$bootVarTrans*100


myFormula <- bootVarTrans~ logCtFm +  I(logCtFm^2) + InstarNumber + InstarNumber:InstarSex + logCtFm:InstarNumber + logCtFm:InstarNumber:InstarSex + 
		I(InstarNumber^2) + I(InstarNumber^2):InstarSex + logCtFm:I(InstarNumber^2) + logCtFm:I(InstarNumber^2):InstarSex

outputModel<- glmmPQL(myFormula, ~1|NestID, family = gaussian(link = "log") ,
		data = legBootVar, weights = lmrWgts, niter = 20)
Anova(outputModel)

outputModel <- update(outputModel, .~. - logCtFm:InstarNumber)

Anova(outputModel)

outputModel <- update(outputModel, .~. - logCtFm:InstarNumber:InstarSex )

Anova(outputModel)


outputModel <-update(outputModel, .~. - logCtFm:I(InstarNumber^2):InstarSex )
Anova(outputModel)

outputModel <-update(outputModel, .~. - logCtFm )
Anova(outputModel)



Anova(update(outputModel, .~. - logCtFm:I(InstarNumber^2)))

Anova(update(outputModel, .~. - logCtFm:I(InstarNumber^2):InstarSex))


plot(outputModel)

myDataSet <- legBootVar

pVal <- 1
while(pVal > 0.01){
	
	modelOutput <- runGLMMPQR(myFormula)
	myFormula <- reduceFormula(myFormula, modelOutput[[2]])
	pVal <- modelOutput[[3]]
	
	
}


