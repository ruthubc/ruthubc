# TODO: Add comment
# 
# Author: Ruth
###############################################################################


sub_adDsp0.8$log_metPopAgeMax <- log10(sub_adDsp0.8$metPopAgeMax)

modseltable <- pn.mod.compare(x = sub_adDsp0.8$log_metPopAgeMax, y = sub_adDsp0.8$input_var,
		grp = sub_adDsp0.8$Comp_meas, 
		existing = FALSE, 
		pn.options = "myoptions", Envir = FlexParamCurve:::FPCEnv)

#create starting values
modParOutput <- modpar(sub_adDsp0.8$input_var, sub_adDsp0.8$log_metPopAgeMax, pn.options = "myoptions.a")


x = 0.2

y <- posnegRichards.eqn(x,Asym = modParOutput$Asym, K = modParOutput$K, Infl = modParOutput$Infl, M = modParOutput$M, 
		RAsym = modParOutput$RAsym, Rk = modParOutput$Rk, Ri = modParOutput$Ri, 
		modno = 1, pn.options = "myoptions.a")


print( c(x = x, y = y) )

points(x = sub_adDsp0.8$input_var, y = sub_adDsp0.8$log_metPopAgeMax,  pch = 9)

plot(x = sub_adDsp0.8$input_var, predict(my_nls))

myCurvecurve <- curve(posnegRichards.eqn(x,modno = 32, pn.options = "myoptions.a"), add = TRUE, lwd = 3)

richardsR2.lis <- nls(metPopAgeMax ~ SSposnegRichards(input_var,
				Asym = modParOutput$Asym, K = modParOutput$K, Infl = modParOutput$Infl, M = modParOutput$M, 
				RAsym = modParOutput$RAsym, Rk = modParOutput$Rk, Ri = modParOutput$Ri, 
				modno = 2, pn.options = "myoptions.a"), data = sub_adDsp0.8)

richardsR31.nls <- nls(metPopAgeMax ~ SSposnegRichards(input_var , Asym = Asym , K = K , Infl = Infl,RAsym = RAsym, modno = 31, pn.options = "myoptions.a"), data = sub_adDsp0.8)

#Extract mean coefficients for nls curve

nlsparams <- coef(richardsR31.nls)



subband <- substring(row.names(subcoef), nchar(as.character(row.names(subcoef)))- 2, nchar(as.character(row.names(subcoef))) )



nlsList(model, data, start, control, level, subset,
		na.action = na.fail, pool = TRUE, warn.nls = NA)


plot(as.environment(".GlobalEnv")$nlsOutput[[1L]][[16L]]())

seq(0,10)


nlsList(log(metPopAgeMax) ~ SSposnegRichards(input_var, modno = 32), data = sub_adDsp0.8)

geom_smooth(method="nls", formula=y~1+Vmax*(1-exp(-x/tau)), # this is an nls argument
		method.args = list(start=c(tau=0.2,Vmax=2)), # this too
		se=FALSE) + 
