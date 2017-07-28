# TODO: Add comment
# 
# Author: Ruth
###############################################################################

setwd("C:/Users/Ruth/Dropbox/")

library(FlexParamCurve)
library(nlshelper)



mytheme <- theme_bw(base_size=10)  + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
		panel.border = element_rect(fill = NA, colour = "black", linetype=1, size = 0.5), 
		legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour ="black"), legend.text=element_text(size=8), 
		strip.background = element_rect(fill="white", size=0.7, linetype="solid", colour ="black"))



# constant variables
env1<- 0.0
no_off <- 6 

subDispFile <- subset(importDispFile, Comp_meas != 0.5)

subDispFile <- subset(subDispFile, max_no_off == no_off & input_var == env1)


aveFile <- ddply(subDispFile, .(ad_dsp_fd, Comp_meas), summarize, 
		metPopAgeMax = mean(metPopAgeMax))

png("RuthSync/Thesis/Presentation/3_compdispNoEnv.png", width = 1300, height = 950, units = "px", res = 200)




ggplot(aveFile, aes(x = as.factor(Comp_meas), y = as.factor(ad_dsp_fd), fill = (metPopAgeMax)))  + 
		geom_tile(colour = "gray50", size=0.5) +
		mytheme + xlab("Degree of contest competition") +  ylab("Minimum dispersal size") + 
		scale_fill_gradient(trans = "log", low="lightcyan", high="navyblue", 
				name = "Metapopulation\nsurvival (generations)", breaks = c(5, 10, 30, 100, 500))

dev.off()



## With environmental variation

set_ad_dsp_fd1 <- 0.8
set_ad_dsp_fd2 <- 0.6

subDispFile <- subset(importDispFile, max_no_off == no_off)

subDispFile <- subset(subDispFile, Comp_meas != 0.5 & input_var != 0.05 & input_var != 0.15)

nrow(subDispFile)


sub_adDsp1.2 <- subset(subDispFile, ad_dsp_fd == 1.2)

png("RuthSync/Thesis/Presentation/3_compdispNoEnv_adDsp1pt2.png", width = 1800, height = 600, units = "px", res = 200)

ggplot(sub_adDsp1.2 , aes(x = input_var, y =  (metPopAgeMax), color = as.factor(Comp_meas))) + 
		geom_point(size = 1, position = position_jitter(w = 0.002, h = 0), pch = 18) + 
		mytheme + ylab("Metapopulation survival (generations)") + 
		scale_color_discrete(Label_compLegend) + 
		scale_shape_discrete(Label_compLegend) + 
		xlab("environmental variation")+ 
		scale_y_log10(breaks = c(5, 10, 30, 100, 500)) +
		stat_smooth(method = 'nls', se = FALSE, formula = y ~ a * log(x+0.1) + b)
		
dev.off()



sub_adDsp0.8 <- subset(subDispFile, ad_dsp_fd == 0.8)


png("RuthSync/Thesis/Presentation/3_compdispNoEnv_adDsp0pt8.png", width = 1800, height = 600, units = "px", res = 200)

ggplot(sub_adDsp0.8 , aes(x = input_var, y =  (metPopAgeMax), color = as.factor(Comp_meas))) + 
		geom_point(size = 1, position = position_jitter(w = 0.002, h = 0), pch = 18) + 
		mytheme + ylab("Metapopulation survival (generations)") + 
		scale_color_discrete(Label_compLegend) + 
		scale_shape_discrete(Label_compLegend) + 
		xlab("environmental variation")+ 
		scale_y_log10(breaks = c(5, 10, 30, 100, 500)) +
		stat_smooth(method = 'nls', se = FALSE, formula = y ~ (a * exp(-x+0.1) + b))

dev.off()




modseltable <- pn.mod.compare(x = sub_adDsp0.8$metPopAgeMax, y = sub_adDsp0.8$input_var,
		grp = sub_adDsp0.8$Comp_meas, 
		existing = FALSE, 
		pn.options = "myoptions", Envir = FlexParamCurve:::FPCEnv)

#create starting values
modParOutput <- modpar(sub_adDsp0.8$input_var, sub_adDsp0.8$metPopAgeMax, pn.options = "myoptions.a")


x = 0.2

y <- posnegRichards.eqn(x,Asym = modParOutput$Asym, K = modParOutput$K, Infl = modParOutput$Infl, M = modParOutput$M, 
		RAsym = modParOutput$RAsym, Rk = modParOutput$Rk, Ri = modParOutput$Ri, 
		modno = 2, pn.options = "myoptions.a")


print( c(x = x, y = y) )

plot(x = sub_adDsp0.8$input_var, y = sub_adDsp0.8$metPopAgeMax,  pch = 16)

myCurvecurve <- curve(posnegRichards.eqn(x,modno = 2, pn.options = "myoptions.a"), add = TRUE, lwd = 3)
		
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