# TODO: Add comment
# 
# Author: Ruth
###############################################################################

setwd("C:/Users/Ruth/Dropbox/")

library(FlexParamCurve)
library(nlshelper)



mytheme <- theme_bw(base_size=15)  + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
		panel.border = element_rect(fill = NA, colour = "black", linetype=1, size = 0.5), 
		legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour ="black"), legend.text=element_text(size=8), 
		strip.background = element_rect(fill="white", size=0.7, linetype="solid", colour ="black"))

importDispFile$log_metPopAgeMax <- importDispFile$metPopAgeMax




# constant variables
env1<- 0.0
no_off <- 6 

subDispFile <- subset(importDispFile, Comp_meas != 0.5)

subDispFile <- subset(subDispFile, max_no_off == no_off & input_var == env1)


aveFile <- ddply(subDispFile, .(ad_dsp_fd, Comp_meas), summarize, 
		metPopAgeMax = mean(metPopAgeMax))

png("RuthSync/Thesis/Presentation/3_compdispNoEnv.png", width = 1550, height = 950, units = "px", res = 200)


'''

ggplot(aveFile, aes(x = as.factor(Comp_meas), y = as.factor(ad_dsp_fd), fill = (metPopAgeMax)))  + 
		geom_tile(colour = "gray50", size=0.5) +
		mytheme + xlab("Degree of contest competition") +  ylab("Minimum dispersal size") + 
		scale_fill_gradient(trans = "log", low="lightcyan", high="navyblue", 
				name = "Metapopulation\nsurvival (generations)", breaks = c(5, 10, 30, 100, 500))
'''

ggplot(aveFile, aes(x = as.factor(Comp_meas), y = as.factor(ad_dsp_fd), fill = (metPopAgeMax)))  + 
		geom_tile(colour = "gray50", size=0.5) +
		mytheme + xlab("Degree of contest competition") +  ylab("Minimum dispersal size") + 
		scale_fill_gradient(low="white", high="grey2", guide = FALSE, breaks = c(5, 10, 30, 100, 500))


dev.off()




# with environmental variation

no_off <- 6 

subDispFile <- subset(importDispFile, Comp_meas != 0.5)

subDispFile_noise <- subset(subDispFile, max_no_off == no_off & input_var > 0.6)


aveFile_noise <- ddply(subDispFile_noise, .(ad_dsp_fd, Comp_meas), summarize, 
		metPopAgeMax = mean(metPopAgeMax))

png("RuthSync/Thesis/Presentation/3_compdispWithNoise.png", width = 1550, height = 950, units = "px", res = 200)

ggplot(aveFile_noise, aes(x = as.factor(Comp_meas), y = as.factor(ad_dsp_fd), fill = (metPopAgeMax)))  + 
		geom_tile(colour = "gray50", size=0.5) +
		mytheme + xlab("Degree of contest competition") +  ylab("Minimum dispersal size") + 
		scale_fill_gradient(trans = "log", low="lightcyan", high="navyblue", 
				name = "Metapopulation\nsurvival", breaks = c(5, 10, 30, 100, 500)) +
		theme(legend.position="bottom")

dev.off()




########### With environmental variation #################################


Label_compLegend <- "degree of\ncontest\ncompetition"
Label_compAxis <- "degree of contest competition"
Label_dspSizeLegend <- "minimum\nindividual\ndispersal size"
Label_dspSizeAxis <- "minimum individual dispersal size"
Label_Met <- "metapopulation survival time"
Label_pop <- "population survival time"

set_ad_dsp_fd1 <- 0.8
set_ad_dsp_fd2 <- 0.6

subDispFile <- subset(importDispFile, max_no_off == no_off)

subDispFile <- subset(subDispFile, Comp_meas != 0.5 & input_var != 0.05 & input_var != 0.15)

subDispFile$nls_crv <- NULL

nrow(subDispFile)


sub_adDsp1.2 <- subset(subDispFile, ad_dsp_fd == 1.2)

png("RuthSync/Thesis/Presentation/3_compdispNoEnv_adDsp1pt2.png", width = 1750, height = 600, units = "px", res = 200)

ggplot(sub_adDsp1.2 , aes(x = input_var, y =  (metPopAgeMax), color = as.factor(Comp_meas))) + 
		geom_point(size = 1, position = position_jitter(w = 0.002, h = 0), pch = 18) + 
		mytheme + ylab("Metapopulation survival") + 
		scale_color_discrete(Label_compLegend) + 
		scale_shape_discrete(Label_compLegend) + 
		xlab("environmental variation")+ 
		scale_y_log10(breaks = c(5, 10, 30, 100, 500)) +
		stat_smooth(method = "lm",  formula = y ~ x + I(x^2), se = FALSE)
		
dev.off()



sub_adDsp0.8 <- subset(subDispFile, ad_dsp_fd == 0.8 & Comp_meas == 0.2)
sub_adDsp0.8 <- subset(subDispFile, ad_dsp_fd == 0.8)
nrow(sub_adDsp0.8)

my_nls <- nls(log_metPopAgeMax ~ 2.7/(1+exp(-k * (input_var - z))) ,  data = sub_adDsp0.8, start = list(k = 1, z = 0.2))

my_seq <- seq(0, 0.8, by = 0.001)
my_pdt_nls<- predict(my_nls, list(log_metPopAgeMax = my_seq)) # not working
points(x = sub_adDsp0.8$input_var, y = sub_adDsp0.8$log_metPopAgeMax,  pch = 9)

plot(x = seq(0, 0.8, by = 0.001), pdt_nls, pch = ".")





png("RuthSync/Thesis/Presentation/3_compdispNoEnv_adDsp0pt8.png", width = 1950, height = 600, units = "px", res = 200)



ggplot(sub_adDsp0.8 , aes(x = input_var, y =  metPopAgeMax, color = as.factor(Comp_meas))) + 
		geom_point(size = 2, position = position_jitter(w = 0.002, h = 0), pch = 18) + 
		mytheme + ylab("Metapopulation survival") + 
		xlab("environmental variation")+ 
		scale_y_log10(breaks = c(5, 10, 30, 100, 500)) +
		geom_smooth(se = FALSE, method = "lm",  formula = y ~ x + I(x^2))
		

dev.off()		
		
		
		
		
		stat_smooth(method = 'nls', se = FALSE, 
				method.args = list(
						formula = y ~ 2.7/(1+exp(-k * (x - z))), 
						start = list(k = 1, z = 0.2)
						))



scale_y_log10(breaks = c(5, 10, 30, 100, 500)) +
############## Dispersal Increases ###################################

subDispFile <- subset(importDispFile, max_no_off == no_off)

subDispFile <- subset(subDispFile, Comp_meas != 0.5 & input_var != 0.05 & input_var != 0.15)


dispEnv <- subset(subDispFile, (ad_dsp_fd == 0.6 & Comp_meas == 0) | (ad_dsp_fd == 0.8 & Comp_meas == 0) | (ad_dsp_fd == 0.8 & Comp_meas == 0.2))

dispEnv$all_ave_num_disp <- ifelse(is.na(dispEnv$all_ave_num_disp) == TRUE , 0, dispEnv$all_ave_num_disp)

dispEnv$Legend <- ifelse(dispEnv$ad_dsp_fd == 0.6 & dispEnv$Comp_meas == 0, "comp=0.0 and dispersal size=0.6", 
		ifelse(dispEnv$ad_dsp_fd == 0.8 & dispEnv$Comp_meas == 0, "comp=0.0 and dispersal size=0.8", 
				ifelse(dispEnv$ad_dsp_fd == 0.8 & dispEnv$Comp_meas == 0.2, "comp=0.2 and dispersal size=0.8", "MISTAKE")))

dispSum <- summarySE(dispEnv, measurevar="all_ave_num_disp", groupvars=c("input_var","Legend"))

png("RuthSync/Thesis/Presentation/3_compDispIncreaes.png", width = 1500, height = 800, units = "px", res = 200)

ggplot(dispSum, aes(x=input_var, y=all_ave_num_disp, color = Legend)) + 
		geom_errorbar(aes(ymin=all_ave_num_disp-se, ymax=all_ave_num_disp+se), width=.03) +
		geom_point(size = 2) + mytheme+
		geom_line()  + 
		xlab("Environmental variation") + ylab("Number of dispersing colonies") +
		scale_shape_manual(values=c(0, 2, 19)) +
		scale_color_discrete("Competition and\ndispersal size") + 
		scale_shape_discrete("Competition and\ndispersal size")

dev.off()

