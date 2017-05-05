# TODO: Add comment
# 
# Author: Ruth
###############################################################################

library(survival)



no_off <- 6 
subDispFile <- subset(importDispFile, max_no_off == no_off)

subDispFile <- subset(subDispFile, Comp_meas != 0.5)

dataFrame <- subDispFile


metSurv_df <- data.frame(time = numeric(), metPopCond = factor(), Comp_slope = factor(), ad_dsp_fd = factor())


for(i in 1:nrow(dataFrame)) {
	
	row <- dataFrame[i,]
	
	maxAge <- row$pop_age
	
	comp_slope <- row$Comp_slope
	
	Ad_Dsp_Fd <- row$ad_dsp_fd
	
	df <- data.frame(time = 1:maxAge , metPopCond = 0, Comp_slope = comp_slope, ad_dsp_fd = Ad_Dsp_Fd) # 0 = alive 1 = dead
	
	df$metPopCond[which(df$time == maxAge)] <- 1
	
	metSurv_df <- rbind(df, metSurv_df)
	
	
	
}

msurv <- with(metSurv_df, Surv(time, metPopCond)) # getting the survival thing

summary(msurv)

coxph(msurv)

survfit(msurv ~ 1)

a_output <- survfit(Surv(time, metPopCond) ~ Comp_slope, data = metSurv_df)

svFit <- survfit(Surv(time, metPopCond) ~ Comp_slope, data = metSurv_df)

summary(svFit)
print(svFit)


pyears <- pyears(Surv(time, metPopCond) ~ Comp_slope, data = metSurv_df)

coxx <- coxph(Surv(time, metPopCond) ~ Comp_slope, data = metSurv_df)

output <- anova(coxx)

summary(a_output)

summary(msurv)
survivalMean <- as.data.frame(mean(msurv[,1])) # don't use mean without the [,1] by itself, wrong!

survivalMean

survexp(time ~ 1, data = metSurv_df, times = metPopCond)

names(survivalMean)[1] <- "survivalMean_all"



ggplot(subDispFile, aes(x = input_var, y = log10(metPopAgeMax), colour = as.factor(Comp_meas))) + 
		geom_point(size = pointSize, position = position_jitter(w = 0, h = 0.05)) + 
		geom_smooth(se = FALSE) + mytheme + ylab("Log max metapopulation age") + 
		scale_colour_discrete("Competition") + xlab("Environmental variation") +
		scale_y_continuous(breaks=seq(0, 3.0, by=0.5), limits = c(0, 3.0)) + facet_wrap(~ad_dsp_fd)

