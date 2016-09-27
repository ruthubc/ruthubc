# TODO: Add comment
# 
# Author: user
###############################################################################


## Size of dispersing population

## vs. ad size and comp

df_DP <- subset(dis_ply, max_no_off == 8 & input_var == 0.2)

ggplot(data = df_DP, aes(x = Comp_meas , y = col_size_disp.mean, colour = as.factor(ad_dsp_fd))) + geom_point() + geom_line() + mytheme

##vs. offspring number and environment variation
levels(as.factor(dis_ply$Comp_meas))

df_DP2 <- subset(dis_ply, Comp_meas== 0.8 & ad_dsp_fd == 0.6)

ggplot(data = df_DP2, aes(x = input_var , y = col_size_disp.mean, colour = as.factor(max_no_off))) + geom_point() + geom_line() + mytheme


ggplot(dis_ply, aes(x = input_var, y = col_size_disp.mean, colour = as.factor(max_no_off))) + geom_point() + 
		facet_grid(ad_dsp_fd~ Comp_meas)  + geom_line()



### dispersal frequency



## survival of population

# vs. comp vs environmental

df_PS <- subset(dis_ply, max_no_off == 4 & input_var == 0.1)

ggplot(data = df_PS, aes(x = Comp_meas , y = log10(survival_all.mean), colour = as.factor(ad_dsp_fd))) + geom_point() + geom_line() + mytheme

###
df_PS2 <- subset(dis_ply, max_no_off == 4)
ggplot(df_PS2, aes(x = input_var, y = log10(survival_all.mean), colour = as.factor(Comp_meas))) + geom_point() + 
		facet_grid(~ad_dsp_fd)  + geom_line() + mytheme

df_PS3 <- subset(dis_ply, input_var == 0 & ad_dsp_fd != 0.6 & ad_dsp_fd != 0.8 )


ggplot(df_PS3, aes(x =max_no_off, y = log10(survival_all.mean), colour = as.factor(Comp_meas))) + geom_point() + 
		facet_grid(~ad_dsp_fd)  + geom_line() + mytheme


########## extinct without dispersing



ggplot(df_PS2, aes(x = input_var, y = MeanPerDieNoDsp, colour = as.factor(Comp_meas))) + geom_point() + 
		facet_grid(~ad_dsp_fd)  + geom_line() + mytheme


df_END <- subset(dis_ply, input_var == 0.1 & Comp_meas != 0.5 & ad_dsp_fd == 0.8)

ggplot(df_END, aes(x = max_no_off, y = MeanPerDieNoDsp, colour = as.factor(Comp_meas))) + geom_point() + 
		geom_line(position = position_jitter(w = 0.06, h = 0.06)) + mytheme


#### Metapopulation survival



