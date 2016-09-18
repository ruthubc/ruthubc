# TODO: Add comment
# 
# Author: user
###############################################################################


myDF <- subset(dis_ply, Comp_meas != 0.5)



ggplot(myDF, aes(x = Comp_meas, y = input_var, fill = log10(survival_all.mean)))  + geom_raster()


ggplot(subset(myDF,  !is.na(pcntDisp.mean)) , aes(x = Comp_meas, y = max_no_off, fill = pcntDisp.mean))  + geom_raster()

ggplot(subset(myDF,  !is.na(col_size_disp.mean)), aes(x = Comp_meas, y = ad_dsp_fd, fill = col_size_disp.mean)) + geom_raster()

ggplot(subset(myDF,  !is.na(col_size_disp.mean)), aes(x = Comp_meas, y = max_no_off, fill = col_size_disp.mean)) + geom_raster()