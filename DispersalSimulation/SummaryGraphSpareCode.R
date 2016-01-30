# TODO: Add comment
# 
# Author: user
###############################################################################




########### Graphs by comp measure ################


pa1 <- ggplot(dis_ply, aes(x = ad_dsp_fd, y = PopAge.Mean, colour = as.factor(Comp_meas))) + geom_point(size = 3) + 
		myFacet  +  ggtitle("average population age") + geom_line() + mytheme
#geom_errorbar(aes(ymin=PopAge.Mean-PopAge.SE, ymax=PopAge.Mean + PopAge.SE), width = 0.1) + # + scale_colour_manual(values=c("blue", "red"))

### Not sure whether better to graph all generations??? Prob not. 
# Average colony age at death for all colonies

pa2<- ggplot(dis_ply, aes(x = ad_dsp_fd, y = survival_all.mean, colour = as.factor(Comp_meas))) + geom_point(size = 3) + 
		myFacet  +  ggtitle("Average colony Survival") + geom_line() + mytheme
#geom_errorbar(aes(ymin=survival_all.mean-survival_all.SE, ymax=survival_all.mean + survival_all.SE), width = 0.1) + 
#+ scale_colour_manual(values=c("blue", "red"))




# Colony size at dispersal

pa3 <- ggplot(dis_ply, aes(x = ad_dsp_fd, y = col_size_disp.mean, colour = as.factor(Comp_meas))) + geom_point(size = 3) + 
		myFacet +  ggtitle("average colony size of dispersing colonies") + geom_line() +
		geom_errorbar(aes(ymin=col_size_disp.mean-col_size_disp.SE, ymax=col_size_disp.mean + col_size_disp.SE), width = 0.1) + mytheme# + scale_colour_manual(values=c("blue", "red"))

# binary of whether any colonies died
pa4 <- ggplot(dis_ply, aes(x = ad_dsp_fd, y = colDeath_bin_all.mean, colour = as.factor(Comp_meas))) + geom_point(size = 3) + 
		myFacet +  ggtitle("Did any colonies die?") + geom_line() +
		mytheme # + scale_colour_manual(values=c("blue", "red"))

# average colony size
pa5 <- ggplot(dis_ply, aes(x = ad_dsp_fd, y = colSizeB4Disp.mean, colour = as.factor(Comp_meas))) + geom_point(size = 3) + 
		myFacet +  ggtitle("Average colony size (measured before dispersal)") + geom_line() +
		mytheme +  
		geom_errorbar(aes(ymin=colSizeB4Disp.mean-colSizeB4Disp.SE, ymax=colSizeB4Disp.mean + colSizeB4Disp.SE), width = 0.1) # + scale_colour_manual(values=c("blue", "red"))

# size of colonies that disperse

pa6 <- ggplot(dis_ply, aes(x =ad_dsp_fd, y = colSizeDisp.mean, colour = as.factor(Comp_meas))) + geom_point(size = 3) + 
		myFacet +  ggtitle("Average colony size at dispersal") + geom_line() +
		mytheme + geom_errorbar(aes(ymin=colSizeDisp.mean-colSizeDisp.SE, ymax=colSizeDisp.mean + colSizeDisp.SE), width = 0.1) # + scale_colour_manual(values=c("blue", "red")) 


# percentage of individuals dispersal

pa7 <- ggplot(dis_ply, aes(x =ad_dsp_fd , y = pcntDisp.mean, colour = as.factor(Comp_meas))) + geom_point(size = 3) + 
		myFacet +  ggtitle("Average percentage that disperse") + geom_line() +
		mytheme + 
		geom_errorbar(aes(ymin=pcntDisp.mean-pcntDisp.SE, ymax=pcntDisp.mean + pcntDisp.SE), width = 0.1) #+ scale_colour_manual(values=c("blue", "red"))

# colony size at death

pa8 <- ggplot(dis_ply, aes(x = ad_dsp_fd, y = colSizeDeath.mean, colour = as.factor(Comp_meas))) + geom_point(size = 3) + 
		myFacet +  ggtitle("Colony Size at Death") + geom_line() + mytheme + 
		geom_errorbar(aes(ymin=colSizeDeath.mean-colSizeDeath.SE, ymax=colSizeDeath.mean + colSizeDeath.SE), width = 0.1) # + scale_colour_manual(values=c("blue", "red")) 


#grid.arrange(p1, p2, p3, p4, p4a, p5, p6, p7, p8, p9, pa1, pa2,  pa3, pa4, pa5, pa6, pa7, pa8, ncol=1)
