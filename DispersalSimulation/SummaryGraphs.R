# TODO: Add comment
# 
# Author: Ruth
###############################################################################

library(ggplot2)
library(plyr)
library(gridExtra)

## When running this with new  competition slopes, make sure that it updates the comp for the graphs correctly with the lookup table

folder <- "DisperalSimulationOutput/"

dis_aves <- read.csv(paste(folder, "DispersalAves.csv", sep = ""))

rownames(dis_aves)


xtabs(~ input_var + Comp_slope + meanK + disp_rsk + ad_dsp_fd, data = dis_aves)

dis_aves$full <- ifelse(dis_aves$pop_age > 100, 1, 0) # marking which colonies survived over 100 generations and which didn't

dis_aves$dis_bin <- ifelse(is.na(dis_aves$ave_num_disp), ifelse(dis_aves$full == 1, 0, NA), 1) # making binary variable of whether any dispersal occured

print("did any populations have colonies that did not disperse?")
nrow(subset(dis_aves, dis_bin == 0))  # checking if any surviving populations had any colonies that did not disperse

dis_aves$all_dis_bin <- ifelse(is.na(dis_aves$all_ave_num_disp), 0, 1)


## Making binary measurement of colony death

dis_aves$all_death_bin<- ifelse(is.na(dis_aves$all_ave_colSize_Death), 0, 1)



#possibility for something to check : survivial analysis for individual colonies?

nrow(dis_aves[duplicated(dis_aves$fileNum),]) # checking there are no duplicated file numbers

levels(as.factor(dis_aves$Comp_slope))
levels(as.factor(dis_aves$disp_rsk))

CompLookUp <- data.frame (Comp_slope = c(0, 0.2, 0.4, 0.6, 0.8, 1, 1.25, 1.33, 2.5, 5, 10), 
		Comp_meas = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))

dis_aves <- merge(dis_aves, CompLookUp, by = "Comp_slope")

## changing colony age at death to 500 for those populations that had no deaths
dis_aves$ave_colAge_Death[is.na(dis_aves$ave_colAge_Death) & dis_aves$pop_age > 100] <- 500

levels(as.factor(dis_aves$ave_colAge_Death))

dis_ply<- ddply(dis_aves, .(Comp_slope, meanK, input_var, disp_rsk, ad_dsp_fd, Comp_meas), summarise, # need to discount trials where no feeding obs and eve
		N_all = length(!is.na(fileNum)),
		N = length(fileNum[which(!is.na(tot_num_cols))]),
		PopAge.Mean = mean(pop_age, na.rm=TRUE),
		PopAge.SE = sd(pop_age, na.rm = TRUE) / sqrt(N_all), 
		maxColAge.mean = mean(max_colAge, na.rm = TRUE),
		maxColAge.SE = sd(max_colAge, na.rm = TRUE)/sqrt(N),
		col_size_disp.mean = mean(col_size_disp, na.rm  = TRUE),
		col_size_disp.SE = sd(col_size_disp, na.rm = TRUE)/sqrt(N),
		colSizeB4Disp.mean = mean(ave_colSizeB4Disp, na.rm = TRUE),
		colSizeB4Disp.SE = sd(ave_colSizeB4Disp, na.rm = TRUE) / sqrt(N),
		colAgeDeath_all.mean = mean(all_ave_colAge_Death, na.rm = TRUE),
		colAgeDeath_all.SE = sd(all_ave_colAge_Death, na.rm = TRUE)/ sqrt(N_all),
		colDeath_bin_all.mean = mean(all_death_bin),
		colSizeDisp.mean = mean(col_size_disp, na.rm = TRUE),
		colSizeDisp.SE = sd(col_size_disp, na.rm = TRUE)/sqrt(N),
		pcntDisp.mean = mean(ave_perDisp, na.rm = TRUE),
		pcntDisp.SE = sd(ave_perDisp, na.rm = TRUE), sqrt(N),
		colSizeDeath.mean = mean(ave_colSize_Death, na.rm = TRUE),
		colSizeDeath.SE = sd(ave_colSize_Death, na.rm = TRUE)/ sqrt(N)
	
)

dis_ply$KAndAdDisLmt <- paste("K =", dis_ply$meanK, "AdDisFdLm =", dis_ply$ad_dsp_fd)

dis_ply$KAndVar <- paste("K =", dis_ply$meanK, "Var =", dis_ply$input_var)

dis_ply$KVarAndRsk <- paste("K =", dis_ply$meanK, "Var =", dis_ply$input_var, "DisRsk = ", dis_ply$disp_rsk)

dis_ply$VarAndRsk <- paste("Var=", dis_ply$input_var, ", DR=", dis_ply$disp_rsk)



########### Graphs ###############

mytheme <-theme_bw(base_size=18)  + theme(plot.title = element_text(vjust=2), panel.margin= unit(0.75, "lines"),
		plot.margin=unit(c(1,1,1.5,1.2),"cm"), panel.border = element_rect(fill = NA, colour = "grey", linetype=1, size = 1))


dis_ply$PopAge.SE <- ifelse(dis_ply$PopAge.SE == 0, NA, dis_ply$PopAge.SE)  # making all zero se's into NA's

# Population Age
p1 <- ggplot(dis_ply, aes(x = Comp_meas, y = PopAge.Mean, colour = as.factor(ad_dsp_fd))) + geom_point(aes(shape = as.factor(ad_dsp_fd)), size = 3) + 
		facet_grid(meanK~VarAndRsk)  +  ggtitle("average population age") + geom_line(aes(linetype = as.factor(ad_dsp_fd))) +
		geom_errorbar(aes(ymin=PopAge.Mean-PopAge.SE, ymax=PopAge.Mean + PopAge.SE), width = 0.1) + mytheme + scale_colour_manual(values=c("blue", "red"))
 
### Not sure whether better to graph all generations??? Prob not. 
# Average colony age at death for all colonies
p2 <- ggplot(dis_ply, aes(x = Comp_meas, y = colAgeDeath_all.mean, colour = as.factor(ad_dsp_fd))) + geom_point(aes(shape = as.factor(ad_dsp_fd)), size = 3) + 
		facet_grid(meanK~VarAndRsk)  +  ggtitle("average colony age at death") + geom_line(aes(linetype = as.factor(ad_dsp_fd))) +
		geom_errorbar(aes(ymin=colAgeDeath_all.mean-colAgeDeath_all.SE, ymax=colAgeDeath_all.mean + colAgeDeath_all.SE), width = 0.1) + 
		mytheme + scale_colour_manual(values=c("blue", "red"))


# Colony size at dispersal

p3 <- ggplot(dis_ply, aes(x = Comp_meas, y = col_size_disp.mean, colour = as.factor(ad_dsp_fd))) + geom_point(aes(shape = as.factor(ad_dsp_fd)), size = 3) + 
		facet_grid(meanK~VarAndRsk, scales = "free")  +  ggtitle("average colony size at dispersal") + geom_line(aes(linetype = as.factor(ad_dsp_fd))) +
		geom_errorbar(aes(ymin=col_size_disp.mean-col_size_disp.SE, ymax=col_size_disp.mean + col_size_disp.SE), width = 0.1) + mytheme + 
		scale_colour_manual(values=c("blue", "red"))

# binary of whether any colonies died
p4 <- ggplot(dis_ply, aes(x = Comp_meas, y = colDeath_bin_all.mean, colour = as.factor(ad_dsp_fd))) + geom_point(aes(shape = as.factor(ad_dsp_fd)), size = 3) + 
		facet_grid(meanK~VarAndRsk, scales = "free")  +  ggtitle("Did any colonies die?") + geom_line(aes(linetype = as.factor(ad_dsp_fd))) +
		mytheme + scale_colour_manual(values=c("blue", "red"))

# average colony size
p5 <- ggplot(dis_ply, aes(x = Comp_meas, y = colSizeB4Disp.mean, colour = as.factor(ad_dsp_fd))) + geom_point(aes(shape = as.factor(ad_dsp_fd)), size = 3) + 
		facet_grid(meanK~VarAndRsk, scales = "free")  +  ggtitle("Average colony size (measured before dispersal)") + geom_line(aes(linetype = as.factor(ad_dsp_fd))) +
		mytheme + scale_colour_manual(values=c("blue", "red")) + 
		geom_errorbar(aes(ymin=colSizeB4Disp.mean-colSizeB4Disp.SE, ymax=colSizeB4Disp.mean + colSizeB4Disp.SE), width = 0.1)

# size of colonies that disperse

p6 <- ggplot(dis_ply, aes(x = Comp_meas, y = colSizeDisp.mean, colour = as.factor(ad_dsp_fd))) + geom_point(aes(shape = as.factor(ad_dsp_fd)), size = 3) + 
		facet_grid(meanK~VarAndRsk, scales = "free")  +  ggtitle("Average colony size at dispersal") + geom_line(aes(linetype = as.factor(ad_dsp_fd))) +
		mytheme + scale_colour_manual(values=c("blue", "red")) + 
		geom_errorbar(aes(ymin=colSizeDisp.mean-colSizeDisp.SE, ymax=colSizeDisp.mean + colSizeDisp.SE), width = 0.1)

# percentage of individuals dispersal

p7 <- ggplot(dis_ply, aes(x = Comp_meas, y = pcntDisp.mean, colour = as.factor(ad_dsp_fd))) + geom_point(aes(shape = as.factor(ad_dsp_fd)), size = 3) + 
		facet_grid(meanK~VarAndRsk, scales = "free")  +  ggtitle("Average percentage that disperse") + geom_line(aes(linetype = as.factor(ad_dsp_fd))) +
		mytheme + scale_colour_manual(values=c("blue", "red")) + 
		geom_errorbar(aes(ymin=pcntDisp.mean-pcntDisp.SE, ymax=pcntDisp.mean + pcntDisp.SE), width = 0.1)

# colony size at death

p8 <- ggplot(dis_ply, aes(x = Comp_meas, y = colSizeDeath.mean, colour = as.factor(ad_dsp_fd))) + geom_point(aes(shape = as.factor(ad_dsp_fd)), size = 3) + 
		facet_grid(meanK~VarAndRsk, scales = "free")  +  ggtitle("Colony Size at Death") + geom_line(aes(linetype = as.factor(ad_dsp_fd))) +
		mytheme + scale_colour_manual(values=c("blue", "red")) + 
		geom_errorbar(aes(ymin=colSizeDeath.mean-colSizeDeath.SE, ymax=colSizeDeath.mean + colSizeDeath.SE), width = 0.1)




num_graphs <- 8

gr_ht <- num_graphs * 650

png("DisperalSimulationOutput/DispersalSummaryGraphs.png", width = 1300, height = gr_ht, units = "px")

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8,  ncol=1)

dev.off()


