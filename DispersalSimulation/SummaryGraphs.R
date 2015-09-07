# TODO: Add comment
# 
# Author: Ruth
###############################################################################

library(ggplot2)
library(plyr)

## When running this with new  competition slopes, make sure that it updates the comp for the graphs correctly with the lookup table

folder <- "DisperalSimulationOutput/"

dis_aves <- read.csv(paste(folder, "DispersalAves.csv", sep = ""))

xtabs(~ input_var + Comp_slope + meanK + disp_rsk + ad_dsp_fd, data = dis_aves)
 
#dis_aves$KAndAdDisLmt <- paste("K =", dis_aves$meanK, "AdDisFdLm=", dis_aves$ad_dsp_fd)

#possibility for something to check : survivial analysis for individual colonies?

dis_aves[duplicated(dis_aves$fileNum),] # checking there are no duplicated file numbers

levels(as.factor(dis_aves$Comp_slope))
levels(as.factor(dis_aves$disp_rsk))

CompLookUp <- data.frame (Comp_slope = c(0, 0.2, 0.4, 0.6, 0.8, 1, 1.25, 1.33, 2.5, 5, 10), 
		Comp_meas = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))

dis_aves <- merge(dis_aves, CompLookUp, by = "Comp_slope")
 

dis_ply<- ddply(dis_aves, .(Comp_slope, meanK, input_var, disp_rsk, ad_dsp_fd, Comp_meas), summarise, # need to discount trials where no feeding obs and eve
		N = length(!is.na(fileNum)),
		PopAge.Mean = mean(pop_age),
		PopAge.SE = sd(pop_age) / sqrt(N), 
		maxColAge.mean = mean(max_colAge),
		maxColAge.SE = sd(max_colAge)/sqrt(N),
		col_size_disp.mean = mean(col_size_disp),
		col_size_disp.SE = sd(col_size_disp)/sqrt(N),
		colSizeB4Disp.mean = mean(ave_colSizeB4Disp),
		colSizeB4Disp.SE = sd(ave_colSizeB4Disp) / sqrt(N)
		
		
)

dis_ply$KAndAdDisLmt <- paste("K =", dis_ply$meanK, "AdDisFdLm =", dis_ply$ad_dsp_fd)

dis_ply$KAndVar <- paste("K =", dis_ply$meanK, "Var =", dis_ply$input_var)

dis_ply$KVarAndRsk <- paste("K =", dis_ply$meanK, "Var =", dis_ply$input_var, "DisRsk = ", dis_ply$disp_rsk)

dis_ply$VarAndRsk <- paste("Var", dis_ply$input_var, "DR", dis_ply$disp_rsk)



########### Graphs ###############

png("DisperalSimulationOutput/DispersalSummaryGraphs.png", width = 1500, height = 750, units = "px", pointsize = 20)

dis_ply$PopAge.SE <- ifelse(dis_ply$PopAge.SE == 0, NA, dis_ply$PopAge.SE)  # making all zero se's into NA's

# Population Age
ggplot(dis_ply, aes(x = Comp_meas, y = PopAge.Mean, colour = as.factor(ad_dsp_fd))) + geom_point(aes(shape = as.factor(ad_dsp_fd))) + 
		facet_grid(meanK~VarAndRsk)  +  ggtitle("average population age") + geom_line(aes(linetype = as.factor(ad_dsp_fd))) +
		geom_errorbar(aes(ymin=PopAge.Mean-PopAge.SE, ymax=PopAge.Mean + PopAge.SE), width = 0.1)

dev.off()

ggplot(dis_ply, aes(x = Comp_slope, y = PopAge.Mean, colour = as.factor(input_var))) + geom_point(position = position_jitter(w = 0.1, h = 0.1)) + facet_wrap(~KAndAdDisLmt) +
		geom_errorbar(aes(ymin=PopAge.Mean-PopAge.SE, ymax=PopAge.Mean + PopAge.SE), width = 0.01) + ggtitle("average population age") + geom_line()



# Colony survival i.e. max colony age
ggplot(dis_ply, aes(x = Comp_slope, y = maxColAge.mean, colour = as.factor(input_var) )) + geom_point() + facet_wrap(~KAndAdDisLmt) + geom_line() +
		geom_errorbar(aes(ymin=maxColAge.mean-maxColAge.SE, ymax=maxColAge.mean+maxColAge.SE), width = 0.01) + ggtitle("max colony age")


# Colony size at dispersal

ggplot(dis_ply, aes(x = Comp_slope, y = col_size_disp.mean, colour = as.factor(input_var) )) + geom_point() + facet_wrap(~KAndAdDisLmt) + geom_line() +
		geom_errorbar(aes(ymin=col_size_disp.mean-col_size_disp.SE, ymax=col_size_disp.mean+col_size_disp.SE), width = 0.01) + 
		ggtitle("colony size at dispersal")

# Mean colony size












### Old Graphs

# ave col size b4 dispersal
ggplot(dis_aves, aes(x = Comp_slope, y = ave_colSizeB4Disp, colour = as.factor(input_var) )) + 
		geom_errorbar(aes(ymin= ave_colSizeB4Disp - se_colSizeB4Disp, ymax= ave_colSizeB4Disp + se_colSizeB4Disp), width=.01, position = position_dodge(0.1)) +
		geom_point() + facet_wrap(~KAndAdDisLmt) + geom_line() 
		

# max colony size b4disperal
ggplot(dis_aves, aes(x = Comp_slope, y = max_colSizeB4Disp, colour = as.factor(input_var) )) + geom_point() + facet_wrap(~KAndAdDisLmt) + geom_line()

# ave num of dispersers
ggplot(dis_aves, aes(x = Comp_slope, y = ave_disp, colour = as.factor(input_var) )) + geom_point() + facet_wrap(~KAndAdDisLmt) + geom_line()

# ave percentage of dispersers
ggplot(dis_aves, aes(x = Comp_slope, y = ave_perDisp, colour = as.factor(input_var) )) + geom_point() + facet_wrap(~KAndAdDisLmt) + geom_line()

# ave colony size at deaths
ggplot(dis_aves, aes(x = Comp_slope, y = ave_colSize_Death, colour = as.factor(input_var) )) + geom_point() + facet_wrap(~KAndAdDisLmt) + geom_line()

# max population size
ggplot(dis_aves, aes(x = Comp_slope, y = pop_age, colour = as.factor(input_var) )) + geom_point() + facet_wrap(~KAndAdDisLmt) + geom_line()

# ave ad size
ggplot(dis_aves, aes(x = Comp_slope, y = ave_adSze, colour = as.factor(input_var) )) + geom_point() + facet_wrap(~KAndAdDisLmt) + geom_line()

# ave juv size
ggplot(dis_aves, aes(x = Comp_slope, y = ave_juvSze, colour = as.factor(input_var) )) + geom_point() + facet_wrap(~KAndAdDisLmt) + geom_line()