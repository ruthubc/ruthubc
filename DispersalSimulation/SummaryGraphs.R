# TODO: Add comment
# 
# Author: Ruth
###############################################################################

library(ggplot2)
library(plyr)

folder <- "DisperalSimulationOutput/"

dis_aves <- read.csv(paste(folder, "DispersalAves.csv", sep = ""))

xtabs(~ input_var + Comp_slope + meanK + disp_rsk + ad_dsp_fd, data = dis_aves)
 
#dis_aves$KAndAdDisLmt <- paste("K =", dis_aves$meanK, "AdDisFdLm=", dis_aves$ad_dsp_fd)

#possibility for something to check : survivial analysis for individual colonies?

dis_aves[duplicated(dis_aves$fileNum),] # checking there are no duplicated file numbers
 

dis_ply<- ddply(dis_aves, .(Comp_slope, meanK, input_var, disp_rsk, ad_dsp_fd ), summarise, # need to discount trials where no feeding obs and eve
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

dis_ply$KAndAdDisLmt <- paste("K =", dis_ply$meanK, "AdDisFdLm=", dis_ply$ad_dsp_fd)

# Population Age
ggplot(dis_ply, aes(x = Comp_slope, y = PopAge.Mean, colour = as.factor(input_var))) + geom_point() + facet_wrap(~KAndAdDisLmt) + geom_line() +
		geom_errorbar(aes(ymin=PopAge.Mean-PopAge.SE, ymax=PopAge.Mean + PopAge.SE), width = 0.01) + ggtitle("average population age")


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