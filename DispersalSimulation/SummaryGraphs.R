# TODO: Add comment
# 
# Author: Ruth
###############################################################################

library(ggplot2)

folder <- "DisperalSimulationOutput/"

dis_aves <- read.csv(paste(folder, "DispersalAves.csv", sep = ""))

xtabs(~ input_var + Comp_slope + meanK + disp_rsk + ad_dsp_fd, data = dis_aves)
 
dis_aves$KAndAdDisLmt <- paste("K =", dis_aves$meanK, "AdDisFdLm=", dis_aves$ad_dsp_fd)


dis_ply<- ddply(dis_aves, .(Comp_slope, meanK, input_var, disp_rsk, ad_dsp_fd ), summarise, # need to discount trials where no feeding obs and eve
		N = length(!is.na(SpiderID)),
		IndEatDur.Mean = mean(TotalTimeEating, na.rm = TRUE),
		SumIndEat = sum(TotalTimeEating, na.rm = TRUE),
		RankEatDur.Mean = mean(Rank.TimeEating, na.rm = TRUE),
		AveFeed = mean(IndFeedNum, na.rm=TRUE),
		AveCap = mean(IndCapNum, na.rm= TRUE)
)

### ********** Might want to remove populations that did not survive to 200 generations


# colony age vs stuff - will have to think about whether or how to include colonies that are still alive when the simulation ends\
# could also a survival analysis for indivdual colonies

# Colony survival i.e. max colony size
ggplot(dis_aves, aes(x = Comp_slope, y = max_colAge, colour = as.factor(input_var) )) + geom_point() + facet_wrap(~KAndAdDisLmt) + geom_line()

# ave col size b4 dispersal
ggplot(dis_aves, aes(x = Comp_slope, y = ave_colSizeB4Disp, colour = as.factor(input_var) )) + 
		geom_errorbar(aes(ymin= ave_colSizeB4Disp - se_colSizeB4Disp, ymax= ave_colSizeB4Disp + se_colSizeB4Disp), width=.1, position = position_dodge(0.1)) +
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