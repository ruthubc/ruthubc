# TODO: Add comment
# 
# Author: Ruth
###############################################################################

library(ggplot2)
library(plyr)
library(gridExtra)
library(grid) # not sure if I need this or not.

## Combining csvs by cmd line http://www.tomnash.eu/how-to-combine-multiple-csv-files-into-one-using-cmd/

num_graphs <- 18 #aka length of doc

gr_ht <- num_graphs * 650

#png("DisperalSimulationOutput/DispersalJuly2016.png", width = 1600, height = gr_ht, units = "px")



## When running this with new  competition slopes, make sure that it updates the comp for the graphs correctly with the lookup table
setwd("C:/Work/ownCloud/")
folder <- "DisperalSimulationOutput/"

dis_aves <- read.csv(paste(folder, "SummaryCombine26July2016.csv", sep = ""))



#dis_aves <- subset(dis_aves, Fd_ln == 0.61)

numGensRmv <- 50



xtabs(~ input_var + Comp_slope + meanK + disp_rsk + ad_dsp_fd + max_no_off,  data = dis_aves)

dis_aves$full <- ifelse(dis_aves$pop_age > numGensRmv, 1, 0) # marking which colonies survived over num generations and which didn't

#dis_aves$dis_bin <- ifelse(is.na(dis_aves$ave_num_disp), ifelse(dis_aves$full == 1, 0, NA), 1) # making binary variable of whether any dispersal occured

print("did any populations have colonies that did not disperse?")

dis_aves$any_disp <- ifelse(dis_aves$num_cols_disp > 0, 1, 0)


## Making binary measurement of colony death

dis_aves$all_death_bin<- ifelse(dis_aves$num_cols_die > 0, 1, 0)




#possibility for something to check : survivial analysis for individual colonies?

nrow(dis_aves[duplicated(dis_aves$fileNum),]) # checking there are no duplicated file numbers

levels(as.factor(dis_aves$Comp_slope))
levels(as.factor(dis_aves$disp_rsk))

CompLookUp <- data.frame (Comp_slope = c(0, 0.2, 0.4, 0.6, 0.8, 1, 1.25, 1.33, 2.5, 5, 10), 
		Comp_meas = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))

dis_aves <- merge(dis_aves, CompLookUp, by = "Comp_slope")

## changing colony age at death to 500 for those populations that had no deaths
dis_aves$ave_colAge_Death[is.na(dis_aves$ave_colAge_Death) & dis_aves$pop_age > numGensRmv] <- 500

levels(as.factor(dis_aves$ave_colAge_Death))

dis_ply<- ddply(dis_aves, .(Comp_slope, meanK, input_var, disp_rsk, ad_dsp_fd, Comp_meas, max_no_off), summarise, # need to discount trials where no feeding obs and eve
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
		colSizeDeath_all.mean = mean(all_ave_colSize_Death, na.rm = TRUE),
		pcntDisp.mean = mean(ave_perDisp, na.rm = TRUE),
		pcntDisp.SE = sd(ave_perDisp, na.rm = TRUE), sqrt(N),
		colSizeDeath.mean = mean(ave_colSize_Death, na.rm = TRUE),
		colSizeDeath.SE = sd(ave_colSize_Death, na.rm = TRUE)/ sqrt(N),
		survival_all.mean = mean(survivalMean_all, na.rm = TRUE),
		survival_all.SE = sd(survivalMean_all, na.rm = TRUE)/ sqrt(N_all),
		anyDisp.mean = mean(any_disp, na.rm = TRUE),
		propSigNestsGrow.mean = mean((1-propSigNestNotGrow), na.rm = TRUE),
		MeanPopSize = mean(MeanNoCols, na.rm = TRUE),
		MeanDispFreq = mean(mean_freq_disp, na.rm = TRUE),
		MeanPerColsDisp = mean(perc_cols_disp, na.rm = TRUE),
		MeanPerDieNoDsp = mean(perc_die_no_disp, na.rm = TRUE),
		MeanPopSizeVar = mean(var_pop_size, na.rm = TRUE),
		MeanPopSizeVarNoSig = mean(var_pop_size_noSig, na.rm = TRUE)
		
		

	
)

dis_ply$KAndAdDisLmt <- paste("K =", dis_ply$meanK, "AdDisFdLm =", dis_ply$ad_dsp_fd)

dis_ply$KAndVar <- paste("K =", dis_ply$meanK, "Var =", dis_ply$input_var)

dis_ply$KVarAndRsk <- paste("K =", dis_ply$meanK, "Var =", dis_ply$input_var, "DisRsk = ", dis_ply$disp_rsk)

dis_ply$VarAndRsk <- paste("Var=", dis_ply$input_var, ", DR=", dis_ply$disp_rsk)

dis_ply$NOffAndVar <- paste("NoOff =", dis_ply$max_no_off, "Var =", dis_ply$input_var)

dis_ply$VarWords <- paste("Envirn Variation = " , dis_ply$input_var)

dis_ply$OffWords <- as.factor(paste("Max Offspring = " , dis_ply$max_no_off))

dis_ply$OffWords <- factor(dis_ply$OffWords, levels = c("Max Offspring =  4", "Max Offspring =  6" , "Max Offspring =  8" , "Max Offspring =  10" ))

levels(dis_ply$OffWords)


myFacet <- facet_grid(OffWords~VarWords)
legendLabel <- "Min adult size to disperse:"
xlabel <- "Competition (0 = full scramble, 1 = full contest)"

########### Graphs ###############

mytheme <- theme_bw(base_size=30)  + theme(plot.title = element_text(vjust=2), panel.margin= unit(0.75, "lines"),
		plot.margin=unit(c(1,1,1.5,1.2),"cm"), panel.border = element_rect(fill = NA, colour = "black", linetype=1, size = 1), legend.position="bottom", 
		legend.background = element_rect(fill="white", size=1, linetype="solid", colour ="black"))


dis_ply$PopAge.SE <- ifelse(dis_ply$PopAge.SE == 0, NA, dis_ply$PopAge.SE)  # making all zero se's into NA's

# Population Age
p1 <- ggplot(dis_ply, aes(x = Comp_meas, y = PopAge.Mean, colour = as.factor(ad_dsp_fd))) + geom_point(size = 5, position = position_jitter(w = 0.03, h = 0.0)) + 
		myFacet  +  ggtitle("Max Metapopulation Age") + geom_line() + mytheme + ylab("Age") + scale_colour_discrete(legendLabel) + xlab(xlabel)

		#geom_errorbar(aes(ymin=PopAge.Mean-PopAge.SE, ymax=PopAge.Mean + PopAge.SE), width = 0.1) + # + scale_colour_manual(values=c("blue", "red"))
 
### Not sure whether better to graph all generations??? Prob not. 
# Average colony age at death for all colonies

p2<- ggplot(dis_ply, aes(x = Comp_meas, y = survival_all.mean, colour = as.factor(ad_dsp_fd))) + geom_point(size = 5, position = position_jitter(w = 0.03, h = 0.0)) + 
		myFacet  +  ggtitle("Average colony survival") + geom_line() + mytheme + ylim(0, 500)+ ylab("Survival") + scale_colour_discrete(legendLabel) + xlab(xlabel)
		#geom_errorbar(aes(ymin=survival_all.mean-survival_all.SE, ymax=survival_all.mean + survival_all.SE), width = 0.1) + 
		 #+ scale_colour_manual(values=c("blue", "red"))

p2a <- ggplot(dis_ply, aes(x = Comp_meas, y = survival_all.mean, colour = as.factor(ad_dsp_fd))) + geom_point(size = 5, position = position_jitter(w = 0.03, h = 0.0)) + 
		myFacet  +  ggtitle("Average colony Survival compressed y axis") + geom_line() + mytheme + ylim(0, 75) + ylab("Survival") + 
		scale_colour_discrete(legendLabel) + xlab(xlabel)


# Colony size at dispersal

p3 <- ggplot(dis_ply, aes(x = Comp_meas, y = col_size_disp.mean, colour = as.factor(ad_dsp_fd))) + geom_point(size = 5, position = position_jitter(w = 0.03, h = 0.0)) + 
		myFacet +  ggtitle("Log 10 Average size of dispersing colonies") + geom_line() + mytheme + ylab("Population size (Log number of adults)") + 
		scale_colour_discrete(legendLabel) + xlab(xlabel)
		#geom_errorbar(aes(ymin=col_size_disp.mean-col_size_disp.SE, ymax=col_size_disp.mean + col_size_disp.SE), width = 0.1)  + scale_colour_manual(values=c("blue", "red"))

# binary of whether any colonies die
p4 <- ggplot(dis_ply, aes(x = Comp_meas, y = colDeath_bin_all.mean, colour = as.factor(ad_dsp_fd))) + geom_point(size = 5, position = position_jitter(w = 0.03, h = 0.0)) + 
		myFacet +  ggtitle("Did any colonies die? ( 0 = no, 1 = yes)") + geom_line() + ylim(0, 1)  + 
		scale_colour_discrete(legendLabel) + xlab(xlabel) + 
		mytheme # + scale_colour_manual(values=c("blue", "red"))

# binary of whether any colonies died
p4a <- ggplot(dis_ply, aes(x = Comp_meas, y = anyDisp.mean, colour = as.factor(ad_dsp_fd))) + geom_point(size = 5, position = position_jitter(w = 0.03, h = 0.0)) + 
		myFacet +  ggtitle("Did any colonies disperse? ( 0 = no, 1 = yes)") + geom_line() + ylim(0, 1) + 
		mytheme # + scale_colour_manual(values=c("blue", "red"))

# average colony size
p5 <- ggplot(dis_ply, aes(x = Comp_meas, y = colSizeB4Disp.mean, colour = as.factor(ad_dsp_fd))) + geom_point(size = 5, position = position_jitter(w = 0.03, h = 0.0)) + 
		myFacet +  ggtitle("Log 10 Average colony size (measured before dispersal)") + geom_line() + mytheme  + ylab("Log 10 Number of adults") + 
		scale_colour_discrete(legendLabel) + xlab(xlabel) 
		#geom_errorbar(aes(ymin=colSizeB4Disp.mean-colSizeB4Disp.SE, ymax=colSizeB4Disp.mean + colSizeB4Disp.SE), width = 0.1) # + scale_colour_manual(values=c("blue", "red"))




# percentage of individuals dispersal

p7 <- ggplot(dis_ply, aes(x = Comp_meas, y = pcntDisp.mean, colour = as.factor(ad_dsp_fd))) + geom_point(size = 5, position = position_jitter(w = 0.03, h = 0.0)) + 
		myFacet +  ggtitle("Average percentage of adults dispersing from colony") + geom_line() + ylab("Percentage of adults that disperse from a population") + 
		scale_colour_discrete(legendLabel) + xlab(xlabel) + 
		mytheme# + geom_errorbar(aes(ymin=pcntDisp.mean-pcntDisp.SE, ymax=pcntDisp.mean + pcntDisp.SE), width = 0.1) #+ scale_colour_manual(values=c("blue", "red"))

# colony size at death

p8 <- ggplot(dis_ply, aes(x = Comp_meas, y = colSizeDeath_all.mean, colour = as.factor(ad_dsp_fd))) + geom_point(size = 5, position = position_jitter(w = 0.03, h = 0.0)) + 
		myFacet +  ggtitle("Log 10 Average size of colony when it dies") + geom_line() + ylab("Log 10 Number of adults") + 
		scale_colour_discrete(legendLabel) + xlab(xlabel) + mytheme
		 #+ geom_errorbar(aes(ymin=colSizeDeath.mean-colSizeDeath.SE, ymax=colSizeDeath.mean + colSizeDeath.SE), width = 0.1) # + scale_colour_manual(values=c("blue", "red")) 



# prop of single nests that grow
p9 <- ggplot(dis_ply, aes(x = Comp_meas, y = propSigNestsGrow.mean, colour = as.factor(ad_dsp_fd))) + geom_point(size = 5, position = position_jitter(w = 0.03, h = 0.0)) + 
		myFacet +  ggtitle("Prop of single nests that grow") + geom_line() + mytheme + ylim(0, 1)


#Ave num cols in metapopulatiobn
p10 <- ggplot(dis_ply, aes(x = Comp_meas, y = MeanPopSize, colour = as.factor(ad_dsp_fd))) + geom_point(size = 5, position = position_jitter(w = 0.03, h = 0.0)) + 
		myFacet +  ggtitle("Average number of populations in the metapopulation at each time step") + geom_line() + mytheme


# Average frequence of disperal per nest
p11 <- ggplot(dis_ply, aes(x = Comp_meas, y = MeanDispFreq, colour = as.factor(ad_dsp_fd))) + geom_point(size = 5, position = position_jitter(w = 0.03, h = 0.0)) + 
		myFacet +  ggtitle("Average disperal frequency per population (colony) - wrong! Cal in summary table code wrong on cluster") + geom_line() + mytheme

# Average percentage of populations that disperse
p12 <- ggplot(dis_ply, aes(x = Comp_meas, y = MeanPerColsDisp, colour = as.factor(ad_dsp_fd))) + geom_point(size = 5, position = position_jitter(w = 0.03, h = 0.0)) + 
		myFacet +  ggtitle("Average percentage of colonies that disperse") + geom_line() + mytheme


#Perc cols die without dispersing
p13 <- ggplot(dis_ply, aes(x = Comp_meas, y = MeanPerDieNoDsp, colour = as.factor(ad_dsp_fd))) + geom_point(size = 5, position = position_jitter(w = 0.03, h = 0.0)) + 
		myFacet +  ggtitle("Average percentage of colonies that die without dispersing of total num cols die") + geom_line() + mytheme + scale_colour_discrete(legendLabel) + 
		xlab(xlabel) + ylab("Percentage")

#Overall varience in population size
p14 <- ggplot(dis_ply, aes(x = Comp_meas, y = MeanPopSizeVar, colour = as.factor(ad_dsp_fd))) + geom_point(size = 5, position = position_jitter(w = 0.03, h = 0.0)) + 
		myFacet +  ggtitle("Average variance in population sizes within the metapopulation") + geom_line() + mytheme

#Overall varience in population size - sig nests removed
p14a <- ggplot(dis_ply, aes(x = Comp_meas, y = MeanPopSizeVarNoSig, colour = as.factor(ad_dsp_fd))) + geom_point(size = 5, position = position_jitter(w = 0.03, h = 0.0)) + 
		myFacet +  ggtitle("Average variance in population sizes within the metapopulation - no single nests") + geom_line() + mytheme

#Overall varience in population size - sig nests removed
p14b <- ggplot(dis_ply, aes(x = Comp_meas, y = MeanPopSizeVarNoSig, colour = as.factor(ad_dsp_fd))) + geom_point(size = 5, position = position_jitter(w = 0.03, h = 0.0)) + 
		myFacet +  ggtitle("Average variance in population sizes within the metapopulation - no single nests, compressed axis") + geom_line() + mytheme + ylim(0,0.6)



#grid.arrange(p1, p2, p2a,  p3, p4, p4a, p5, p7, p8, p9, p10, p11, p13, p14, p14a, p14b,  ncol=1)

#dev.off()

