# TODO: Add comment
# 
# Author: Ruth
###############################################################################

TalkDatesToProcess <- "24/01/2017"


library(ggplot2)
library(rmarkdown)

readInput <- function()
{ 
	n <- readline(prompt="Warning: Above file already exists. Do you want to overwrite? y or n")
	return(as.character(n))
}


mytheme <- theme_bw() + 
		theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line=element_line(), 
				panel.border = element_blank(), legend.position="none")



source("C:/Users/Ruth/git/ruthubc/Biol345/RunCommunityFeedback.R", echo=FALSE, encoding="Cp1252")
source("C:/Users/Ruth/git/ruthubc/Biol345/RunIgniteFeedback.R", echo=FALSE, encoding="Cp1252")



