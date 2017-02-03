
# Author: Ruth
###############################################################################

TalkDatesToProcess <- "31/01/2017"


library(ggplot2)
library(rmarkdown)

readInput <- function()
{ 
	n <- readline(prompt="Warning: Above file already exists. Do you want to overwrite? y or n")
	return(as.character(n))
}

#axis.line=element_line(), 
mytheme <- theme_bw() + 
		theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
				panel.border = element_blank(), legend.position="none") +  theme(axis.line.x = element_line(color="black"),
        		axis.line.y = element_line(color="black"))

comp_name <- Sys.info()["nodename"]

## HOME COMPUTER
if (comp_name == "DELL-1545") {
	print("You are using your home computer")
	pathRMD <- "C:\\Work\\EclipseNeonWorkspace\\" # home
	pathOutput <- "C:\\Work\\Dropbox\\RuthSync\\Biol345_2017\\TalkFeedback\\" # home
	
	#community
	source("C:/Work/EclipseNeonWorkspace/ruthubc/Biol345/RunCommunityFeedback.R", echo=FALSE, encoding="Cp1252") # home
	
	#ignite
	source("C:/Work/EclipseNeonWorkspace/ruthubc/Biol345/RunIgniteFeedback.R", echo=FALSE, encoding="Cp1252") # home

#WORK COMPUTER	
}else{
	print("You are using your school computer")
	pathRMD <- "C:\\Users\\Ruth\\git\\" # work
	pathOutput <- "C:\\Users\\Ruth\\Dropbox\\RuthSync\\Biol345_2017\\TalkFeedback\\"
	source("C:/Users/Ruth/git/ruthubc/Biol345/RunCommunityFeedback.R", echo=FALSE, encoding="Cp1252") # work
	source("C:/Users/Ruth/git/ruthubc/Biol345/RunIgniteFeedback.R", echo=FALSE, encoding="Cp1252") # work

	
}
