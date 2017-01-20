# TODO: Add comment
# 
# Author: Ruth
###############################################################################

TalkDatesToProcess <- "17/01/2017"


library(ggplot2)
library(rmarkdown)

mytheme <- theme_bw() + 
		theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line=element_line(), 
				panel.border = element_blank(), legend.position="none")

setwd("C:\\Users\\Ruth\\Dropbox\\Biol345_2017\\TalkFeedback")

data_table <- read.csv("C:/Users/Ruth/Dropbox/RuthSync/Biol345_2017/TalkFeedback/CommunityFeedback.csv")

data_table <- subset(data_table, Date == TalkDatesToProcess)

numberTalks <- nrow(data_table)


for (i in 1:numberTalks){


	row_num <- i

	ylabel = "Number of Students"

	student <- as.character(data_table$Name[row_num])	
	talk_date <-  as.Date(data_table$Date[row_num], "%d/%m/%Y")
	title_date <- as.character(format.Date(talk_date, format = "%d %b"))
	title <-  as.character(data_table$Title[row_num])
	mark <- as.character(data_table$Mark[row_num])
	
	file_student_name <- gsub(" ", "", student, fixed = TRUE)
	file_title_date <- as.character(format.Date(talk_date, format = "%m-%d"))

	fileOutputName<- paste("Community", file_title_date, file_student_name, ".docx",  sep = "")

	fileOutputPath <- paste("C:\\Users\\Ruth\\Dropbox\\RuthSync\\Biol345_2017\\TalkFeedback\\", fileOutputName,  sep = "")

	print(fileOutputPath)


	rmarkdown::render(input= "C:\\Users\\Ruth\\EclipseWorkspace\\Biol345\\CommunityFeedbackDoc.Rmd", 
			output_format = "word_document", 
			output_file= fileOutputPath, encoding= "UTF-8", clean = TRUE, quiet = TRUE)

}