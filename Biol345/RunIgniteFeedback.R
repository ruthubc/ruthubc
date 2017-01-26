# TODO: Add comment
# 
# Author: Ruth
###############################################################################


data_table <- read.csv("IgniteFeedback.csv")

data_table <- subset(data_table, Date == TalkDatesToProcess)

numberTalks <- nrow(data_table)


for (i in 1:numberTalks){
	
	
	row_num <- i
	
	ylabel = "Number of Students"
	
	student <- as.character(data_table$Name[row_num])	
	talk_date <-  as.Date(data_table$Date[row_num], "%d/%m/%Y")
	title_date <- as.character(format.Date(talk_date, format = "%d %B"))
	title <-  as.character(data_table$Title[row_num])
	mark <- as.character(data_table$Mark[row_num])
	
	file_student_name <- gsub(" ", "", student, fixed = TRUE)
	file_title_date <- as.character(format.Date(talk_date, format = "%m-%d"))
	
	fileOutputName<- paste("Ignite", file_title_date, file_student_name, ".docx",  sep = "")
	
	fileOutputPath <- paste("C:\\Users\\Ruth\\Dropbox\\RuthSync\\Biol345_2017\\TalkFeedback\\", fileOutputName,  sep = "")
	
	print(fileOutputPath)
	
	
	
	if (file.exists(fileOutputPath) == "TRUE" ) {
		
		answer <- readInput()
		
	} else { answer <- "y"}
	
	
	if (answer == "y") {
		
		
		
		rmarkdown::render(input= "C:\\Users\\Ruth\\git\\ruthubc\\Biol345\\IgniteFeedbackDoc.Rmd", 
				output_format = "word_document", 
				output_file= fileOutputPath, encoding= "UTF-8", clean = TRUE, quiet = TRUE)
		
		print("File Output")
		
	} else {
		
		print("file not overwritten")
		
	}
	
	
}

