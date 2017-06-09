# TODO: Add comment
# 
# Author: Ruth
###############################################################################

mytheme2 <-theme_bw(base_size=3.5)  + theme(plot.title = element_text(vjust=2, size = 4.5), 
		panel.border = element_rect(fill = NA, colour = "grey", linetype=1, size = 0.5), 
		panel.grid.major = element_blank(), panel.grid.minor = element_blank())


PostWEX <- read.csv("Aquarium/WexData/PostWexSurveyData2017.csv")

fillList <-  c("yellow", "pink", "orange", "green", "blue")

xlabelText  <- "Score (1 = completely disagree; 5 = totally agree)"

#Question 1
Q1Plot <- ggplot(data = PostWEX) + geom_histogram(aes(Q1EnvConn, ..density..), binwidth = 1, colour = "black", fill = fillList) + 
		xlim(0.5,5.5) + mytheme2 + xlab(xlabelText) + scale_y_continuous(labels = scales::percent, breaks=seq(0, 1, 0.2)) + ylab("") + 
		ggtitle("Statement: I care more about the ocean\nand environmental conservation")


#I have a greater understanding of how my actions affect the natural world 
Q2Plot <- ggplot(aes(x = Q2EnvConn, ..density..), data = PostWEX) + geom_histogram(binwidth = 1, colour = "black", fill = fillList) + 
		xlim(0.5,5.5) + mytheme2 + xlab(xlabelText) + ylab("") + scale_y_continuous(labels = scales::percent, breaks=seq(0, 1, 0.2)) + 
		ggtitle("Statement: I have a greater understanding of\nhow my actions affect the natural world")

#My involvement with the aquarium has increased my interest in science

Q3Plot <- ggplot(aes(x = Q3STEM, ..density..), data = PostWEX) + geom_histogram(binwidth = 1, colour = "black", fill = fillList) + 
		xlim(0.5,5.5) + mytheme2 + xlab(xlabelText) + scale_y_continuous(labels = scales::percent, breaks=seq(0, 1, 0.2)) + ylab("") + 
		ggtitle("Statement: My involvement with the aquarium\nhas increased my interest in science")

#Volunteering at the aquarium has increased by desire to pursue a STEM (science, technology, engineering or math) career and/or continue studying STEM after high school.
Q4Plot <- ggplot(aes(x = Q4STEM, ..density..), data = PostWEX) + geom_histogram(binwidth = 1, colour = "black", fill = fillList) + 
		xlim(0.5,5.5) + mytheme2 + xlab(xlabelText) + scale_y_continuous(labels = scales::percent, breaks=seq(0, 1, 0.2)) + ylab("") + 
		ggtitle("Statement: My desire to pursue a STEM career\nand/or continue studying STEM after high\nschool has increased")

#I feel that I am better at communicating with other people and working in a team
Q5Plot <- ggplot(aes(x = Q5Skills, ..density..), data = PostWEX) + geom_histogram(binwidth = 1, colour = "black", fill = fillList) + 
		xlim(0.5,5.5) + mytheme2 + xlab(xlabelText) + scale_y_continuous(labels = scales::percent, breaks=seq(0, 1, 0.2)) + ylab("")+ 
		ggtitle("Statement: I feel I am better at communicating\nwith other people and working in a team")

# My planning, organizational and leadership skills have improved
Q6Plot <- ggplot(aes(x = Q6Skills, ..density..), data = PostWEX) + geom_histogram(binwidth = 1, colour = "black", fill = fillList) + 
		xlim(0.5,5.5) + mytheme2 + xlab(xlabelText) + scale_y_continuous(labels = scales::percent, breaks=seq(0, 1, 0.2)) + ylab("")+ 
		ggtitle("Statement: My planning, organizational\nand leadership skills have improved")

#I feel more confident about my future career or job prospects now
Q7Plot <- ggplot(aes(x = Q7Skills, ..density..), data = PostWEX) + geom_histogram(binwidth = 1, colour = "black", fill = fillList) + 
		xlim(0.5,5.5) + mytheme2 + xlab(xlabelText) + scale_y_continuous(labels = scales::percent, breaks=seq(0, 1, 0.2)) + ylab("")+ 
		ggtitle("Statement: I feel more confident about my\nfuture career or job prospects now")


png("Aquarium/WexData/PostWex_Environmental_Connectivity.png", width = 600, height = 1000, res = 400, pointsize = 12)

multiplot(Q1Plot, Q2Plot, cols=1)

dev.off()

png("Aquarium/WexData/PostWex_STEM_Engagement.png", width = 600, height = 1000, res = 400, pointsize = 12)

multiplot(Q3Plot, Q4Plot, cols=1)

dev.off()

png("Aquarium/WexData/PostWex_Skills_gained.png", width = 1200, height = 1000, res = 400, pointsize = 12)

multiplot(Q5Plot, Q6Plot, Q7Plot, cols=2)

dev.off()
