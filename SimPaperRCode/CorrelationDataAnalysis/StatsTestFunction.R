# TODO: Add comment
# 
# Author: user
###############################################################################


statsFun<- function(lin_mod, printAnova){

	anv<-anova(lin_mod)	
	if (printAnova =="y"){
			print (anv)
		print (summary(anv))}
	anv_df <- as.data.frame(anv)
	min <- rownames(anv_df)[which(anv_df$"Sum Sq"==min(anv_df$"Sum Sq"))] ##gives minimum value
	print (min)	
	
}
