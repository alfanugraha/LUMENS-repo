##[LUMENS]=group
LUMENS_log <- as.data.frame(Sys.info())
OS <- substr(as.character(LUMENS_log[2,1]), 1, 2)
username <- as.character(LUMENS_log[6,1])
if(OS == "XP") {
  user_path<-paste("C:/Documents and Settings/", username, sep="")
} else {
  user_path<-paste("C:/Users/", username, sep="")
}
LUMENS_path_user <- paste(user_path,"/LUMENS", sep="")
sink(paste(LUMENS_path_user, "/LUMENS.log", sep=""))
cat()
sink()
