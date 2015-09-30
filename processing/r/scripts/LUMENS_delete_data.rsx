##[LUMENS]=group

library(tcltk)

#READ LUMENS LOG FILE
LUMENS_log <- as.data.frame(Sys.info())
OS <- substr(as.character(LUMENS_log[2,1]), 1, 2)
username <- as.character(LUMENS_log[6,1])
if(OS == "XP") {
  user_path<-paste("C:/Documents and Settings/All Users", sep="")
} else {
  user_path<-paste("C:/Users/Public", sep="")
}
LUMENS_path_user <- paste(user_path,"/LUMENS/LUMENS.log", sep="")
log.file<-read.table(LUMENS_path_user, header=FALSE, sep=",")
proj.file<-paste(log.file[1,1], "/", log.file[1,2],"/",log.file[1,2], ".lpj", sep="")

#CREATE AND LOAD NEW R ENVIRONMENT
tempEnv<-new.env()
load(proj.file, tempEnv)

#PREPARE DF OF LUMENS DATABASE
listOfData<-as.data.frame(ls(tempEnv))
listOfData$delete<-0
colnames(listOfData)[1]="Data"
listOfData$Data<-as.character(listOfData$Data)

#SELECT DATA THAT YOU WANT TO DELETE
repeat{
  listOfData<-edit(listOfData)
  listOfDataDel<-listOfData[which(listOfData$delete==1),]
  n<-nrow(listOfDataDel)
  if(n==0){
    msgBox <- tkmessageBox(title = "Database",
                           message = "Choose at least one data to delete. Retry?",
                           icon = "question", 
                           type = "retrycancel", default="retry")
    if(as.character(msgBox)=="cancel"){
      quit()
    }
  } else {
    break
  }
}

#DELETE DATA FROM R ENVIRONMENT
for(i in 1:n){
  eval(parse(text=(paste("remove(", listOfDataDel[i,1], ", envir=tempEnv)", sep="")))) 
}

save(list=ls(tempEnv), file=proj.file, envir=tempEnv)