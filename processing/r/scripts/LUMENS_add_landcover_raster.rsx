##[LUMENS]=group
##data=raster
##Period=number 2010
##Description=string
##passfilenames

library(spatial.tools)

#READ LUMENS LOG FILE
LUMENS_log <- as.data.frame(Sys.info())
OS <- substr(as.character(LUMENS_log[2,1]), 1, 2)
username <- as.character(LUMENS_log[6,1])
if(OS == "XP") {
  user_path<-paste("C:/Documents and Settings/", username, sep="")
} else {
  user_path<-paste("C:/Users/", username, sep="")
}
LUMENS_path_user <- paste(user_path,"/LUMENS/LUMENS.log", sep="")
log.file<-read.table(LUMENS_path_user, header=FALSE, sep=",")
proj.file<-paste(log.file[1,1], "/", log.file[1,2],"/",log.file[1,2], ".lpj", sep="")
load(proj.file)
lumens_database<-proj.file

#IMPORTING DATA INTO LUMENS DATABASE
command="raster"
landuse.index<-landuse.index+1
period.index<-period.index+1
eval(parse(text=(paste("period", period.index, "<-Period", sep=""))))
eval(parse(text=paste("period_info<-period", period.index, sep="")))
data_name<-paste("Landuse",sep="")
eval(parse(text=(paste(data_name,"_", landuse.index, "<-", command,'("', data, '")', sep=""))))
eval(parse(text=(paste(data_name,"_", landuse.index, "<-spatial_sync_raster(",data_name,"_", landuse.index, ',', 'ref, method = "ngb")', sep=""))))
eval(parse(text=(paste(data_name,"_", landuse.index, "<-", data_name,"_", landuse.index, "*1",  sep=""))))
eval(parse(text=(paste("names(",data_name,"_", landuse.index, ")<-Description", sep=""))))
eval(parse(text=(paste("freq",data_name, "_", landuse.index, "<-as.data.frame(na.omit(freq(", data_name,"_", landuse.index, ")))",  sep=""))))
eval(parse(text=(paste("resave(", data_name,"_", landuse.index, ",landuse.index, file=lumens_database)", sep=""))))
eval(parse(text=(paste("resave(freq", data_name,"_", landuse.index, ", file=lumens_database)", sep=""))))
period_i<-paste("period", period.index, sep="")
eval(parse(text=(paste(period_i, "<-Period", sep="" ))))
eval(parse(text=(paste("resave(", period_i, ",period.index, file=lumens_database)", sep=""))))

