##[LUMENS]=group
##working_directory=folder
##project=string (enter name of the project)
##location=string (enter location)
##province=string (enter province name of your location)
##country=string (enter country name)
##description=string
##data=raster
##admin_attribute=vector
##field_attribute=field admin_attribute

time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

library(rgdal)
library(tiff)
library(spatial.tools)
library(RColorBrewer)
library(grid)
library(plyr)
library(ggplot2)
library(rtf)
library(rasterVis)


#build LUMENS project folder structure
setwd(working_directory)
LUMENS_path <- paste(working_directory, "/", project, sep="")
PUR_path <- paste(LUMENS_path, "/PUR", sep="")
QUES_path <- paste(LUMENS_path, "/QUES", sep="")
PreQUES_path <- paste(QUES_path, "/PreQUES", sep="")
QUESC_path <- paste(QUES_path, "/QUES-C", sep="")
QUESB_path <- paste(QUES_path, "/QUES-B", sep="")
QUESH_path <- paste(QUES_path, "/QUES-H", sep="")
TA_path <- paste(LUMENS_path, "/TA", sep="")
SCIENDO_path  <- paste(LUMENS_path, "/SCIENDO", sep="")

dir.create(LUMENS_path, mode="0777")
dir.create(PUR_path, mode="0777")
dir.create(QUES_path, mode="0777")
dir.create(PreQUES_path, mode="0777")
dir.create(QUESC_path, mode="0777")
dir.create(QUESB_path, mode="0777")
dir.create(QUESH_path, mode="0777")
dir.create(TA_path, mode="0777")
dir.create(SCIENDO_path, mode="0777")

#create LUMENS.log
LUMENS_log <- as.data.frame(Sys.info())
OS <- substr(as.character(LUMENS_log[2,1]), 1, 2)
username <- as.character(LUMENS_log[6,1])
if(OS == "XP") {
user_path<-paste("C:/Documents and Settings/", username, sep="")
} else {
user_path<-paste("C:/Users/", username, sep="")
}
LUMENS_path_user <- paste(user_path,"/LUMENS", sep="")
dir.create(LUMENS_path_user, mode="0777")
sink(paste(LUMENS_path_user, "/LUMENS.log", sep=""))
cat(working_directory, project, time_start, sep=",")
sink()

#WRITE PROJECT PROPERTIES
#project_name<-paste(project,".lpj", sep="")
db_name<-paste(project, ".lpj", sep="")
landuse.index=0
pu.index=0
pu_rec.index=0
lut_carbon.index=0
lut_landuse.index=0
lut_zone.index=0
period.index=0
PUR.index=0
PreQUES.index=0
QUESC.index=0
QUESB.index=0
QUESH.index=0
SCIENDO1.index=0
SCIENDO2.index=0
TA1.index=0
TA2.index=0
ref.index=1
admin.index=1

#CREATE RESAVE FUNCTION
resave <- function(..., list = character(), file) {
  previous  <- load(file)
  var.names <- c(list, as.character(substitute(list(...)))[-1L])
  for (var in var.names) assign(var, get(var, envir = parent.frame()))
  save(list = unique(c(previous, var.names)), file = file)
}

#CREATE PROJECT DESCRIPTION TABLE
proj_descr <- as.data.frame(rbind(project, description, working_directory, location, province, country))

# #CREATE COVERAGE REFERENCE FOR PROJECT
# ref<-raster(data)
# ref<-ref*1
# Ref.name<-names(ref)
# Ref.type<-class(ref)
# Ref.source<-data
# Ref.coord<-as.character(crs(ref))
# Ref.res<-res(ref)
# Ref.xmin<-xmin(ref)
# Ref.xmax<-xmax(ref)
# Ref.ymin<-ymin(ref)
# Ref.ymax<-ymax(ref)
# cov.desc1<-c("Reference name","Reference class", "Reference source", "Reference CRS", "Reference Resolution", "Xmin", "Xmax", "Ymin", "Ymax")
# cov.desc2<-as.data.frame(rbind(Ref.name, Ref.type, Ref.source, Ref.coord, Ref.res, Ref.xmin, Ref.xmax, Ref.ymin, Ref.ymax))
# cov.desc2<-cov.desc2[1]
# cov.desc<-cbind(cov.desc1,cov.desc2)
# colnames(cov.desc)[1]<-"Coverage"
# colnames(cov.desc)[2]<-"Description"


#CREATE COVERAGE REFERENCE FOR PROJECT
ref<-data
ref<-ref*1
Ref.name<-names(ref)
Ref.type<-class(ref)
Ref.coord<-as.character(crs(ref))
Ref.res<-res(ref)
Ref.xmin<-xmin(ref)
Ref.xmax<-xmax(ref)
Ref.ymin<-ymin(ref)
Ref.ymax<-ymax(ref)
cov.desc1<-c("Reference name","Reference class", "Reference CRS", "Reference Resolution", "Xmin", "Xmax", "Ymin", "Ymax")
cov.desc2<-as.data.frame(rbind(Ref.name, Ref.type, Ref.coord, Ref.res, Ref.xmin, Ref.xmax, Ref.ymin, Ref.ymax))
cov.desc2<-cov.desc2[1]
cov.desc<-cbind(cov.desc1,cov.desc2)
colnames(cov.desc)[1]<-"Coverage"
colnames(cov.desc)[2]<-"Description"

#ATTRIBUTE OF ADMIN
test1<-as.data.frame(admin_attribute)
test2<-as.character(field_attribute)
eval(parse(text=(paste("p.admin.df<-aggregate(IDADM~",test2,",data=test1,FUN=mean)", sep=""  ))))
p.admin.df<-edit(p.admin.df)

palette <- brewer.pal("Greys", n=9)
color.background = palette[2]

plot3<-gplot(ref, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
coord_equal() + theme(plot.title = element_text(lineheight= 5, face="bold")) +
theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
legend.title = element_text(size=10),
legend.text = element_text(size=10),
legend.key.height = unit(0.35, "cm"),
legend.key.width = unit(0.35, "cm"))+ theme(panel.background=element_rect(fill=color.background, color=color.background))

setwd(LUMENS_path)

test<-c(rownames(proj_descr))
proj_descr<-cbind(test, proj_descr)
colnames(proj_descr)[1]<-"Type"
colnames(proj_descr)[2]<-"Description"
proj_descr<-as.data.frame(proj_descr)
proj.file<-paste(LUMENS_path, "/",project,".lpj", sep="")

pu_pu1<-ref
names(pu_pu1)<-"Administrative maps"

save(LUMENS_path_user,
landuse.index,
proj_descr,
ref,
location,
province,
country,
p.admin.df,
ref.index,
admin.index,
cov.desc,
pu.index,
pu_pu1,
pu_rec.index,
lut_carbon.index,
lut_landuse.index,
lut_zone.index,
period.index,
PUR.index,
PreQUES.index,
QUESC.index,
QUESB.index,
QUESH.index,
SCIENDO1.index,
SCIENDO2.index,
TA1.index,
TA2.index,
resave,
file=proj.file)

#WRITE REPORT
title1<-"{\\colortbl;\\red0\\green0\\blue0;\\red255\\green0\\blue0;\\red146\\green208\\blue80;\\red0\\green176\\blue240;\\red140\\green175\\blue71;\\red0\\green112\\blue192;\\red79\\green98\\blue40;} \\pard\\qr\\b\\fs70\\cf2 L\\cf3U\\cf4M\\cf5E\\cf6N\\cf7S \\cf1REPORT \\par\\b0\\fs20\\ql\\cf1"
title2<-paste("\\pard\\qr\\b\\fs40\\cf1 Create LUMENS Project ", "for ", location, ", ", province, ", ", country, "\\par\\b0\\fs20\\ql\\cf1", sep="")
sub_title<-"\\cf2\\b\\fs32 Ringkasan Deskripsi Projek\\cf1\\b0\\fs20"
chapter1<-"\\cf2\\b\\fs28 Deskripsi Projek \\cf1\\b0\\fs20"
chapter2<-"\\cf2\\b\\fs28 Cakupan Geografis Projek \\cf1\\b0\\fs20"
chapter3<-"\\cf2\\b\\fs28 Data-data Acuan Dalam Projek \\cf1\\b0\\fs20"
#time_start<-paste("Proses LUMENS dimulai : ", time_start, sep="")
time_end<-paste("Proses LUMENS selesai : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
line<-paste("-------------------------------------------------------------------------------------------------------------------------------------------------------")
area_name_rep<-paste("\\b", "\\fs20", location, "\\b0","\\fs20")
rtffile <- RTF("LUMENS_Create-Project_report.lpr", font.size=9)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addParagraph(rtffile, title1)
addParagraph(rtffile, title2)
addNewLine(rtffile)
addParagraph(rtffile, line)
#addParagraph(rtffile, time_start)
addParagraph(rtffile, time_end)
addParagraph(rtffile, line)
addNewLine(rtffile)
width<-as.vector(c(1.34,3.1))
addTable(rtffile,proj_descr,font.size=8,col.widths=width)
addPageBreak(rtffile)
addParagraph(rtffile, sub_title)
addNewLine(rtffile)
addParagraph(rtffile, line)
#addParagraph(rtffile, time_start)
addParagraph(rtffile, time_end)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, paste("Selamat datang di LUMENS!!. Anda telah berhasil menyusun konfigurasi data-data awal yang akan digunakan dalam perencanaan penggunaan lahan yang mempertimbangkan berbagai fungsi lingkungan. LUMENS project file terdiri dari dua file utama dengan akhiran .lpj dan lpd. Project file yang telah anda buat bernama ", project, ".lpj."))
addNewLine(rtffile)
addParagraph(rtffile, chapter1)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Deskripsi projek menyimpan informasi umum yang anda masukkan mengenai projek ini")
addNewLine(rtffile)
width<-as.vector(c(1.34,3.1))
addTable(rtffile,proj_descr,font.size=8,col.widths=width)
addNewLine(rtffile)
addParagraph(rtffile, chapter2)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Cakupan geografis projek menyimpan informasi mengenai cakupan area yang akan digunakan di dalma project, batas-batas koordinat, sistem projeksi serta resolusi spasial yang akan digunakan dalam projek")
addNewLine(rtffile)
addTable(rtffile,cov.desc,font.size=8,col.widths=width)
addNewLine(rtffile)
addPageBreak(rtffile)
addParagraph(rtffile, chapter3)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Berikut ini adalah beberapa data yang akan dijadikan data acuan dalam projek ini")
addNewLine(rtffile)
#addParagraph(rtffile, paste("\\cf4\\b \\fs20 Peta Acuan Dalam Format Raster\\b \\fs20\\cf1", sep=" "))
#addPlot(rtffile,plot.fun=print, width=5,height=3.5,res=150,  plot3)
#addNewLine(rtffile)
addParagraph(rtffile, paste("\\cf4\\b \\fs20 Peta batas administrasi\\b \\fs20\\cf1", sep=" "))
addPlot(rtffile,plot.fun=print, width=6,height=4.5,res=150,  plot3)
addNewLine(rtffile)
done(rtffile)

command<-paste("start ", "winword ", LUMENS_path, "/LUMENS_Create-Project_report.lpr", sep="" )
shell(command)
#CLEAN ENVIRONMENT
rm(list=ls(all.names=TRUE))








