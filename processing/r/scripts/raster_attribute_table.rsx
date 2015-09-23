##[QUES]=group
##raster_file=raster
##passfilenames

r<-raster(raster_file)
area<-as.data.frame(freq(r))
edit(area)

