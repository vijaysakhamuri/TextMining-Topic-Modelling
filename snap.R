
library(data.table)

setwd("C:/Users/ViJaY PaVan/Documents/data sets")

list.files()

snap<-read.table('snap_sample1.csv',skipNul = T,fill = T,sep = ",",header = T)
 
str(snap)
head(snap)
snap[3,c(1,2)]
write.csv(snap,file ="C:/Users/ViJaY PaVan/Documents/data sets/sample7.csv")

################################################################
################################################################
setwd("C:/Users/ViJaY PaVan/Documents/data sets/snaps")
dir3<- "C:/Users/ViJaY PaVan/Documents/data sets/snaps"

file.names<-dir(dir3)

datalist=list()
dat2<-data.frame()
for(i in seq(from=1, to=length(file.names), by=1))
{
  snap1<-read.table(file.names[i],skipNul = T,fill = T,sep = ",",header = T)
  snap_write<-snap1[76,c(1,2)]
  snap_write1<-snap1[77,c(1,2)]
  snap_write2<-snap1[83,c(1,2)]
  snap_write3<-snap1[84,c(1,2)]
  snap_w<-data.frame(snap_write,snap_write1,snap_write2,snap_write3)
  datalist[[i]]<-snap_w
  
}
big1=do.call(rbind,datalist)

write.csv(big1,file ="C:/Users/ViJaY PaVan/Documents/data sets/sample7.csv")
