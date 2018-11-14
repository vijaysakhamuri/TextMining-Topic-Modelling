##log file reading
library(data.table)

setwd("C:/Users/ViJaY PaVan/Documents/data sets")
df_log = read.table('logs.log',fill = TRUE , header = FALSE)
str(df_log)
names(df_log)
head(df_log)

df_logs <-data.frame(df_log)
str(df_logs)
head(df_logs)
df_logs1<-as.integer(df_logs$V2)
data1<-df_logs$V2 < '6:53:39:749'

write.csv(df_logs,file ="C:/Users/ViJaY PaVan/Documents/data sets/sample7.csv")





df_logs[V2,'6:53:39:749']
df_log[which(df_log$V2 > '6:50:58:628'& df_log$V2 < '6:52:31:208'),]
data_logs<-df_logs[as.numeric(df_logs$v2) > '6:50:58:628' & as.numeric(df_logs$v2) < '6:52:29:376']
    data_logs
    
    df <- data.frame(as.Date(df_log,"format="%d-%m-%Y")
class(df_log$V2)
df_log1<-as.character(df_log$V2)
str(df_log1)
rm(df_log1)
library(anytime)
anytime(as.factor(df_log$v2))
class(anytime(as.factor(df_log$v2)) )                            