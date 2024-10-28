#======================Traitement des données=======================
#Libraries
library(DBI)
library(RSQLite)
library(hydroTSM)
library(reshape2)
#Création de base de données
setwd("D:/MODELISATION/Mouhoun/DATA/StationData")
MouhounDataBase <- dbConnect(RSQLite::SQLite(),"DataBase.sqlite")
#--------------Importation de données--------------------------------
Filesnames <- dir(pattern = "Data.txt")
NamesFiles <- sub(".txt","",Filesnames)
i <- length(Filesnames)
repeat{
  clim_data <- read.table(file = Filesnames[i],header = T,sep = "\t")
  if(i==length(Filesnames)){
    DATE=clim_data[,1]
  }
  if(i==1){
    clim_data <- data.frame(DATE,clim_data)
  }
  dbWriteTable(conn = MouhounDataBase,name =NamesFiles[i],value =clim_data)
  i <- i-1
  if(i==0)break
}
#-------------Lectures-------------------------
info_file <- read.table("D:/MODELISATION/Mouhoun/DATA/StationData/tmp.txt",header = T,sep = ",")
TablesListes <- dbListTables(conn =MouhounDataBase )
j <- 1
repeat{
  j <- j+1
  import_data <- dbReadTable(conn = MouhounDataBase,name = TablesListes[j])
  import_data <- transform(import_data,DATE=as.Date(paste(substr(x = import_data$DATE,start = 1,stop = 4),
                                                          substr(x = import_data$DATE,start = 5,stop = 6),
                                                          substr(x = import_data$DATE,start = 7,stop = 8),sep = "-")))
  AnnualData <- t(daily2annual.data.frame(x = import_data,FUN = sum,na.rm = T,out.fmt = "%Y-%m-%d",
                                          date.fmt = "%Y-%m-%d",dates = 1))
  Stations <- rownames(AnnualData)
  rownames(AnnualData) <- NULL
  AnnualData_2 <- data.frame(Stations=Stations,Long=info_file$LONG,Lat=info_file$LAT,
                             Elev=info_file$ELEVATION,AnnualData)
  AnnualData_Pcp <-melt(data = AnnualData_2,id.vars = c("Stations","Long","Lat","Elev")) 
  }
Pcp=AnnualData_Pcp[,-5]
sp_ClimData <- data.frame(AnnualData_Tmax[,c(1:4)],Tmax=round(AnnualData_Tmax[,6],2),Tmin=round(AnnualData_Tmin[,6],2))
save_path <- "D:/MODELISATION/Mouhoun/Spatialisation/Data/"
write.table(x = Pcp,file =paste0(save_path,"Precipitation.txt"),quote = F,sep = "\t",row.names = F)
