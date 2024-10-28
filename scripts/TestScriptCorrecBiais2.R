#===========Triatement des données climatiques===============
rm(list = ls())
#Libraries
library(readxl)
library(hyfo)
library(DBI)
library(RSQLite)
library(hydroTSM)
library(stringr)
library(stringi)
#Connection de la base de données
setwd("D:/MODELISATION/Mouhoun/DATA/TreatedData")
DataBase2 <- RSQLite::dbConnect(drv = RSQLite::SQLite(),"DataBase.sqlite")
FilesNames2 <- dbListTables(DataBase2)
#Importation des fichiers dans la base de données
files_path <- "D:/MODELISATION/Mouhoun/Merging/DATA/StationData/EstimateData"
FileName <- dir(path =files_path ,pattern = ".txt")
FileName_a <- sub(pattern = ".txt",replacement = "",x = FileName)
k <- 0
repeat{
  k <- k+1
  climateData <- read.table(file = paste(files_path,FileName[k],sep = "/"),header = T,sep = "\t")
  dbWriteTable(conn = DataBase2,name =FileName_a[k] ,value =climateData )
}
#------------------Calcul des valeurs annuelles-------------------
#Importation des données
DATE <- seq(as.Date("1981-01-01"),as.Date("2017-12-31"),by = "years")
ref_date <- seq(as.Date("1981-01-01"),as.Date("2017-06-30"),by = "days")
    #Données observées
db_path <- "C:/Users/User/Documents/Memoire/donnees/Donnees_pluvio/ObsData/"
ObserveDataBase <- RSQLite::dbConnect(drv = RSQLite::SQLite(),paste0(db_path,"DataBase.sqlite"))
ObserveRainFall <- dbReadTable(conn =ObserveDataBase ,name ="Precipitation_Obs" )
    #Données estimées
EstimateRainFallData <- dbReadTable(conn =DataBase2 ,name ="EstimateRainFall" )

# Transformation des données journalières 
    #Calcul de valeurs annuelles des données observées
ObserveRainFall <- transform(ObserveRainFall,DATE=as.Date(paste(ObserveRainFall$Annees,
                                                                ObserveRainFall$Mois,
                                                                ObserveRainFall$Jours,sep = "-")))
ObserveRainFall_1 <- ObserveRainFall[,c(14,4:13)]
ObserveRainFall_1 <- ObserveRainFall_1[ObserveRainFall_1$DATE%in%ref_date,]
AnnualObserveRainFall <- data.frame(DATE,daily2annual.data.frame(x = ObserveRainFall_1,FUN = sum,na.rm = T,dates = 1,))

    #Calcul de valeurs annuelles des données estimées
EstimateRainFallData <- transform(EstimateRainFallData,DATE=as.Date(paste(substr(x = EstimateRainFallData$DATE,start = 1,stop = 4),
                                                          substr(x = EstimateRainFallData$DATE,start = 5,stop = 6),
                                                          substr(x = EstimateRainFallData$DATE,start = 7,stop = 8),sep = "-")))

EstimateRainFallData <- EstimateRainFallData[EstimateRainFallData$DATE%in%ref_date,]
AnnualEstimateRainFallData <- data.frame(DATE,daily2annual.data.frame(x = EstimateRainFallData,FUN = sum,na.rm = T,dates = 1,))

#----------------------Formatage CDT------------------------------------
StationsCoordinates <- read_xls("C:/Users/User/Documents/Memoire/donnees/Donnees_pluvio/ObsData/stationsynoptique.xls")
Lon <- c("LON",StationsCoordinates$lon)
Lat <- c("DAILY/LAT",StationsCoordinates$lat)
stationNames <-c("Station",colnames(AnnualEstimateRainFallData)[-1])
AnnualRainFall_CDT <- transform(AnnualEstimateRainFallData,DATE=str_replace_all(string = AnnualEstimateRainFallData$DATE,pattern = "-",replacement = ""))
colnames(AnnualRainFall_CDT) <-NA

AnnualRainFallCDT <- rbind(stationNames,Lon,Lat,AnnualRainFall_CDT)

##sauvegarde
save_path <- "D:/MODELISATION/Mouhoun/Merging/DATA/StationData/AnnualRawData/"
write.table(x =AnnualRainFallCDT,file = paste0(save_path,"AnnualEstimateRainFallCDT.txt"),sep = "\t",row.names = F,col.names = F,quote = F,na = "-99")

