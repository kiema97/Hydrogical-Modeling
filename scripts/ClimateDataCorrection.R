#===========Correction des données climatiques===============
rm(list = ls())
#Libraries
library(readxl)
library(hyfo)
library(DBI)
library(RSQLite)
library(hydroTSM)
library(raster)
library(rgdal)
library(stringr)
library(stringi)
#++++++++++++++++++++Importation des données+++++++++++++++++++++++
setwd("C:/Users/User/Documents/Memoire/donnees/Donnees_pluvio/ObsData")
#Connection de base de données
DataBase <- RSQLite::dbConnect(drv = RSQLite::SQLite(),"DataBase.sqlite")
#importation de données depuis la base de données 
FilesNames <- dbListTables(conn = DataBase)
#Definition des noms de champs pour le filtrage des données

field_names <- c("Annees","Mois","Jours", "Bobo" ,"Boromo","Dedougou",
                 "Gaoua","Ouagadougou","Ouahigouya")
#Lecture des données depuis la base de données 
rain_data <- dbReadTable(conn =DataBase,name = "Precipitation_Obs" )[,field_names]
tmax_data <- dbReadTable(conn =DataBase,name = "Tmax_Obs" )[,field_names]
tmin_data <- dbReadTable(conn =DataBase,name = "Tmin_Obs" )[,field_names]

ObsDATA <- data.frame(DATE=as.Date(paste(rain_data$Annees,rain_data$Mois,rain_data$Jours,sep = "-")),
                      ObsPCP=round(rowMeans(rain_data[,c(4:9)]),2),
                      ObsTmax=round(rowMeans(tmax_data[,c(4:9)]),2),
                      ObsTmin=round(rowMeans(tmin_data[,c(4:9)]),2))
#Importation des données estimées
Ref_Date <- dip("1981-01-01","2017-06-30")
EstimateData <- dbReadTable(conn = DataBase,name = "EstimateData")
EstimateData <- transform(EstimateData,DATE=as.Date(EstimateData$DATE))

## Correction de la précipitation
ObsDATA2 <- subset(ObsDATA,subset=DATE%in%Ref_Date)
salcing=biasCorrect(frc =EstimateData[,c(1,2)],
                    hindcast = EstimateData[,c(1,2)],
                    obs =ObsDATA2[,c(1,2)],method = "scaling",
                    preci = T,
                    prThreshold = 1)
eqm=biasCorrect(frc =EstimateData[,c(1,2)],
                    hindcast = EstimateData[,c(1,2)],
                    obs =ObsDATA2[,c(1,2)],method = "eqm",
                    preci = T,
                    prThreshold = 1)
gqm=biasCorrect(frc =EstimateData[,c(1,2)],
                    hindcast = EstimateData[,c(1,2)],
                    obs =ObsDATA2[,c(1,2)],method = "gqm",
                    preci = T,
                    prThreshold = 1,)

CorrectedPcpData <- data.frame(DATE=as.factor(salcing$DATE),
                               scaling=round(salcing$pcp,2),
                               eqm=round(eqm$pcp,2),
                               gqm=round(gqm$pcp,2))
#Correction des températures maximales
frc_tmaxdata <- EstimateData[,c(1,3)]
obs_tmax <- ObsDATA2[,c(1,3)]
tmax_scaling <- biasCorrect(frc = frc_tmaxdata,
                            hindcast = frc_tmaxdata,
                            obs = obs_tmax,method = "scaling",scaleType = "add")


tmax_eqm <- biasCorrect(frc = frc_tmaxdata,
                            hindcast = frc_tmaxdata,
                            obs = obs_tmax,method = "eqm")
CorrectedTmaxData <- data.frame(DATE=as.factor(tmax_scaling$DATE),
                                scaling=round(tmax_scaling$tmax,2),
                                eqm=round(tmax_eqm$tmax,2))
#Correction des températures minimales
frc_tmindata <- EstimateData[,c(1,4)]
obs_tmin <- ObsDATA2[,c(1,4)]

tmin_scaling <- biasCorrect(frc = frc_tmindata,
                            hindcast = frc_tmindata,
                            obs = obs_tmin,method = "scaling",scaleType = "add")


tmin_eqm <- biasCorrect(frc = frc_tmindata,
                        hindcast = frc_tmindata,
                        obs = obs_tmin,method = "eqm")
CorrectedTminData <- data.frame(DATE=as.factor(tmin_scaling$DATE),
                                scaling=round(tmin_scaling$tmin,2),
                                eqm=round(tmin_eqm$tmin,2))
ObsDATA2 <- transform(ObsDATA2,DATE=as.factor(ObsDATA2$DATE))
#EXportation des donénes
#setwd("D:/MODELISATION/Mouhoun/DATA/TreatedData")
#DataBase2 <- RSQLite::dbConnect(drv = RSQLite::SQLite(),"DataBase.sqlite")
#RSQLite::dbWriteTable(conn = DataBase2,name ="ObserveDATA" ,value =ObsDATA2)


#===============Evaluation des méthodes de correction de biais==============
#Connection de base de données
setwd("D:/MODELISATION/Mouhoun/DATA/TreatedData")
DataBase2 <- RSQLite::dbConnect(drv = RSQLite::SQLite(),"DataBase.sqlite")
FilesNames2 <- dbListTables(DataBase2)
#Importation des données depuis la base de données
observeData <- dbReadTable(conn =DataBase2 ,name ="ObserveDATA" )
CorrectedPcpData <-  dbReadTable(conn =DataBase2 ,name ="CorrectedPcpData" )
CorrectedTmaxData <- dbReadTable(conn =DataBase2 ,name ="CorrectedTmaxData" )
CorrectedTminData <- dbReadTable(conn =DataBase2 ,name ="CorrectedTminData" )
EstimateData <- dbReadTable(conn =DataBase2 ,name ="EstimateData" )

#Calcul des critères de performace
parameters <- rownames(gof(sim =EstimateData$pcp,obs=observeData$ObsPCP,na.rm = T))

Pcpcrit <- data.frame(parameters,brute=gof(sim =EstimateData$pcp,obs=observeData$ObsPCP,na.rm = T),
                      scaling=gof(sim =CorrectedPcpData$scaling,obs=observeData$ObsPCP,na.rm = T),
                      eqm=gof(sim =CorrectedPcpData$eqm,obs=observeData$ObsPCP,na.rm = T),
                      gqm=gof(sim =CorrectedPcpData$gqm,obs=observeData$ObsPCP,na.rm = T))


Tmaxcrit <- data.frame(parameters,brute=gof(sim =EstimateData$tmax,obs=observeData$ObsTmax,na.rm = T),
                      scaling=gof(sim =CorrectedTmaxData$scaling,obs=observeData$ObsTmax,na.rm = T),
                       eqm=gof(sim =CorrectedTmaxData$eqm,obs=observeData$ObsTmax,na.rm = T))


Tmincrit <- data.frame(parameters,brute=gof(sim =EstimateData$tmin,obs=observeData$ObsTmin,na.rm = T),
                       scaling=gof(sim =CorrectedTminData$scaling,obs=observeData$ObsTmin,na.rm = T),
                       eqm=gof(sim =CorrectedTminData$eqm,obs=observeData$ObsTmin,na.rm = T))

#Sauvegarde des résultats
dbWriteTable(conn =DataBase2 ,name ="Tmincrit" ,value =Tmincrit)
rr <- dbReadTable(conn =DataBase2 ,name = "Tmincrit")

save_path <- "D:/MODELISATION/Mouhoun/Output/data/ClimaticDataCorrection/"
write.table(x = Tmincrit,file = paste0(save_path,"Tmincrit.txt"),sep = "\t",quote = F,row.names = F)

#*********************Diagramme qauntiel-quantile************************
##Précipitation
ObsPCPDATA <- observeData[!is.na(observeData$ObsPCP),]
CorrectedPcpData_ <- CorrectedPcpData[!is.na(observeData$ObsPCP),]

CorrectedPcpData2 <- transform(CorrectedPcpData_,
                               scaling=sort(CorrectedPcpData_$scaling,decreasing = T),
                               eqm=sort(CorrectedPcpData_$eqm,decreasing = T),
                               gqm=sort(CorrectedPcpData_$gqm,decreasing = T),
                               Obs=sort(ObsPCPDATA$ObsPCP,decreasing = T),
                               var="precipitation")

CorrectedPcpData2$DATE <- NULL
CorrectedPcpData3 <- melt(data = CorrectedPcpData2,id.vars = c("Obs","var"),na.rm = T)


ggplot(data = CorrectedPcpData3,mapping = aes(x =Obs,y =value ))+
  geom_line(aes(colour=variable),size=1.4,show.legend = F)+
  geom_abline(slope = 1,intercept = 0)+
  coord_equal(ratio =2)+
  xlim(c(0,70))+
  ylim(c(0,70))+
  facet_grid(facets =.~variable )+
  scale_colour_manual(name="",
                      values = c("black","red","green"),
                      labels=c("Observé","Simulé","Corrigé"))+
  xlab("PRECIPITATION OBSERVEE [mm]")+
  ylab("PRECIPITATION CORRIGEE [mm]")+
  theme_bw()+
  theme(legend.position = "top",
        strip.placement = "inside",
        strip.background = element_rect(fill = NA,
                                        colour = "black",
                                        size = 1),
        strip.text = element_text(face = "bold",
                                  colour = "black",size = 12),
        axis.text.x = element_text(face = "bold",
                                   size = 10,colour = "black",angle = 90),
        axis.text.y =element_text(face = "bold",
                                  size = 10,colour = "black",angle = 0),
        axis.title.x =element_text(face = "bold",
                                   size = 12,colour = "black",angle = 0),
        axis.title.y = element_text(face = "bold",
                                    size = 12,colour = "black",angle = 90))
#Température
##Tmax
ObsTmaxDATA <- observeData[!is.na(observeData$ObsTmax),]
CorrectedTmaxData_ <- CorrectedTmaxData[!is.na(observeData$ObsTmax),]
CorrectedTmaxData2 <- transform(CorrectedTmaxData_,
                               scaling=sort(CorrectedTmaxData_$scaling,decreasing = T),
                               eqm=sort(CorrectedTmaxData_$eqm,decreasing = T),
                               Obs=sort(ObsTmaxDATA$ObsTmax,decreasing = T),
                               var="Tmax")
##Tmin
ObsTminDATA <- observeData[!is.na(observeData$ObsTmin),]
CorrectedTminData_ <- CorrectedTminData[!is.na(observeData$ObsTmin),]
CorrectedTminData2 <- transform(CorrectedTminData_,
                               scaling=sort(CorrectedTminData_$scaling,decreasing = T),
                               eqm=sort(CorrectedTminData_$eqm,decreasing = T),
                               Obs=sort(ObsTminDATA$ObsTmin,decreasing = T),
                               var="Tmin")
TempData <- rbind(CorrectedTminData2,CorrectedTmaxData2)
TempData$DATE <- NULL
TempData2 <- melt(data = TempData,id.vars =c("Obs","var"))

#Diagramme Quantiel-Quantile

ggplot(data = TempData2,mapping = aes(x =Obs,y =value ))+
  geom_line(aes(colour=variable),size=1.4,show.legend = F)+
  geom_abline(slope = 1,intercept = 0)+
  facet_grid(facets =var~variable )+
  scale_colour_manual(values = c("blue","red"))+
  xlim(c(10,46))+
  ylim(c(10,46))+
  xlab("TEMPERATURE OBSERVEE [°C]")+
  ylab("TEMPERATURE CORRIGEE [°C]")+
  theme_bw()+
  theme(legend.position = "top",
        strip.placement = "inside",
        strip.background = element_rect(fill = NA,
                                        colour = "black",
                                        size = 1),
        strip.text = element_text(face = "bold",
                                  colour = "black",size = 12),
        axis.text.x = element_text(face = "bold",
                                   size = 10,colour = "black",angle = 90),
        axis.text.y =element_text(face = "bold",
                                  size = 10,colour = "black",angle = 0),
        axis.title.x =element_text(face = "bold",
                                   size = 12,colour = "black",angle = 0),
        axis.title.y = element_text(face = "bold",
                                    size = 12,colour = "black",angle = 90))


#Extraction des données au niveau des stations synoptiques
##Repertiore des données satellitaires
setwd("C:/Users/User/Documents/Memoire/donnees/Donnees_pluvio/CHIRPS/CHIRPS_19812018WA")
##Importation de la couche shapefile des stations synoptiques
StationCoordinates <-  readOGR(dsn ="D:/MODELISATION/Mouhoun/Merging/DATA/StationData/StationCoordinates",layer = "StationSynoptic")
path <- "D:/MODELISATION/Mouhoun/Merging/DATA/StationData/EstimateData/"
## Lecture d'échantillon de données
FilesNames <- dir(pattern = "*.nc")
ExampleFile <- raster::brick(FilesNames[2])# Echantillon de données
DATE <- substr(x = FilesNames,start = 12,stop = 19)

##Extraction
SaveObject <- rbind()
p <- 0
field_names <- c("Bobo","Bogande","Boromo","Dori","Dedougou",
                 "Fada","Gaoua","Ouagadougou","Ouahigouya","Po")
  repeat{
    p <- p+1
    RasterBirck <- ExampleFile <- raster::brick(FilesNames[p])
    RasterExtract <- t(round(raster::extract(x =RasterBirck,StationCoordinates),2))
    SaveObject <- rbind(SaveObject,RasterExtract)
    if(p==length(FilesNames)){
      colnames(SaveObject) <- field_names
      SaveObject_Final <- data.frame(DATE,SaveObject)
      write.table(x = SaveObject_Final,file = paste0(path,"EstimateRainFall.txt"),quote = F,sep = "\t",row.names = F)
    }
    print(paste0("PRECIPITATION_", DATE[p]))
    if(p==length(FilesNames))break
  } 
  
#=======================Extraction des données de Températures================
## Repertoire des données estimées de température
setwd("C:/Users/User/Documents/Memoire/donnees/Temperature/JRA-55")

##Initialisation de la boucle
variables <- c("Tmax","Tmin")
TempData <- list()

for (j in variables) {
  setwd(j)
  TempFilesNames <- dir(pattern = "*.nc")
  SaveObject <- rbind()
  DATE <- substr(x = TempFilesNames,start = 6,stop = 13)
  i <- 0
  repeat{
    i <- i+1
    RasterBirck <- brick(TempFilesNames[i])
    ExtractTemp <- t(round(raster::extract(x =RasterBirck, y = StationCoordinates),2))
    SaveObject <- rbind(SaveObject,ExtractTemp)
    if(i==length(TempFilesNames)){
      colnames(SaveObject) <- field_names
      TempData[[j]] <- data.frame(DATE,SaveObject)
      write.table(x = TempData[[j]],file = paste0(path,"Estimate",j,".txt"),quote = F,sep = "\t",row.names = F)
    }
    print(paste0(j,"-",DATE[i]))
    if(i==length(TempFilesNames))break
  }
  setwd("../")
}
#=========================Traitement primaire des données====================

#Connection de la base de données
setwd("D:/MODELISATION/Mouhoun/DATA/TreatedData")
DataBase2 <- RSQLite::dbConnect(drv = RSQLite::SQLite(),"DataBase.sqlite")
#Importation des fichiers dans la base de données
files_path <- "D:/MODELISATION/Mouhoun/Merging/DATA/StationData/EstimateData"
FileName <- dir(path =files_path ,pattern = ".txt")[-1]
FileName_a <- sub(pattern = ".txt",replacement = "",x = FileName)
k <- 0
repeat{
  k <- k+1
  climateData <- read.table(file = paste(files_path,FileName[k],sep = "/"),header = T,sep = "\t")
  dbWriteTable(conn = DataBase2,name =FileName_a[k] ,value =climateData )
  if(k==length(FileName))break
  }
#------------------Calcul des valeurs annuelles-------------------
#Importation des données
DATE <- seq(as.Date("1981-01-01"),as.Date("2017-12-31"),by = "years")
ref_date <- seq(as.Date("1981-01-01"),as.Date("2017-06-30"),by = "days")
    #Données observées
db_path <- "C:/Users/User/Documents/Memoire/donnees/Donnees_pluvio/ObsData/"
ObserveDataBase <- RSQLite::dbConnect(drv = RSQLite::SQLite(),paste0(db_path,"DataBase.sqlite"))
ObserveRainFall <- dbReadTable(conn =ObserveDataBase ,name ="Tmin_Obs" )
    #Données estimées
EstimateRainFallData <- dbReadTable(conn =DataBase2 ,name ="EstimateTmin" )

# Transformation des données journalières 

    #Calcul de valeurs annuelles des données observées
ObserveRainFall <- transform(ObserveRainFall,DATE=as.Date(paste(ObserveRainFall$Annees,
                                                                ObserveRainFall$Mois,
                                                                ObserveRainFall$Jours,sep = "-")))
ObserveRainFall_1 <- ObserveRainFall[,c(14,4:13)]
ObserveRainFall_1 <- ObserveRainFall_1[ObserveRainFall_1$DATE%in%ref_date,]
AnnualObserveRainFall <- data.frame(DATE,round(daily2annual.data.frame(x = ObserveRainFall_1,FUN = mean,na.rm = T,dates = 1),2))

#Calcul de valeurs annuelles des données estimées
dbfilesnames <- dbListTables(DataBase2)
dbfilesnames_1 <- dbfilesnames[grep("Correct",dbfilesnames)]
j=8
dbfilesnames_1[j]
EstimateRainFallData <- dbReadTable(conn =DataBase2 ,name =dbfilesnames_1[j] )

#EstimateRainFallData <- transform(EstimateRainFallData,DATE=as.Date(paste(substr(x = EstimateRainFallData$DATE,start = 1,stop = 4),
                                                                          #substr(x = EstimateRainFallData$DATE,start = 5,stop = 6),
                                                                          #substr(x = EstimateRainFallData$DATE,start = 7,stop = 8),sep = "-")))

EstimateRainFallData <- transform(EstimateRainFallData,DATE=as.Date(EstimateRainFallData$DATE))

EstimateRainFallData <- EstimateRainFallData[EstimateRainFallData$DATE%in%ref_date,]
AnnualEstimateRainFallData <- data.frame(DATE,round(daily2annual.data.frame(x = EstimateRainFallData,FUN = mean,na.rm = T,dates = 1),2))

#----------------------Formatage CDT------------------------------------
StationsCoordinates <- read_xls("C:/Users/User/Documents/Memoire/donnees/Donnees_pluvio/ObsData/stationsynoptique.xls")
Lon <- c("LON",StationsCoordinates$lon)
Lat <- c("DAILY/LAT",StationsCoordinates$lat)
stationNames <-c("Station",colnames(AnnualEstimateRainFallData)[-1])
AnnualRainFall_CDT <- transform(AnnualEstimateRainFallData,DATE=str_replace_all(string = AnnualEstimateRainFallData$DATE,pattern = "-",replacement = ""))
colnames(AnnualRainFall_CDT) <-NA

AnnualRainFallCDT <- rbind(stationNames,Lon,Lat,AnnualRainFall_CDT)

##sauvegarde
save_path <- "D:/MODELISATION/Mouhoun/Merging/DATA/StationData/AnnualCorrectData/"
write.table(x =AnnualRainFallCDT,file = paste0(save_path,dbfilesnames_1[j],".txt"),sep = "\t",row.names = F,
            col.names = F,quote = F,na = "-99")

#+++++++++++++++++++++Correction de biais++++++++++++++++++++++++++++
DATE <- seq(as.Date("1981-01-01"),as.Date("2017-12-31"),by = "years")
ref_date <- seq(as.Date("1981-01-01"),as.Date("2017-06-30"),by = "days")

#Données observées
#db_path <- "C:/Users/User/Documents/Memoire/donnees/Donnees_pluvio/ObsData/"
#ObserveDataBase <- RSQLite::dbConnect(drv = RSQLite::SQLite(),paste0(db_path,"DataBase.sqlite"))
ObserveDataSet <- dbReadTable(conn =ObserveDataBase ,name ="Precipitation_Obs" )

ObserveDataSet <- transform(ObserveDataSet,DATE=as.Date(paste(ObserveDataSet$Annees,
                                                              ObserveDataSet$Mois,
                                                              ObserveDataSet$Jours,sep = "-")))
ObserveDataSet_1 <- ObserveDataSet[,c(14,4:13)]
ObserveDataSet_2 <- ObserveDataSet_1[ObserveDataSet_1$DATE%in%ref_date,]

#Données estimées
#setwd("D:/MODELISATION/Mouhoun/DATA/TreatedData")
#DataBase2 <- RSQLite::dbConnect(drv = RSQLite::SQLite(),"DataBase.sqlite")
EstimateDataSet <- dbReadTable(conn =DataBase2 ,name ="EstimateRainFall" )

EstimateDataSet <- transform(EstimateDataSet,DATE=as.Date(paste(substr(x = EstimateDataSet$DATE,start = 1,stop = 4),
                                                                          substr(x = EstimateDataSet$DATE,start = 5,stop = 6),
                                                                          substr(x = EstimateDataSet$DATE,start = 7,stop = 8),sep = "-")))

EstimateDataSet_1 <- EstimateDataSet[EstimateDataSet$DATE%in%ref_date,]
frc_index <- which(EstimateDataSet_1$DATE=="2006-01-01")
##Correction
BiasCorrecData <- hyfo::biasCorrect(frc =EstimateDataSet_1,hindcast =EstimateDataSet_1[1:frc_index,],
                                    obs = ObserveDataSet_2[1:frc_index,],method = "gqm",scaleType ='multi',preci = T)

dbWriteTable(conn = DataBase2,name ="PCPCorrectgqm" ,value =BiasCorrecData )

#dbRemoveTable(conn =DataBase2,name = "TminCorrectscaling" )

