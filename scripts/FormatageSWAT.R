#----Extratction des données et formatage format SWAT----------
#Libraries
library(raster)
library(rgdal)
library(sp)
#-------------Importation de la couche des stations ----
StationShapeFile <-  readOGR(dsn = "D:/Internship/Mouhoun/Merging/ShapeFile",layer = "Station")
path <- "D:/MODELISATION/Mouhoun/DATA/StationData/"
# Données de pluies
setwd("C:/Users/User/Documents/Memoire/donnees/Donnees_pluvio/CHIRPS/CHIRPS_19812018WA")
FilesNames <- dir(pattern = "*.nc")
ExampleFile <- raster::brick(FilesNames[2])
DATE <- substr(x = FilesNames,start = 12,stop = 19)
m=1
repeat{
  m <- m+1
  #Extraction des données de pluies 
  
  SaveObject <- rbind()
  p <- 0
  repeat{
    p <- p+1
    RasterBirck <- ExampleFile <- raster::brick(FilesNames[p])
    RasterExtract <- t(round(raster::extract(x =RasterBirck, y = StationShapeFile),2))
    SaveObject <- rbind(SaveObject,RasterExtract)
    if(p==length(FilesNames)){
      colnames(SaveObject) <- StationShapeFile@data$NAME
      SaveObject_Final <- data.frame(DATE,SaveObject)
      write.table(x = SaveObject_Final,file = paste0(path,"pcpData.txt"),quote = F,sep = "\t",row.names = F)
    }
    print(paste0("PRECIPITATION_", DATE[p]))
    if(p==length(FilesNames))break
  }

  #Mise au format SWAT des données de pluies
  StationNames <- colnames(SaveObject_Final[,-1])
  
  SWATformat <- as.data.frame(sapply(SaveObject_Final[,-1],FUN = function(x){
    format_1 <- c("19810101",x)}))
  
  for (k in StationNames) {
    write.table(x =SWATformat[,k] ,file = paste0(path,"pcp_",k,".txt"),quote = F,sep = "\t",row.names = F,col.names = F)
  }
  if(m==2)break
}

   

# Extraction des données de Températures

path2 <- "C:/Users/User/Documents/Memoire/donnees/Temperature/JRA-55"
setwd(path2)
var <- c("Tmax","Tmin")
TempData <- list()

for (j in var) {
  setwd(j)
TempFilesNames <- dir(pattern = "*.nc")
SaveObject <- rbind()
DATE <- substr(x = TempFilesNames,start = 6,stop = 13)
i <- 0
repeat{
  i <- i+1
  RasterBirck <- brick(TempFilesNames[i])
  ExtractTemp <- t(round(raster::extract(x =RasterBirck, y = StationShapeFile),2))
  SaveObject <- rbind(SaveObject,ExtractTemp)
  if(i==length(TempFilesNames)){
    colnames(SaveObject) <- StationShapeFile@data$ID
    TempData[[j]] <- data.frame(DATE,SaveObject)
    write.table(x = TempData[[j]],file = paste0(path,j,".txt"),quote = F,sep = "\t",row.names = F)
  }
  print(DATE[i])
  if(i==length(FilesNames))break
}
  setwd("../")
}


#Mise au format SWAT des donnnées de températures

TmaxEXtract <- as.data.frame(sapply(as.data.frame(TempData[[1]])[,-1],FUN = function(x){
  format_1 <- c(x)}))

TminEXtract <- as.data.frame(sapply(as.data.frame(TempData[[2]])[,-1],FUN = function(x){
  format_1 <- c(x)}))

for (h in 1:length(StationNames)) {
  TmaxTmin <- as.data.frame(paste(TmaxEXtract[,h],TminEXtract[,h],sep = ','))
  colnames(TmaxTmin) <- "19610101"
  write.table(x =TmaxTmin ,file = paste0(path,"tmp_",StationNames[h],".txt"),quote = F,sep = "",row.names = F,col.names = T)
}
