#Préparation des données du modèle GR6j
#Libararies
library(readxl)
library(hydroTSM)
library(airGR)
#-------------------Importation et traitement des données-------------------------
data_path <- "D:/MODELISATION/Mouhoun/DATA/StationData/"
RefDate <- dip("1981-01-01","2017-06-30")

##Traitement des Débits
Discharge <- read_xls("D:/MODELISATION/Mouhoun/DATA/ObservedData/Qjr_Dapola.xls")
Discharge <- transform(Discharge,Date=as.Date(Discharge$Date))
Discharge_1 <- subset(x=Discharge ,subset =Date%in%RefDate)
colnames(Discharge_1) <- c("DATE","FLOWOUT")
BV_Surf <- 98805370461.777634# surface du bassin en m^2
Discharge_2 <- transform(Discharge_1,Qls=Discharge_1$FLOWOUT*10**3,
                         Qmm=Discharge_1$FLOWOUT/BV_Surf*86400*10^3)


##Traitement des données de pluviométriques
PCP <-read.table(file = paste0(data_path,"pcpData.txt"),header = T,sep = "\t")
DATE_PCP <- as.Date(paste(substr(x = PCP$DATE,start = 1,stop = 4),substr(x = PCP$DATE,start = 5,stop = 6),
                          substr(x = PCP$DATE,start = 7,stop = 8),sep = "-"))
PCP$DATE <- NULL
PCPData <- data.frame(DATE=DATE_PCP,PCP=round(rowMeans(PCP),2))
PCPData2 <- subset(x = PCPData,subset = PCPData$DATE%in%RefDate)

##Températures
Tmax <-read.table(file = paste0(data_path,"TmaxData.txt"),header = T,sep = "\t")[,-1]
Tmin <- read.table(file = paste0(data_path,"TminData.txt"),header = T,sep = "\t")[,-1]
Tmean <- as.data.frame((Tmax+Tmin)/2)
BVTeam <- data.frame(DATE=dip("1961-01-01","2017-06-30"),
                     Tmean=rowMeans(Tmean))
BVTeam2 <- subset(x = BVTeam,subset =DATE%in%RefDate)
## Calcul de l'évapotranspiration
InfoFiles <- read.table(file = paste0(data_path,"tmp.txt"),header = T,sep = ",")
DATES=dip("1961-01-01","2017-06-30")
k <- 0
EvapData <- sapply(Tmean, function(x){
  k <<- k+1
  DATE <<- DATES
  lat <<- as.numeric(InfoFiles$LAT[k])
  PotEvap <- PE_Oudin(JD = as.POSIXlt(DATE)$yday + 1,
                      Temp = Tmean[,k],
                      Lat = lat, LatUnit = "deg")
  return(PotEvap)
})

EvapData <- as.data.frame(round(EvapData,2))
EvapData2 <- data.frame(DATES,EvapData)
EvapData3 <- data.frame(DATE=as.Date(EvapData2$DATES),
                        Evap=round(rowMeans(EvapData2[,-1]),2))
EvapData4 <- subset(x =EvapData3,subset =DATE%in%RefDate)

#-----------------Assemblage des données du modèle GR6J-------------
airGrData <- data.frame(DATE=Discharge_2$DATE,
                        FlowOUT=Discharge_2$FLOWOUT,
                        Qls =Discharge_2$Qls,
                        Qmm=Discharge_2$Qmm,
                        PCP=PCPData2$PCP,
                        Temp=BVTeam2$Tmean,
                        Evap=EvapData4$Evap)
write.table(x = airGrData,file = "D:/MODELISATION/Mouhoun/Output/data/airGrData2.txt",quote = F,sep = "\t",row.names = F)


