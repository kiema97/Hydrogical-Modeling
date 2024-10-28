#==================Analyse du régime hydrologique====================
# Nétoyage de l'environnement
#rm(list = ls())
#libraries
library(readxl)
library(hydrostats)
library(hydroTSM)
library(reshape2)
library(ggplot2)
# Importation des données 
path <- "D:/MODELISATION/Mouhoun/DATA/ObservedData/"
HydroData <- read.table(file = paste0(path,"ObsData.txt"),header = T,sep = "\t")

# Reorganisation des données
DATE <- as.Date(paste(HydroData$ANNEES,HydroData$MOIS,HydroData$JOURS,sep = "-"))
JulianDay <- as.POSIXlt(DATE)$yday+1

HydroData2 <- data.frame(subset(HydroData,select = c("ANNEES","MOIS","JOURS")),
                         JulianDay,subset(HydroData,select = "FlowOut2"))

HydroData3 <- reshape2::acast(data = HydroData2,formula =JulianDay~ANNEES,value.var="FlowOut2")
DateJrs <-substr(x = dip("2000-01-01","2000-12-31"),start = 6,stop = 10) 
row.names(HydroData3) <- DateJrs
write.table(x = HydroData3,file = "D:/MODELISATION/Mouhoun/DATA/ObservedData/HydroData_1.txt",sep = "\t",quote = F,na = "")

#Détermination de l'année hydrologique
save_data_path <- "D:/MODELISATION/Mouhoun/Output/data/"
DataHydroYears <- NULL
for (k in 1:64) {
  xx <- as.matrix(HydroData3[153:366,k])
  yy <- as.matrix(HydroData3[1:152,k+1])
 
  HyDrY <-rbind(xx,yy)
  DataHydroYears <- cbind(DataHydroYears,HyDrY)
}
DataHydroYears <- as.data.frame(DataHydroYears)
name_col_1 <- colnames(HydroData3)[-65]
name_col_2 <-colnames(HydroData3)[-1] 
name_col <- paste0(name_col_1,"-",name_col_2)
colnames(DataHydroYears) <-name_col_1

ModuleData <- data.frame(Years=name_col_1,
                         Module=round(colMeans(x = DataHydroYears,na.rm = T),2))

write.table(x = ModuleData,file = paste0(save_data_path,"ModulesKronoSat.txt"),quote = F,sep = "\t",row.names = F,dec = ",")

ModuleData2 <-ModuleData[!is.na(ModuleData$Module),] 

