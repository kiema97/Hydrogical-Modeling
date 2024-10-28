#***************************************************************************#
#               MODELISATION HYDROLOGIQUE AVEC GR6J                         #
#***************************************************************************#
## NETOYAGE DE L'ENVIRONNEMENT
rm(list = ls())
#LIBRAIRIES
library(airGR)
library(stats)
library(ggplot2)
library(reshape2)
library(hydroTSM)
library(DBI)
library(RSQLite)
library(tidyverse)
## WORK SPACE
setwd("G:/PROJET/Article/Modelisation/Mouhoun")
## DATA BASE
DBMouhounDapola <- dbConnect(drv = RSQLite::SQLite(),dbname=file.path("DataBase","DataBase.sqlite"))
#params <- readRDS(file = "D:/PROJET/Article/Modelisation/Mouhoun/Output/ModelCalib/SWATCalibData.rds")
#dbWriteTable(conn = DBMouhounDapola,name = "FULLDATA",value = FillData ,field.types=c(DATE="DATE"),overwrite=T)
#------------------IMPORTATION DE JEU DE DONNEES----------------------------------
best.param <- readRDS(file = "./Output/ModelCalib/GR6JBestParameters.rds")$ParamFinalR

airGrData<- dbReadTable(conn = DBMouhounDapola,name ="GR6JInputData" )%>%
  mutate(DATE=as.POSIXct(DATE))
#-------------------PREPARATION DES INSTRANTS-------------------------
Modelinputs <- CreateInputsModel(FUN_MOD = RunModel_GR6J,
                                 DatesR = airGrData$DATE,
                                 Precip =airGrData$PCP,
                                 PotEvap = airGrData$Evap,
                                 TempMean = airGrData$Temp)
#--------------------DEFINITION DES PERIODES----------------------

Ind_Run <- seq(which(format(airGrData$DATE, format = "%Y-%m-%d")=="1983-01-01"), 
               which(format(airGrData$DATE, format = "%Y-%m-%d")=="2016-12-31"))

Ind_warmUp <- seq(which(format(airGrData$DATE, format = "%Y-%m-%d")=="1981-01-01"), 
                  which(format(airGrData$DATE, format = "%Y-%m-%d")=="1982-12-31"))

RunOptions <- CreateRunOptions(FUN_MOD = RunModel_GR6J,
                               InputsModel = Modelinputs, IndPeriod_Run = Ind_Run,
                               IniStates = NULL, IniResLevels = NULL,
                               IndPeriod_WarmUp = Ind_warmUp,Outputs_Cal = "Qsim")
#------------------------DEFINITION DE LA FONCTION OBJECTIVE---------------------------------
InputsCrit <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, 
                               InputsModel = Modelinputs, 
                               RunOptions = RunOptions,
                               Obs = airGrData$Qmm[Ind_Run])
#---------------------------CALIBRATION INTERNE -------------------------------------------------
CalibOptions <- CreateCalibOptions(FUN_MOD = RunModel_GR6J, FUN_CALIB = Calibration_Michel)

OutputsCalib <- Calibration_Michel(InputsModel = Modelinputs, RunOptions = RunOptions,
                                   InputsCrit = InputsCrit, CalibOptions = CalibOptions,
                                   FUN_MOD = RunModel_GR6J)
#saveRDS(object = OutputsCalib,file = "D:/MODELISATION/Mouhoun/Output/ModelCalib///airGRCalibData.rds")
#-----------------------------EXECUTION DU MODELE-----------------------
OutputsModel <- RunModel_GR6J(InputsModel = Modelinputs, 
                              RunOptions = RunOptions, 
                              Param =OutputsCalib$ParamFinalR)

#-----------------------------EVALUATION DES RESULTATS -----------------------------------------
OutputsCrit <- ErrorCrit_NSE(InputsCrit = InputsCrit, 
                             OutputsModel = OutputsModel)
#V?rification des r?sultats de la simulation
surface_bv <- 98805370461.777634# surface du bassin en m^2
coef_conver <- surface_bv/(86400*10^3)# coefficient de conversion
Ind_Calib <- interval(start =as.Date("2001-01-01") ,end =as.Date("2010-12-31") )
Qobs <- airGrData%>%
  mutate(DATE=as.Date(DATE))%>%
  filter(DATE%within%Ind_Calib,!is.na(FlowOUT))

GR6Sim <- data.frame(DATE=as.Date(OutputsModel$DatesR),Qsim=OutputsModel$Qsim*coef_conver)%>%
  filter(DATE%in%Qobs$DATE)%>%
  mutate(DATE=as.character(DATE))
  


hydroGOF::NSE(sim = GR6Sim$Qsim,obs =Qobs$FlowOUT)
#---------------------------CALIBRATION EXTERNE : EVOLUTION DIFFERENTIELLE--------------------------

DATE <- dip("1998-01-01","2010-12-31")
Refdates <- airGrData$DATE[Ind_Run]
Qobs <- airGrData$FlowOUT[Ind_Run]
k <- 0
paramset <- NULL
OptimGR6J <- function(ParamOptim) {
  ParamOptim2 <<-ParamOptim 
  k <<- k+1
  ## Transformation of the parameter set to real space
  
  RawParamOptim <<-airGR::TransfoParam_GR6J(ParamIn =ParamOptim,
                                            Direction = "TR")
  ## Simulation given a parameter set
  OutputsModel <<- airGR::RunModel_GR6J(InputsModel = Modelinputs,
                                       RunOptions = RunOptions,
                                       Param = RawParamOptim)
  Qsim <<- data.frame(DATE=as.Date(OutputsModel$DatesR),
                      sim=OutputsModel$Qsim*coef_conver)
  ## Computation of the value of the performance criteria
  Qsim_Extract <- subset(Qsim,as.Date(Qsim$DATE)%in%as.Date(airGrData$DATE))
  dis_sim <- Qsim$sim
  CritValue <- hydroGOF::NSE(dis_sim,Qobs)
  ParamOptim2 <-t(RawParamOptim )
  paramset_ <- data.frame(CritValue,ParamOptim2)
  paramset <<-rbind(paramset,paramset_) 
  print(paste0("Meilleur Nash : ",max(paramset$CritValue)))
  print(paste0(k, " : " ,CritValue))
  val_nash <- -CritValue
  return(val_nash)
}

# Parameters boundaries
lowerGR6J <- c(8,-12,9,-10,0,9)
upperGR6J <- c(12,-7,12,10,1,12)

#Calibration
optDE <- DEoptim::DEoptim(fn = OptimGR6J,
                          lower = lowerGR6J, upper = upperGR6J,
                          control = DEoptim::DEoptim.control(NP = 600, 
                                                             trace = 10,
                                                             itermax =2000))
# Evolution du Nash en fonction des param?tres
paramset_ <- paramset
paramset2 <- paramset[paramset$CritValue>=-6,]
paramset2 <- transform(paramset2,CritValue=round(paramset2$CritValue,2))
dd <- duplicated(paramset2$CritValue)
paramset_2 <- paramset2[!dd,]

Plot_data <- melt(data =paramset_2,id.vars ="CritValue"  )
Plot_data <- transform(Plot_data,variable=factor(x = Plot_data$variable,levels = unique(unique(Plot_data$variable))))

ggplot(data = Plot_data,aes(x=value,y=CritValue))+
  geom_point()+
  facet_wrap(facets = .~variable,scales = "free_x")+
  xlab("Param?tres")+
  ylab("Nash Sutcliffe")+
  theme_bw()+
  theme(strip.background = element_rect(fill = NA,colour = "blue",size =1.5),
        strip.text = element_text(face = "italic",size = 10),
        axis.text.x = element_text(size = 9,angle = 90,face = "bold"))


#Analyse des r?sultats
Q_sim_GR6J <- data.frame(DATE=as.Date(OutputsModel$DatesR),q_sim_GR6J=OutputsModel$Qsim*coef_conver)

