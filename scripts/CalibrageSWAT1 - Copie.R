#---------------Calibrage du Modèle SWAT----------------

    #Libraries
library(SWATplusR)
library(DBI)
library(RSQLite)
library(sf)
library(tidyr)
library(tibble)
library(dplyr)
library(fast)
library(forcats)
library(ggplot2)
library(hydroGOF)
library(hydroTSM)
library(lhs)
library(lubridate)
library(purrr)
library(sensitivity)
library(stringr)
library(hydroPSO)
library(DEoptim)
#-1 Importation des données observées
path_1 <- "D:/MODELISATION/Mouhoun/DATA/ObservedData/"
obs <- read.table(file = paste0("D:/MODELISATION/Mouhoun/Output/data/","airGrData2.txt"),header = T,sep = "\t")
obs <- transform(obs,DATE=as.Date(obs$DATE))

Q_Obs_Data <- read.table(file = paste0(path_1,"ObsData.txt"),header = T,sep = "\t")
#-2 Première exécution du modèle
project_path <- "D:/MODELISATION/Mouhoun/Projet/Scenarios/Default/TxtInOut"

modele_run <- run_swat2012(
  project_path = project_path,
  output =define_output(file = "rch",
                        variable = "FLOW_OUT",
                        unit = 23),
  start_date = "2000-01-01",
  end_date = "2005-12-31",
  years_skip = 3)
#========================Etude de sensibilité =================
setwd("D:/MODELISATION/Mouhoun/Projet/SWATplusR/Etude_sensibilite")
#1- Constitution d'un Jeux de paramètre

SWATParameters<- tibble("GW_DELAY.gw|change = absval" = c(30, 450),
                      "OV_N.hru|change = absval" = c(0.01, 30),
                      "CN2.mgt|change = relchg" = c(-0.2, 0.2),
                      "REVAPMN.gw|change = absval" = c(0, 500),
                      "SOL_AWC.sol|change = absval" = c(0.01, 0.5),
                      "SURLAG.bsn|change = absval" = c(0, 24),
                      "ESCO.hru|change = absval" = c(0, 1),
                      "SHALLST.gw|change = absval" = c(0, 1000),
                      "GWQMN.gw|change = absval" = c(0, 5000),
                      "ALPHA_BF.gw|change = absval" = c(0, 1),
                      "LAT_TIME.hru|change = absval" = c(0, 180),
                      "SLSOIL.hru|change = absval" = c(0, 150),
                      "HRU_SLP.hru|change = absval" = c(0, 0.6),
                      "CH_K2.rte|change = absval" = c(0, 500),
                      "SOL_Z.sol|change = abschg" = c(0, 1000),
                      "CH_K1.sub|change = absval" = c(0, 300),
                      "SLSUBBSN.hru|change = absval" = c(10, 150),
                      "CANMX.hru|change = absval" = c(0, 100),
                      "CH_N2.rte|change = absval" = c(0, 0.3),
                      "CH_N1.sub|change = absval" = c(0.01, 30),
                      "EVRCH.bsn|change = absval" = c(0.5, 1),
                      "GW_REVAP.gw|change = absval" = c(0, 0.3),
                      "RCHRG_DP.gw|change = absval" = c(0, 1),
                      "EPCO.hru|change = absval" = c(0, 1),
                      "PLAPS.sub|change = absval" = c(0, 100))


lower <- as.numeric(SWATParameters[1,]) # Borne inférieures des paramétres
upper <- as.numeric(SWATParameters[2,])# Bornes superieures des paramétres

#2- Combinaison des paramètres

ParameterFast <- fast_parameters(
  minimum =lower,
  maximum =upper,
  names =names(SWATParameters)) %>%
  as_tibble()
#3- Analyse de la sensibilité
save_path <- "D:/MODELISATION/Mouhoun/Projet/SWATplusR/Etude_sensibilite"

for (k in 1:nrow(ParameterFast)) {
  
  SensTest2<- run_swat2012(
    project_path = project_path,
    output =define_output(file = "rch",
                          variable = "FLOW_OUT",
                          unit = 23),
    parameter = ParameterFast,
    start_date = "2001-01-01",
    end_date = "2010-12-31",
    years_skip = 3,
    output_interval = "d", 
    keep_folder = T)
  print(k)
  
}
# Sauvegarde des données
output_path <- "D:/MODELISATION/Mouhoun/Output/ModelCalib/"
saveRDS(object =SensTest,file = paste0(output_path,"//SensDischarge.rds")) # sauvegarde des données de la simulation

#Calcul de critère de performance
    
    # Formatage de données

DATE <- as.Date(paste(Q_Obs_Data$ANNEES,Q_Obs_Data$MOIS,Q_Obs_Data$JOURS,sep = "-"))
Q_Obs_Data2<- data.frame(DATE,FlowOut=Q_Obs_Data$FlowOut2)
sim_data <- SensTest$simulation$FLOW_OUT

q_obs <- subset(Q_Obs_Data2,as.Date(Q_Obs_Data2$DATE)%in%as.Date(dip("2004-01-01","2010-12-31")))
q_sim <- subset(sim_data,as.Date(sim_data$date)%in%as.Date(q_obs$DATE))
    
    #Calcul de Nash
NSEVal <- sapply(q_sim[,-1],function(x){
  nse <- NSE(x,q_obs$FlowOut)
  nse
})

sensval_fast <- sensitivity(NSEVal, 25)

#Lecture des données:
CalibSWATdata <- data.frame(Nash=NSEVal,SensTest$parameter$values)
CalibSWATdata <- CalibSWATdata[order(CalibSWATdata$Nash,decreasing = T),]
saveRDS(object =CalibSWATdata,file = paste0(output_path,"SWATCalibData.rds")) # sauvegarde des données de la simulation


    #Représentation
         #Formatage de données
par_names <- SensTest$parameter$definition$par_name
ResultFast <- tibble(parameter = as.factor(par_names),
                     fast      = sensval_fast)%>%
  mutate(parameter = factor(parameter)%>% 
           fct_reorder(., fast))

          #Plots
ggplot(data = ResultFast) +
  geom_col(aes(x = parameter, y = fast),fill="steelblue",color="black")+
  xlab(toupper("Paramèters")) +
  ylab(toupper("Sensibilité"))+ 
  coord_flip(expand = T) +
  theme_bw()+
  theme(axis.text.x = element_text(colour = "black",size = 10),
        axis.title.x.bottom =element_text(colour = "black",face = "bold",size = 12),
        axis.title.y =element_text(colour = "black",face = "bold",size = 12),
        axis.text.y = element_text(colour = "black",size = 8))

#Plot
Nash_Lab <- rep(c("positive","negative"),times=c(911,2644))
CalibSWATdata$Nash_Lab <- Nash_Lab
CalibSWATdata2 <- melt(data = CalibSWATdata,id.vars = c("Nash","Nash_Lab"))
CalibSWATdata3 <- CalibSWATdata2[CalibSWATdata2$Nash>=-1,]

ggplot(data = CalibSWATdata3,aes(x=value,y=Nash))+
  geom_point()+
  facet_wrap(facets = .~variable,scales = "free_x",as.table = F)+
  xlab("Paramètres")+
  ylab("Nash Sutcliffe")+
  theme_bw()+
  theme(strip.background = element_rect(fill = NA,colour = "blue",size =1.5),
        strip.text = element_text(face = "italic",size = 10),
        axis.text.x = element_text(size = 9,angle = 90,face = "bold"))


#=====================OPTIMISATION=================================

    # Extraction des paramètres sensibles

ResultFast2 <- ResultFast[order(ResultFast$fast,decreasing = T),]
sens_params <-NULL
for (j in 1:9) {
  sens_params2 <-as.matrix(SWATParameters[,grep(ResultFast2$parameter[j],names(SWATParameters))]) 
  sens_params <- cbind(sens_params,sens_params2)
}

lower_vals <- as.numeric(sens_params[1,])
upper_vals <-  as.numeric(sens_params[2,])

    # Fonction d'optimisation
OptimParameters <- as_tibble(sens_params)
q_obs <- subset(Q_Obs_Data2,as.Date(Q_Obs_Data2$DATE)%in%as.Date(dip("2001-01-01","2010-12-31")))
dis_obs <-q_obs$FlowOut 
Parametrs_2 <- NULL
k <- 0
param <- as_tibble(SensTest$parameter$values[314,])
colnames(param) <- colnames(SWATParameters)
param2 <- param[,colnames(OptimParameters)]

OptimSWATplus <- function(ParamOptim) {
  ParamOptim <<- as.numeric(ParamOptim)
  ParamOptim2 <<-as_tibble(t(ParamOptim ))
  params <<- fast_parameters(
    minimum =ParamOptim,
    maximum =ParamOptim,
    names =names(OptimParameters)) %>%
    as_tibble()
  params_2 <<- params[1,]
  k <<-k+1 
  ## Simulation given a parameter set
  print(k)
  OutputsModel <<- SWATplusR::run_swat2012(
    project_path = project_path,
    output =define_output(file = "rch",
                          variable = "FLOW_OUT",
                          unit = 23),
    parameter = param2,
    start_date = "1998-01-01",
    end_date = "2010-12-31",
    years_skip = 3,
    output_interval = "d", 
    keep_folder = T,refresh = F)
  ## Evaluation de la simulation
  q_sim_ <-as.data.frame(OutputsModel$simulation)
  q_sim <- subset(q_sim_,as.Date(q_sim_$date)%in%as.Date(q_obs$DATE))
  dis_sim <<- q_sim$FLOW_OUT
  
  nse <- -NSE(dis_sim,dis_obs)
  Parametrs <- cbind(nse,params_2)
  Parametrs_2 <<-  rbind(Parametrs_2,Parametrs)
  Nash_val <- -min(Parametrs_2$nse)
  print(paste0("Meilleur Nash : ",Nash_val))
  print(-nse)
  return(nse) 
}

    # Evolution Différentielle
      
      # Première exécution

calibDEoptim <- DEoptim::DEoptim(fn = OptimSWATplus,
                                 lower = lower_vals, upper = upper_vals,
                                 control = DEoptim::DEoptim.control(NP = 500, trace = 10,VTR=-1,
                                                                    steptol=500,
                                                                    reltol = 10^-20,
                                                                    strategy = 6,
                                                                    itermax=5000))
      # Deuxième exécution

lower_sens1 <- lower_vals*0.1-lower_vals
upper_sens1 <- upper_vals*0.1+upper_vals

dd <- duplicated(Parametrs_2$nse)
ParamsNSE <- Parametrs_2[!dd,]

initialpop1 <- subset(ParamsNSE,ParamsNSE$nse<=1)
initialpop <- as.matrix(initialpop1[order(initialpop1$nse),-1])
initialpop <- as_tibble(initialpop)
 
#saveRDS(object = initialpop,file = "Omptim_Q.rds")
calibDEoptim <- DEoptim::DEoptim(fn = OptimSWATplus,
                                 lower = lower_vals, upper = upper_vals,
                                 control = DEoptim::DEoptim.control(NP = 395, trace = 10,VTR=-1,
                                                                    steptol=500,
                                                                    reltol = 10^-20,
                                                                    strategy = 6,
                                                               itermax=5000,
                                                                    initialpop = initialpop))

# Analyse des résultats de la calibration  
CalibSWATdata2 <- CalibSWATdata[order(CalibSWATdata$Nash,decreasing = T),]
best_param <- as.numeric(CalibSWATdata2[1,][,-c(1,27)])
best_param2 <<- fast_parameters(
  minimum =best_param,
  maximum =best_param,
  names =names(SWATParameters)) %>%
  as_tibble()
best_param_2 <- best_param2[1,]

modele_run <- run_swat2012(
  project_path = project_path,
  output =define_output(file = "rch",
                        variable = "FLOW_OUT",
                        unit = 23),
  parameter = best_param_2,
  start_date = "2008-01-01",
  end_date = "2017-06-30",
  years_skip = 3)

Q_sim_SWAT <- modele_run$simulation
Q_sim_SWAT2 <- subset(x = Q_sim_SWAT,subset =date%in%q_obs$DATE)
q_obs <- subset(obs,subset =DATE%in%dip("2011-01-01","2017-06-30"),select =c("DATE","FlowOUT"))
NSEVal <- sapply(Q_sim_SWAT[,-1],function(x){
  nse <- NSE(x,q_obs$FlowOUT)
  nse
})

obs <- subset(obs,subset =DATE%in%Q_sim_SWAT$date)
Data_validation_SWAT_GR6J <- data.frame(DATE=Q_sim_GR6J$DATE,SWAT=Q_sim_SWAT$FLOW_OUT,GR6J=Q_sim_GR6J$q_sim_GR6J,obs=obs$FlowOUT)
saveRDS(object = Data_validation_SWAT_GR6J,file =paste0(output_path,"Data_validation_SWAT_GR6J.rds") )

hydroGOF::NSE(sim = Data_validation_SWAT_GR6J$SWAT,obs=Data_validation_SWAT_GR6J$obs)
