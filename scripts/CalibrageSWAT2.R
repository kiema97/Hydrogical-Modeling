#***************************************************************************#
#               MODELISATION HYDROLOGIQUE AVEC SWAT                         #
#***************************************************************************#
## NETOYAGE DE L'ENVIRONNEMENT
rm(list = ls())
#LIBRAIRIES
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
library(lubridate)
## Tuto
dplyr_vignette <- vignette(package="dplyr")$results
vignette(topic="rowwise",package="dplyr")
## WORK SPACE
setwd("D:/PROJET/Article/Modelisation/Mouhoun")
## DATA BASE
DBMouhounDapola <- dbConnect(drv = RSQLite::SQLite(),dbname=file.path("DataBase","DataBase.sqlite"))
## MPORTATION DES DONNEES
GR6JSimDATA <-dbReadTable(conn =DBMouhounDapola ,name = "GR6JQSim")%>%
  mutate(DATE=as.Date(DATE))%>%
  rename(GR6J=Qsim)

SWATSimDATA <- dbReadTable(conn =DBMouhounDapola ,name = "SWATQSim" )%>%
  mutate(DATE=as.Date(DATE))%>%
  rename(SWAT=FLOW_OUT)

QobsData <- dbReadTable(conn = DBMouhounDapola,name = "DapolaQOBS")%>%
  mutate(DATE=as.Date(DATE))%>%
  rename(OBS=Debit)
FillData2 <- dbReadTable(conn =DBMouhounDapola,name = "FULLDATA" )%>%
  group_by(DATE)%>%
  mutate(GR6j_SWAT_Mean=(GR6J+SWAT)/2)

FillData <- QobsData%>%
  full_join(GR6JSimDATA,by="DATE")%>%
  full_join(SWATSimDATA,by="DATE")%>%
  mutate(DATE=as.character(DATE),
         ObsGR6J=ifelse(is.na(OBS),GR6J,OBS),
         ObsSWAT=ifelse(is.na(OBS),SWAT,OBS))
  
  

## FIXATION DES PERIODES E EXTRACTION DES DONNEES 
calib.periode <- interval(start = as.Date("2001-01-01"),end = as.Date("2010-12-31"))
valid.periode <- interval(start = as.Date("2011-01-01"),end = as.Date("2016-12-31"))

calib.data <- QDapolaOBS%>%
  filter(DATE%within%calib.periode)

valid.data <- QDapolaOBS%>%
  filter(DATE%within%valid.periode)

##DEFINITION DES PARAMETRES DE BASE

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

#- DEFINITION DES INTERVALLES DE VARIATION DES PARAMETRES
lower <- as.numeric(SWATParameters[1,]) # Borne inf?rieures des param?tres
upper <- as.numeric(SWATParameters[2,])# Bornes superieures des param?tres


#- PREPARATION DES PARAMETRES OPTIMUM
Param.Swat <- dbReadTable(conn = DBMouhounDapola,name = "SWATSensitiveDATA")%>%
  arrange(desc(Nash))%>%
  slice(1)%>%
  select(-Nash)%>%
  as_tibble()

param.index2 <- c()
for (j in colnames(Param.Swat)) {
  param.index <- grep(pattern =j,x = colnames(SWATParameters))
  param.index2 <- c(param.index2,param.index)
}

colnames(Param.Swat) <- colnames(SWATParameters)[param.index2]

## EVALUATION DU MODEL
project_path <- "D:/PROJET/Article/Modelisation/Mouhoun/Projet/Scenarios/Default/TxtInOut"

modele_run <- run_swat2012(
  project_path = project_path,
  output =define_output(file = "rch",
                        variable = "FLOW_OUT",
                        unit = 23),
  parameter = Param.Swat,
  start_date = "1981-01-01",
  end_date = "2016-12-31",
  years_skip = 2)


SimData <- modele_run$simulation%>%
  filter(date%within%calib.periode)

NSE(sim =SimData$FLOW_OUT,obs = calib.data$Debit )
#============ETUDE DE SENSIBLITE============
setwd("D:/MODELISATION/Mouhoun/Projet/SWATplusR/Etude_sensibilite")

##- ELABORATION DE JEU DE PARAMETRES

ParameterFast <- fast_parameters(
  minimum =lower,
  maximum =upper,
  names =names(SWATParameters)) %>%
  as_tibble()

 
## ANALYSE DE LA SENSIBILITE DES PARAMETRES CONSTITUES
SensTest<- run_swat2012(
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

## SAUVEGARDE DES RESUTATS DE L'ETUDE DE SENSIBILITE
output_path <- "D:/MODELISATION/Mouhoun/Output/ModelCalib/"
saveRDS(object =SensTest,file = paste0(output_path,"//SensDischarge.rds")) # sauvegarde des donn?es de la simulation

#Calcul de crit?re de performance
    
##-1 Formatage de donn?es
periode_simulation <- SensTest$simulation$date
sim_data <- SensTest$simulation

q_obs <- Q_Obs_Data%>%
  subset(select(.,DATE)%>%unlist%>%as.Date%in%periode_simulation)

q_sim <- sim_data%>%
  subset(select(.,date)%>%unlist%>%as.Date%in%as.Date(q_obs$DATE))
    
##-2 Calcul de Nash
NSEVal <- sapply(q_sim[,-1],function(x){
  nse <- NSE(x,q_obs$FlowOut2)
  nse
})

##-3 calcul des valeurs de sensibilit?
sensval_fast <- sensitivity(NSEVal, 25)

## Sauvegarde des r?sultats
CalibSWATdata <- data.frame(Nash=NSEVal,SensTest$parameter$values)
CalibSWATdata <- CalibSWATdata[order(CalibSWATdata$Nash,decreasing = T),]
saveRDS(object =CalibSWATdata,file = paste0(output_path,"SWATCalibData.rds")) # sauvegarde des donn?es de la simulation


##-4 Visualisation des r?sultat

##Formatage de donn?es
par_names <- SensTest$parameter$definition$par_name
ResultFast <- tibble(parameter = as.factor(par_names),
                     fast      = sensval_fast)%>%
  mutate(parameter = factor(parameter)%>% 
           fct_reorder(., fast))

#Plots: graphiques donnant les param?tres par ordre de sensibilit?
ggplot(data = ResultFast) +
  geom_col(aes(x = parameter, y = fast),fill="steelblue",color="black")+
  xlab(toupper("Param?ters")) +
  ylab(toupper("Sensibilit?"))+ 
  coord_flip(expand = T) +
  theme_bw()+
  theme(axis.text.x = element_text(colour = "black",size = 10),
        axis.title.x.bottom =element_text(colour = "black",face = "bold",size = 12),
        axis.title.y =element_text(colour = "black",face = "bold",size = 12),
        axis.text.y = element_text(colour = "black",size = 8))

#Plot: graphique montrant l'?volution du crit?re de Nash en fonction des valeurs prises par les param?tres
Nash_Lab <- rep(c("positive","negative"),times=c(911,2644))
CalibSWATdata$Nash_Lab <- Nash_Lab
CalibSWATdata2 <- melt(data = CalibSWATdata,id.vars = c("Nash","Nash_Lab"))
CalibSWATdata3 <- CalibSWATdata2[CalibSWATdata2$Nash>=-1,]

ggplot(data = CalibSWATdata3,aes(x=value,y=Nash))+
  geom_point()+
  facet_wrap(facets = .~variable,scales = "free_x",as.table = F)+
  xlab("Param?tres")+
  ylab("Nash Sutcliffe")+
  theme_bw()+
  theme(strip.background = element_rect(fill = NA,colour = "blue",size =1.5),
        strip.text = element_text(face = "italic",size = 10),
        axis.text.x = element_text(size = 9,angle = 90,face = "bold"))


#=====================OPTIMISATION des param?tres=================================

##Extraction des param?tres sensibles

ResultFast2 <- ResultFast[order(ResultFast$fast,decreasing = T),]
sens_params <-NULL
for (j in 1:9) {
  sens_params2 <-as.matrix(SWATParameters[,grep(ResultFast2$parameter[j],names(SWATParameters))]) 
  sens_params <- cbind(sens_params,sens_params2)
}
OptimParameters <- as_tibble(sens_params)

lower_vals <- as.numeric(sens_params[1,])# bornes inf?rieures des param?tres
upper_vals <-  as.numeric(sens_params[2,])#bornes superieures des param?tres

    # Fonction d'optimisation
periode_calage <- as.Date(dip("2001-01-01","2010-12-31"))
q_obs2 <- Q_Obs_Data%>%
  subset(DATE%in%periode_calage)
dis_obs <-q_obs2$FlowOut2
##Cr?ation de la fonction d'optimisation
##Initialisation des param?tres
Parametrs_2 <- NULL
k <- 0
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
  ## Simulation 
  print(k)
  OutputsModel <<- SWATplusR::run_swat2012(
    project_path = project_path,
    output =define_output(file = "rch",
                          variable = "FLOW_OUT",
                          unit = 23),
    parameter = params_2,
    start_date = "1998-01-01",
    end_date = "2010-12-31",
    years_skip = 3,
    output_interval = "d", 
    keep_folder = T,refresh = F)
## Evaluation de la simulation
  q_sim<-OutputsModel$simulation%>%as.data.frame%>%
    subset(select(.,date)%>%unlist%>%as.Date%in%as.Date(q_obs2$DATE))
    
dis_sim <-q_sim$FLOW_OUT
  
  nse <- -NSE(dis_sim,dis_obs)
  Parametrs <- cbind(nse,params_2)
  Parametrs_2 <<-  rbind(Parametrs_2,Parametrs)
  Nash_val <- -min(Parametrs_2$nse)
  print(paste0("Meilleur Nash : ",Nash_val))
  print(-nse)
  return(nse) 
}

## Optimisation avec la m?thode de l'Evolution Diff?rentielle
      
#Premi?re ex?cution
calibDEoptim <- DEoptim::DEoptim(fn = OptimSWATplus,
                                 lower = lower_vals, upper = upper_vals,
                                 control = DEoptim::DEoptim.control(NP = 500, trace = 10,VTR=-1,
                                                                    steptol=500,
                                                                    reltol = 10^-20,
                                                                    strategy = 6,
                                                                    itermax=5000))
#Deuxi?me ex?cution: modification des place de variations
lower_sens1 <- lower_vals*0.1-lower_vals
upper_sens1 <- upper_vals*0.1+upper_vals

dd <- duplicated(Parametrs_2$nse)# suppression des valeurs de Nash identique
ParamsNSE <- Parametrs_2[!dd,]

#d?finition d'une population initiale (jeu de param?tres)
initialpop1 <- subset(ParamsNSE,ParamsNSE$nse<=1)
initialpop <- as.matrix(initialpop1[order(initialpop1$nse),-1])
initialpop <- as_tibble(initialpop)
 
#saveRDS(object = initialpop,file = "Omptim_Q.rds") # sauvedarde de la population initiale
##Deuxi?me ex?cution du mod?le
calibDEoptim <- DEoptim::DEoptim(fn = OptimSWATplus,
                                 lower = lower_vals, upper = upper_vals,
                                 control = DEoptim::DEoptim.control(NP = 395, trace = 10,VTR=-1,
                                                                    steptol=500,
                                                                    reltol = 10^-20,
                                                                    strategy = 6,
                                                                    itermax=5000,
                                                                    initialpop = initialpop))
#=================Analyse des r?sultats de la calibration===========  
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

