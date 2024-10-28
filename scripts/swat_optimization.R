#############################################################################
#########       HYDROLOGICAL MODELING : OPTIMIZATION                   ##############
#############################################################################

## NETTOYAGE
rm(list = ls())

## LIBRARIES
library(SWATrunR)
library(tidyverse)
library(DBI)
library(RSQLite)
library(sf)
library(tibble)
library(fast)
library(forcats)
library(hydroGOF)
library(hydroTSM)
library(lhs)
library(purrr)
library(sensitivity)
library(stringr)
library(hydroPSO)
library(DEoptim)


## WORK SPACE
setwd("G:/PROJET/Article/MODELISATION_II/SWAT")

## DATA BASE
db_path <- "G:/PROJET/Article/Modelisation/Mouhoun/DataBase"
DataBase <- dbConnect(drv = RSQLite::SQLite(),
                      dbname=file.path(db_path,"DataBase.sqlite"))
dbListTables(DataBase)

## DATA IMPORTATION
ET_DATA<- dbReadTable(conn = DataBase,name = "CLIM_SUBS_DATA_ML")%>%
  select(YYYYMMDD,contains("ET"))%>%
  rowwise(YYYYMMDD)%>%
  summarise(ET=mean(ET1:ET23))%>%
  mutate(YYYYMMDD=as_date(YYYYMMDD))

Q_Obs_Data <- dbReadTable(conn = DataBase,name = "DapolaQOBS")%>%
  mutate(DATE=as_date(DATE))%>%
  left_join(ET_DATA,by=c("DATE"="YYYYMMDD"))

## DEFINITION DES PERIODES
calib.periode <- lubridate::interval(start =as.Date("2001-01-01") ,
                                          end = as.Date("2010-12-31"))

valid.periode <- lubridate::interval(start =as.Date("2011-01-01") ,
                                          end = as.Date("2015-12-31"))

Calib.Data <- Q_Obs_Data%>%
  filter(DATE%within%calib.periode)

Valid.Data <-Q_Obs_Data%>%
  filter(DATE%within%valid.periode)


## Sensitivity Results
parameters_sensitivity <- readRDS("G:/PROJET/Article/Modelisation/Mouhoun/Output/ModelCalib/parameters_sensitivity.rds")

# Parametres initiaux
source("scripts/swat_parameters.R")
lower <- as.numeric(SWATParameters[1,]) # Borne inf?rieures des param?tres
upper <- as.numeric(SWATParameters[2,])# Bornes superieures des param?tres

#=====================OPTIMISATION=================================

# Extraction des param?tres sensibles

ResultFast2 <- parameters_sensitivity%>%
  arrange(desc(fast))

sens_params <-NULL
for (j in 1:9) {
  sens_params2 <-as.matrix(SWATParameters[,grep(ResultFast2$parameter[j],names(SWATParameters))]) 
  sens_params <- cbind(sens_params,sens_params2)
}

lower_vals <- as.numeric(sens_params[1,])
upper_vals <-  as.numeric(sens_params[2,])

# Fonction d'optimisation
OptimParameters <- as_tibble(sens_params)
q_obs_calib <- Q_Obs_Data%>%
  filter(DATE>=as.Date("2001-01-01"),DATE<=as.Date("2010-12-31"))
dis_obs <-q_obs_calib$FlowOut 

#param <- as_tibble(SensTest$parameter$values[314,])
#colnames(param) <- colnames(SWATParameters)
#param2 <- param[,colnames(OptimParameters)]

# Créer une barre de progression
total_iterations <- 100
pb <- txtProgressBar(min = 0, max = total_iterations, style = 3)

Parametrs_2 <- NULL
k <- 0
calib_simulation <<- rbind()
OptimSWATplus <- function(ParamOptim) {
  # Progression
  k <<-k+1 
  setTxtProgressBar(pb, k)
  
  # parameters
  ParamOptim <<- as.numeric(ParamOptim)
  ParamOptim2 <<-as_tibble(t(ParamOptim ))
  
  params <<- fast_parameters(
    minimum =ParamOptim,
    maximum =ParamOptim,
    names =names(OptimParameters)) %>%
    as_tibble()
  params_2 <<- params[1,]
  print(k)
  ## Model Run
  OutputsModel <<- SWATrunR::run_swat2012(
    project_path = project_path,
    output =define_output(file = "rch",
                          variable = "FLOW_OUT",
                          unit = 23),
    parameter = params_2,
    start_date = "1998-01-01",
    end_date = "2010-12-31",
    years_skip = 3,
    output_interval = "d", 
    keep_folder = T,refresh = TRUE,quiet = TRUE
    )
  
  ## Evaluation de la simulation
  q_sim <-as.data.frame(OutputsModel$simulation$FLOW_OUT)
  dis_sim <<- q_sim$run_1
  nse <- NSE(dis_sim,dis_obs)
  
if(nse>=0.5 & is.null(calib_simulation)){
  colnames(q_sim) <- c("date",paste0("run_",k))
  calib_simulation <<- q_sim }
  
if(nse>=0.5 & !is.null( calib_simulation)){
  colnames(q_sim) <- c("date",paste0("run_",k))
  calib_simulation <<- calib_simulation%>%
    left_join(q_sim,by="date")}
  
  Parametrs <- cbind(runs=paste0("run_",k),nse,params_2)
  Parametrs_2 <<-  rbind(Parametrs_2,Parametrs)
  Nash_val <- max(Parametrs_2$nse)
  print(paste0("Meilleur Nash : ",Nash_val))
  print(nse)
  if(k==total_iterations)break
  return(-nse) 
}


save_path <- "outputs/modelisation"
project_path <- "G:/PROJET/Article/Modelisation/Mouhoun/Projet/Scenarios/Default/TxtInOut"

SWAT.PARAMETERS <- rbind()
start_run_date <- "1998-01-01"
end_run_date <- "2015-12-31"
SWAT_SIM <- data.frame(DATE=dip("2001-01-01","2015-12-31"))
k=0
j=0
total_iterations <- 2000
pb <- txtProgressBar(min = 0, max = total_iterations, style = 3)

OptimSWAT <- function(ParamOptim) {
  # Progression
  k <<-k+1 
  setTxtProgressBar(pb, k)
  
  params <<- fast_parameters(
    minimum =ParamOptim,
    maximum =ParamOptim,
    names =names(OptimParameters)) %>%
    as_tibble()
  
  params_2 <<- params[1,]
  ## Model Run
  OutputsModel <<- SWATrunR::run_swat2012(
    project_path = project_path,
    output =list(FLOW_OUT=define_output(file = "rch",
                                       variable = "FLOW_OUT",
                                       unit = 23),
                 ET=define_output(file = "hru",
                                  variable = "ET",
                                  unit = 1:23)),
    #parameter = params_2,
    start_date = start_run_date,
    end_date = end_run_date,
    years_skip = 3,
    output_interval = "d", 
    keep_folder = T,refresh = TRUE,quiet = TRUE
  )
  
  ## Evaluation de la simulation
  et_sim <- as.data.frame(OutputsModel$simulation[paste0("ET_",1:23)])%>%
    select(c(1,seq(2,46,2)))%>%
    rowwise(ET_1.date)%>%
    summarize(ET=mean(ET_1.run_1:ET_23.run_1))
  colnames(et_sim) <- c("DATE",paste0("run_",k))
  
  Qsim <<- data.frame(OutputsModel$simulation$FLOW_OUT)#%>%  #*coef_conver
  colnames(Qsim) <- c("DATE",paste0("run_",k))
  SWAT_SIM <- Qsim%>%
    left_join(et_sim,by=c("DATE"),suffix = c("_Q","_ET"))
  colnames(SWAT_SIM) <- c("DATE","Debit","ET")
  
  swat_sim_calib <-SWAT_SIM%>%
    filter(DATE%within%calib.periode)
  
  swat_sim_valid <-SWAT_SIM%>%
    filter(DATE%within%valid.periode)
  #Q
  Q_Calib_KGE <<- hydroGOF::KGE(swat_sim_calib$Debit,Calib.Data$Debit,method="2012")
  
  Q_Calib_NSE <- hydroGOF::NSE(swat_sim_calib$Debit,Calib.Data$Debit,method="2012")
  
  Q_Valid_KGE <- hydroGOF::KGE(swat_sim_valid$Debit,Valid.Data$Debit,method="2012")
  
  Q_Valid_NSE <- hydroGOF::NSE(swat_sim_valid$Debit,Valid.Data$Debit,method="2012")
  
  #ETP
  ET_Calib_KGE <<- hydroGOF::KGE(swat_sim_calib$ET,Calib.Data$ET,method="2012")
  
  ET_Calib_NSE <- hydroGOF::NSE(swat_sim_calib$ET,Calib.Data$ET,method="2012")
  
  ET_Valid_KGE <- hydroGOF::KGE(swat_sim_valid$ET,Valid.Data$ET,method="2012")
  
  ET_Valid_NSE <- hydroGOF::NSE(swat_sim_valid$ET,Valid.Data$ET,method="2012")
  
  swat_calib_kge <- min(-ET_Calib_KGE,-Q_Calib_KGE)
  swat_calib_nse <- min(-ET_Calib_NSE,-Q_Calib_NSE)
  if( !is.nan(swat_calib_kge) & !is.na(swat_calib_kge) & swat_calib_kge>=0.5){
    SWAT_SIM <<- SWAT_SIM%>%
      left_join(SWAT_SIM,by="DATE")
    
    ParamOptim2 <-t(ParamOptim)
    run.param <- data.frame(ParamOptim2,Q_Calib_KGE,Q_Calib_NSE,Q_Valid_KGE,Q_Valid_NSE,
                            ET_Calib_KGE,ET_Calib_NSE,ET_Valid_KGE,ET_Valid_NSE,swat_calib_kge,swat_calib_nse)%>%
      mutate(run=paste0("run_",k))
    colnames(run.param) <- c(OutputsModel$parameter$definition$par_name,
                             "Q_Calib_KGE","Q_Calib_NSE","Q_Valid_KGE","Q_Valid_NSE",
                             "ET_Calib_KGE","ET_Calib_NSE","ET_Valid_KGE","ET_Valid_NSE",
                             "swat_calib_kge","swat_calib_nse","run")
    
    SWAT.PARAMETERS <<-rbind(SWAT.PARAMETERS,run.param)
  }#
  
  print(paste0("Best Goodness Of Fit : ",min(SWAT.PARAMETERS$swat_calib_kge,na.rm = TRUE)))
  print(paste0(k, " : " ,swat_calib_kge))
  
  if(k==total_iterations)break
  
  if(!is.nan(swat_calib_kge) & !is.na(swat_calib_kge)){
    val_gof <- swat_calib_kge
  }else{
    val_gof <- 999999
  }
  
  return(val_gof)
}

# Evolution Differentielle

# Premi?re ex?cution

calibDEoptim <- DEoptim::DEoptim(fn = OptimSWAT,
                                 lower = lower_vals, upper = upper_vals,
                                 control = DEoptim::DEoptim.control(NP = 500, trace = 10,VTR=-1,
                                                                    steptol=500,
                                                                   reltol = 10^-20,
                                                                    strategy = 6,
                                                                    itermax=5000))


out <- hydroPSO(fn="hydromodInR",
                lower=lower,
                upper=upper,
                method="spso2011",
                control=list(write2disk=TRUE, MinMax="max", npart=90,
                             maxit=5000, normalise=TRUE, REPORT=10, parallel="none",
                             reltol=1E-10),
                model.FUN="GR4Jhydromod.basic"
)
write.table(x = SWAT.PARAMETERS,file ="outputs/Cas1_NSE_SWAT_BEST_PARAMETERS.csv",sep = ",",row.names = FALSE)
write.table(x = SWAT.Qsims,file ="outputs/Cas1_NSE_SWAT_BEST_SIMULATIONS.csv",sep = ",",row.names = FALSE)



# Deuxi?me ex?cution

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

# Analyse des r?sultats de la calibration  
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

