#############################################################################
#########       HYDROLOGICAL MODELING : SWAT                   ##############
#############################################################################

## NETTOYAGE
rm(list = ls())

## LIBRARIES
library(SWATplusR)
library(SWATrunR)
library(tidyverse)
library(DBI)
library(RSQLite)
library(sf)
library(fast)
library(forcats)
library(hydroGOF)
library(sensitivity)
library(stringr)
library(hydroPSO)
library(DEoptim)
## WORK SPACE
setwd("G:/PROJET/Article/Modelisation/Mouhoun")

## DATA BASE
DataBase <- dbConnect(drv = RSQLite::SQLite(),dbname=file.path("DataBase","DataBase.sqlite"))


## DATA IMPORTATION
Q_Obs_Data <- dbReadTable(conn = DataBase,name = "DapolaQOBS")%>%
  rename(FlowOut=Debit)%>%
  mutate(DATE= as.Date(DATE))

#Q_Obs_Data2 <- read.table(file = "DATA/ObservedData/ObsData.txt",header = T,sep = "\t")

## RUN SWAT MODEL
project_path <- "G:/PROJET/Article/Modelisation/Mouhoun/Projet/Scenarios/Default/TxtInOut"

modele_run <-SWATplusR::run_swat2012(
  project_path = project_path,
  output =define_output(file = "rch",
                        variable = "FLOW_OUT",
                        unit = 23),
  start_date = "2000-01-01",
  end_date = "2005-12-31",
  years_skip = 3)

## sensitivity Analysis
#1- Definition des parametres initiaux
source("RScript/swat_parameters.R")
lower <- as.numeric(SWATParameters[1,]) # Borne inf?rieures des param?tres
upper <- as.numeric(SWATParameters[2,])# Bornes superieures des param?tres

#2- Combinaison des param?tres
par_names <- str_remove(names(SWATParameters), '\\:\\:.*|\\..*')

ParameterFast <- fast_parameters(
  minimum =lower,
  maximum =upper,
  names =names(SWATParameters)) %>%
  as_tibble()

# Runing the model on the parameters sets
save_path <- "Projet/SWATplusR/Etude_sensibilite"

SWATRuns2<-SWATplusR::run_swat2012(#
    project_path = project_path,
    output =define_output(file = "rch",
                          variable = "FLOW_OUT",
                          unit = 23),
    parameter = ParameterFast,
    start_date = "1998-01-01",
    end_date = "2016-12-31",
    years_skip = 3,
    output_interval = "d", 
    keep_folder = FALSE,refresh = TRUE,n_thread = 4)

## Save data
output_path <- "G:/PROJET/Article/MODELISATION_II/SWAT/outputs"
saveRDS(object =SWATRuns2,file =file.path(output_path,"sensitivity.rds")) 
SWATRuns <- readRDS(file.path(output_path,"sensitivity.rds"))
Q_sim_data <- SWATRuns$simulation$FLOW_OUT

## EVALUATION OF SENSITIVITY ANALYSIS
Q_OBS_SIM <- Q_Obs_Data%>%
  inner_join(Q_sim_data,by=c("DATE"="date"))

nse_q <- Q_sim_data %>% 
  select(-date) %>% 
  map_dbl(., ~ NSE(.x, Q_OBS_SIM$FlowOut))

sensval_fast <- sensitivity(nse_q, 24,make.plot = TRUE,names = )

#Lecture des donn?es:
CalibSWATdata <- data.frame(Nash=nse_q,SWATRuns$parameter$values)%>%
  arrange(desc(Nash))

saveRDS(object =CalibSWATdata,file ="Output/ModelCalib/sensitivity_results.rds") # sauvegarde des donn?es de la simulation


## Plots
par_names <- SWATRuns$parameter$definition$par_name
ResultFast <- tibble(parameter = as.factor(par_names),
                     fast      = sensval_fast)%>%
  mutate(parameter = factor(parameter)%>% 
           fct_reorder(., fast))

## Sensivity plot
ggplot(data = ResultFast) +
  geom_col(aes(x = parameter, y = fast),fill="steelblue",color="black")+
  xlab(toupper("parameters")) +
  ylab(toupper("sensitivity"))+ 
  coord_flip(expand = T) +
  theme_bw()+
  theme(axis.text.x = element_text(colour = "black",size = 10),
        axis.title.x.bottom =element_text(colour = "black",face = "bold",size = 12),
        axis.title.y =element_text(colour = "black",face = "bold",size = 12),
        axis.text.y = element_text(colour = "black",size = 8))

## Plage de variation
CalibSWATdata2 <- CalibSWATdata%>%
  mutate(Nash_Lab=ifelse(Nash>=0,"positive","negative"))%>%
  gather(key = "parameters",value = "values",-Nash,-Nash_Lab)

Nash_Lab <- rep(c("positive","negative"),times=c(911,2644))
CalibSWATdata$Nash_Lab <- Nash_Lab
CalibSWATdata2 <-reshape2::melt(data = CalibSWATdata,id.vars = c("Nash","Nash_Lab"))
CalibSWATdata3 <- CalibSWATdata2[CalibSWATdata2$Nash>=-1,]

ggplot(data = CalibSWATdata2,aes(x=values,y=Nash))+
  geom_point()+
  facet_wrap(facets = .~parameters,scales = "free_x",as.table = F)+
  xlab("parameters")+
  ylab("Nash Sutcliffe")+
  theme_bw()+
  theme(strip.background = element_rect(fill = NA,colour = "blue",linewidth =1.5),
        strip.text = element_text(face = "italic",size = 10),
        axis.text.x = element_text(size = 9,angle = 90,face = "bold"))


