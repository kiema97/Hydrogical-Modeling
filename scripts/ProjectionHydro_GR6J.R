#***************************************************************************#
#               ELABORATION DE SCENARIOS HYDROLOGIQUES                         #
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
setwd("D:/PROJET/Article/Modelisation/Mouhoun")
## DATA BASE
clim_projection <- dbConnect(drv = RSQLite::SQLite(),dbname=file.path("DataBase","ClimProjectionDataBase.sqlite"))
##