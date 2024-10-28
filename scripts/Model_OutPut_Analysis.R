########################################################################
########        ANALYSE DES SIMULATIONS HYDROLOGIQUES       ############
########################################################################

## NETOYAGE
rm(list = ls())
## LIBRARIES
library(tidyverse)
library(lubridate)
library(plotrix)
library(reshape2)
library(ggpubr)
library(hydroGOF)

## WORK SPACE
setwd(dir = "G:/PROJET/Article/Modelisation/SousArticles")

## DATA BASE
DataBase <- dbConnect(drv = RSQLite::SQLite(),dbname=file.path("data","DataBase.sqlite"))

## DATA IMPORTATION
SIM_SWAT_GR6J <- dbReadTable(conn = DataBase,name = "DATA_CALIB_VALID_SWAT_GR6J")

SIM_SWAT_GR6J_DD <- SIM_SWAT_GR6J%>%
  filter(Operation=="Validation")
    mutate(DATE=as_date(DATE),
           YYYY=year(DATE),
           MM=month(DATE),
           DD=yday(DATE))%>%
    dplyr::select(DATE,YYYY,MM,DD,Operation,everything())%>%
    group_by(DD,Operation)%>%
    dplyr::summarize(across(SWAT:obs,~round(mean(.x,na.rm = TRUE),2)))%>%
  gather(key = "MODEL",value = "SIM",SWAT,GR6J)%>%
  gather(key = "VARIABLE",value = "Value",SIM,obs)

colnames(SIM_SWAT_GR6J_DD) <- toupper(colnames(SIM_SWAT_GR6J_DD))

RColorBrewer::display.brewer.all()
RColorBrewer::brewer.pal(9,"Greys")

## ANALYSE DES HYDROGRAMMES
par(mar=c(0,0,0,0))
ggplot(data = SIM_SWAT_GR6J_DD,mapping = aes(x = DD,y = VALUE,color=VARIABLE))+
  geom_line(size=.8)+
  scale_x_continuous(breaks = seq(1,366,35))+
  facet_grid(facets = MODEL~OPERATION,scales = "free")+
  xlab("Jours")+
  ylab(expression(paste("DEBIT [",{m^3},"/s]")))+
  scale_color_manual(name=" ",
                    values = c("black","red"),
                    labels=c("Observation","Simulation"))+
  theme_bw()+
  theme(legend.position = "top",
        strip.placement = "inside",
        strip.background = element_rect(fill = "NA",
                                        colour = "black",
                                        size = 1),
        strip.text = element_text(face = "bold",
                                  colour = "black",size = 12),
        panel.background = element_rect(fill = "#FFFFFF",
                                        size = 1),
        axis.text.x = element_text(face = "bold",
                                   size = 10,colour = "black",angle = 90),
        axis.text.y =element_text(face = "bold",
                                  size = 10,colour = "black",angle = 0),
        axis.title.x =element_text(face = "bold",
                                   size = 12,colour = "black",angle = 0),
        axis.title.y = element_text(face = "bold",
                                    size = 12,colour = "black",angle = 90))


## QQ-PLOT

SIM_SWAT_GR6J_DD1 <- SIM_SWAT_GR6J%>%
  dplyr::select(Operation,everything())%>%
  dplyr::select(-DATE)%>%
  gather(key = "MODEL",value = "SIM",SWAT,GR6J)
colnames(SIM_SWAT_GR6J_DD1) <- toupper(colnames(SIM_SWAT_GR6J_DD1))

ggplot(data = SIM_SWAT_GR6J_DD1, aes(x = OBS,y = SIM))+
  geom_point(alpha=.2)+
  geom_abline(slope =1 ,intercept = 0)+
  facet_grid(facets = MODEL~OPERATION)+
  tune::coord_obs_pred()+
  xlab(expression(paste("DEBIT OBSERVE [",{M^3},"/S]")))+
  ylab(expression(paste("DEBIT SIMULE [",{M^3},"/S]")))+
  theme_bw()
  theme(legend.position = "top",
        strip.placement = "inside",
        strip.background = element_rect(fill = "NA",
                                        colour = "black",
                                        size = 1),
        strip.text = element_text(face = "bold",
                                  colour = "black",size = 12),
        panel.background = element_rect(fill = "#FFFFFF",
                                        size = 1),
        axis.text.x = element_text(face = "bold",
                                   size = 10,colour = "black",angle = 90),
        axis.text.y =element_text(face = "bold",
                                  size = 10,colour = "black",angle = 0),
        axis.title.x =element_text(face = "bold",
                                   size = 12,colour = "black",angle = 0),
        axis.title.y = element_text(face = "bold",
                                    size = 12,colour = "black",angle = 90))


