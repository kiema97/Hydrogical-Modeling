#=======================TRAITEMENT DES DONNEES=================
## NETOYAGE
rm(list = ls())
## LIBRARIES
library(tidyverse)
library(readxl)
library(reshape2)
library(hydroTSM)
library(plyr)

## WORK SPACE
setwd("G:/PROJET/Article/Modelisation/Mouhoun/DATA/ObservedData")

## DATA BASE
Qjr_Dapola <- read_excel("Qjr_Dapola.xls")

#-2 Formatage des donn?es
colnames(Qjr_Dapola) <- c("DATE","FlowOut")
attach(Qjr_Dapola)
ANNEES <- format(as.Date(DATE),"%Y")
MOIS <- format(as.Date(DATE),"%m")
JOURS <- format(as.Date(DATE),"%d")
QjrDapola <- data.frame(ANNEES,MOIS,JOURS,FlowOut)
QjrDapola <- QjrDapola[order(QjrDapola$MOIS),]

    #-3 Calcul du pourcentage des lacunes
MissDataPercentage <- function(x){
  prtage <- sum(is.na(x))/length(x)*100
  prtage
}
steptime <- paste(QjrDapola$ANNEES,QjrDapola$MOIS,sep = "-")

QjrDapolaLacune <- aggregate(QjrDapola$FlowOut,by=list(steptime),
                             MissDataPercentage)

QjrDapolaLacune_2 <- data.frame(ANNEES=substr(QjrDapolaLacune$Group.1,start = 1,stop = 4),
                                MOIS=substr(QjrDapolaLacune$Group.1,start = 6,stop = 7),
                                LacPrtage=QjrDapolaLacune$x)
QjrDapolaLacune_2$Lacunes <- "LACUNES"
    #-4 Elaboration de graphs
par(mar=c(0,0,0,0))

ggplot(QjrDapolaLacune_2, aes(ANNEES, MOIS),group=ANNEES) +
  geom_raster(aes(fill =LacPrtage))+
  #facet_grid(facets = .~Lacunes)+
  scale_fill_gradient(low = "#999999",
                      high = "#000000")+

  coord_equal(ratio = 2.3,expand = T,xlim = c(.5,66.5),ylim = c(.8,12.2))+
  scale_x_discrete(breaks=unique(ANNEES)[seq(1,66,2)],labels=unique(ANNEES)[seq(1,66,2)])+
  scale_y_discrete(breaks=unique(MOIS),labels=unique(MOIS))+
  theme_bw()+
  xlab("ANNEES")+
  ylab("MOIS")+
  theme(axis.text.x =element_text(angle = 90,face = "bold",colour = "black"),
        axis.text.y = element_text(face = "bold",colour = "black"),
        axis.title.x = element_text(face = "bold",colour = "black",size = 12),
        axis.title.y = element_text(face = "bold",colour = "black",size = 12),
        legend.text.align =0,
        legend.margin = margin(-18,0,0,-6),
        legend.key.height = unit(0.8,"cm"),
        legend.title = element_text(face = "bold",colour = "black",hjust = 0.5,vjust = 0.5),
        legend.text = element_text(face = "bold",colour = "black"))+
  guides(fill = guide_legend(title="(%)",reverse = T,title.hjust = 0))


    #-5 D?finition des p?riodes de calage et de validation

        # Calcul des indices standardis?s
Qjr_Dapola <- transform(Qjr_Dapola,DATE=as.Date(DATE))
Qjr_Dapola <- subset(Qjr_Dapola,as.Date(Qjr_Dapola$DATE)<as.Date("2020-01-01"))
Modules <- hydroTSM::daily2annual.data.frame(x = Qjr_Dapola,FUN =mean,na.rm = T,dates = 1,out.type = "data.frame")

IndexStandard <- function(x){
  Index. <- (x-mean(x,na.rm = T))/sd(x,na.rm = T)
  Index.
}
ModulesIndex <- as.vector(IndexStandard(Modules))
ModulesIndex_2 <- data.frame(ANNEES=unique(format(Qjr_Dapola$DATE,"%Y")),Index=ModulesIndex)
          #plot indices standardis?s
ModulesIndex_2 <- ModulesIndex_2[!is.na(ModulesIndex_2$Index),]

RepModuleIndex<- ggplot(ModulesIndex_2,mapping = aes(x=1:64,y=Index ))+
  geom_bar( stat="identity",aes(1:64,Index ), fill="steelblue",colour="black")+
  scale_x_continuous(breaks = seq(1,64,by=4),labels = ModulesIndex_2$ANNEES[seq(1,64,by=4)])+
  xlab("Ann?es") +
  ylab("Indices Centr?s et R?duits")

RepModuleIndex+
  coord_equal(ratio = 6,expand = F,xlim = c(0,65),ylim = c(-1.6,3.7))+
  theme_bw()+
  geom_hline(yintercept = 0, colour= "red")+
  theme(axis.text.x = element_text(colour = "black", angle = 90,size = 10,face = "bold",hjust = 0.5),
        axis.title.x = element_text(colour = "black",size = 12,face = "bold",hjust = 0.5),
        axis.text.y =element_text(colour = "black",size = 10,face = "bold",hjust = 0.5),
        axis.title.y = element_text(colour = "black",size = 12,face = "bold",hjust = 0.5))


        #- Indice centr?s r?duits

HanningFilter <- function(data,type="vector"){
  if(type=="vector"){
    x=data[!is.na(data)]
  }
  if(type=="data.frame"){
    x <- data[!is.na(data[,2]),2]
  }
  n <- length(x)
  filtre_1 <- .54*x[1]+.46*x[2]
  filtre_2 <- .25*x[1]+.5*x[2]+.25*x[3]
  filtre_3 <- c()
  for (i in 3:(n-2)) {
    filtre_ <- .06*x[c(i-2)]+.25*x[c(i-1)]+.38*x[i]+.25*x[c(i+1)+.06*x[c(i+2)]]
    filtre_3 <- c(filtre_3,filtre_)
  }
  filtre_n_1 <- .25*x[c(n-2)]+.5*x[c(n-1)]+0.25*x[n]
  filtre_n <-.54*x[n]+.46*x[c(n-1)]
  IndexFilter <- round(c(filtre_1,filtre_2,filtre_3,filtre_n_1,filtre_n),2)
  if(type=="data.frame"){
    IndexFilter <- data.frame(DATE=data[!is.na(data[,2]),1],Index=IndexFilter)
  }
  IndexFilter
}

ModulesIndexFilter <- HanningFilter(data = ModulesIndex_2,type = "data.frame")

          #Plots
RepModuleIndexFilter<- ggplot(ModulesIndexFilter,mapping = aes(x=1:64,y=Index ))+
  geom_bar( stat="identity",aes(1:64,Index ), fill="steelblue",colour="black")+
  scale_x_continuous(breaks = seq(1,64,by=4),labels = ModulesIndexFilter$DATE[seq(1,64,by=4)])+
  xlab("Ann?es") +
  ylab("Indices Pond?r?s et R?duits")

RepModuleIndexFilter+
  coord_equal(ratio = 10,xlim = c(-1,66),expand = F,ylim = c(-1.3,1.9))+
  theme_bw()+
  geom_hline(yintercept = 0, colour= "red")+
  theme(axis.text.x = element_text(colour = "black", angle = 90,size = 10,face = "bold",hjust = 0.5),
        axis.title.x = element_text(colour = "black",size = 12,face = "bold",hjust = 0.5),
        axis.text.y =element_text(colour = "black",size = 10,face = "bold",hjust = 0.5),
        axis.title.y = element_text(colour = "black",size = 12,face = "bold",hjust = 0.5))


#============== II. D?finition des p?riodes de calage et de validation=======================
DataCalib <- subset(QjrDapola,as.numeric(as.vector(QjrDapola$ANNEES))<=2019 & as.numeric(as.vector(QjrDapola$ANNEES))>=2010)
DataCalib2 <- DataCalib[!is.na(DataCalib$FlowOut),]

write.table(DataCalib2,file = "CalibData.txt",sep = "\t",row.names = F,quote = F)
