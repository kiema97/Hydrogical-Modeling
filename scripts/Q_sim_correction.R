#==================Correction des simulation hydrologiques===============
rm(list = ls())
#Libraries
library(ggplot2)
library(hyfo)
library(hydroGOF)
library(trend)
library(dplyr)
#Importation du jeux de donn?es
SimObsData <- readRDS("G:/PROJET/Article/Modelisation/Mouhoun/Output/ModelCalib/Data_validation_SWAT_GR6J.rds")
save_path <- "G:/PROJET/Article/Modelisation/Mouhoun/Output/ModelCalib/"
#Correction de bias
##M?thode Scaling
CorrectData_Scaling <- hyfo::biasCorrect(frc = SimObsData[,-4],hindcast =SimObsData[,-4] ,obs = SimObsData[,c(1,4)],method = "scaling")
NSE(sim = CorrectData_Scaling$SWAT,obs =SimObsData$obs )
CorrectData_Scaling <- transform(CorrectData_Scaling,Obs=SimObsData$obs,Methode="scaling")
##M?thode eqm
CorrectData_eqm <- hyfo::biasCorrect(frc = SimObsData[,-4],hindcast =SimObsData[,-4] ,obs = SimObsData[,c(1,4)],method = "eqm")
NSE(sim = CorrectData_eqm$SWAT,obs =SimObsData$obs )
CorrectData_eqm <- transform(CorrectData_eqm,Obs=SimObsData$obs,Methode="eqm")

##================================Analyse des r?sultats de la correction==========
CorrectedData <- rbind(CorrectData_Scaling,CorrectData_eqm)
#Formatage des donn?es de SWAT
SWATCorrectedData <- CorrectedData[,c(1,2,4,5)]
SWATCorrectedData <- transform(SWATCorrectedData[,-4],Sim=SimObsData$SWAT,Methode=SWATCorrectedData$Methode,Model="SWAT")
colnames(SWATCorrectedData) <- c("DATE","Corrige","Obs","Sim","Methode","Model")
SWATCorrectedData_AGG <- aggregate(x=SWATCorrectedData[,c(2,3,4)],
                                   by = list(paste0(substr(SWATCorrectedData$DATE,6,10),"_",SWATCorrectedData$Methode)),
                                   FUN = mean,na.rm=T)
SWATCorrectedData_AGG_01 <- transform(SWATCorrectedData_AGG[,-1],
                                      DATE=substr(x =SWATCorrectedData_AGG$Group.1,start = 1,stop = 5 ),
                                      Methode=factor(x = substr(x =SWATCorrectedData_AGG$Group.1,start = 7,stop = 20 ),
                                                     levels = c("scaling","eqm")),
                                      Model="SWAT")
SWATCorrectedData_AGG_01 <- SWATCorrectedData_AGG_01[order(SWATCorrectedData_AGG_01$Methode),]
SWATCorrectedData_AGG_01 <- transform(SWATCorrectedData_AGG_01,j_day=1:366)
SWATCorrectedData_02 <- melt(data = SWATCorrectedData_AGG_01,id.vars = c("DATE","Methode","Model","j_day"))
SWATCorrectedData_02 <- transform(SWATCorrectedData_02,
                                  variable=factor(x = SWATCorrectedData_02$variable,
                                                  levels = c("Obs","Sim","Corrige")))
##Plot
ggplot(data = SWATCorrectedData_02,aes(x =j_day,y =value ))+
  geom_line(aes(colour=variable),size=1.4)+
  facet_grid(facets = Model~Methode)+
  scale_colour_manual(name="",
                      values = c("black","red","green"),
                      labels=c("Observé","Simulé","Corrigé"))+
  scale_x_continuous(breaks =SWATCorrectedData_02$j_day[seq(1,366,by = 27)],
                     labels = SWATCorrectedData_02$DATE[seq(1,366,by = 27)] )+
  xlab("Jours")+
  ylab(expression(paste("Débits [",{m^3},{.s^-1},"]")))+
  theme_bw()+
  theme(legend.position = "top",
        strip.placement = "inside",
        strip.background = element_rect(fill = NA,
                                        colour = "black",
                                        size = 1),
        strip.text = element_text(face = "bold",
                                  colour = "black",size = 12),
        axis.text.x = element_text(face = "bold",
                                   size = 10,colour = "black",angle = 90),
        axis.text.y =element_text(face = "bold",
                                  size = 10,colour = "black",angle = 0),
        axis.title.x =element_text(face = "bold",
                                   size = 12,colour = "black",angle = 0),
        axis.title.y = element_text(face = "bold",
                                    size = 12,colour = "black",angle = 90))



#Formatage des donn?es de GR6J
GR6JCorrectedData <- CorrectedData[,c(1,3,4,5)]
GR6JCorrectedData <- transform(GR6JCorrectedData[,-4],Sim=SimObsData$GR6J,Methode=GR6JCorrectedData$Methode,Model="GR6J")
colnames(GR6JCorrectedData) <- c("DATE","Corrige","Obs","Sim","Methode","Model")
GR6JCorrectedData_AGG <- aggregate(x=GR6JCorrectedData[,c(2,3,4)],
                                   by = list(paste0(substr(GR6JCorrectedData$DATE,6,10),"_",GR6JCorrectedData$Methode)),
                                   FUN = mean,na.rm=T)

GR6JCorrectedData_AGG_01 <- transform(GR6JCorrectedData_AGG[,-1],
                                      DATE=substr(x =GR6JCorrectedData_AGG$Group.1,start = 1,stop = 5 ),
                                      Methode=factor(x = substr(x =GR6JCorrectedData_AGG$Group.1,start = 7,stop = 20 ),
                                                     levels = c("scaling","eqm")),
                                      Model="GR6J")

GR6JCorrectedData_AGG_01 <- GR6JCorrectedData_AGG_01[order(GR6JCorrectedData_AGG_01$Methode),]
GR6JCorrectedData_AGG_01 <- transform(GR6JCorrectedData_AGG_01,j_day=1:366)


GR6JCorrectedData_02 <- melt(data = GR6JCorrectedData_AGG_01,id.vars = c("DATE","Methode","Model","j_day"))
GR6JCorrectedData_02 <- transform(GR6JCorrectedData_02,
                                  variable=factor(x = GR6JCorrectedData_02$variable,
                                                  levels = c("Obs","Sim","Corrige")))

##Plot
ggplot(data = GR6JCorrectedData_02,aes(x =j_day,y =value ))+
  geom_line(aes(colour=variable),size=1.4)+
  facet_grid(facets = Model~Methode)+
  scale_colour_manual(name="",
                      values = c("black","red","green"),
                      labels=c("Observ?","Simul?","Corrig?"))+
  scale_x_continuous(breaks =SWATCorrectedData_02$j_day[seq(1,366,by = 27)],
                     labels = SWATCorrectedData_02$DATE[seq(1,366,by = 27)] )+
  xlab("Jours")+
  ylab(expression(paste("D?bits [",{m^3},{.s^-1},"]")))+
  theme_bw()+
  theme(legend.position = "top",
        strip.placement = "inside",
        strip.background = element_rect(fill = NA,
                                        colour = "black",
                                        size = 1),
        strip.text = element_text(face = "bold",
                                  colour = "black",size = 12),
        axis.text.x = element_text(face = "bold",
                                   size = 10,colour = "black",angle = 90),
        axis.text.y =element_text(face = "bold",
                                  size = 10,colour = "black",angle = 0),
        axis.title.x =element_text(face = "bold",
                                   size = 12,colour = "black",angle = 0),
        axis.title.y = element_text(face = "bold",
                                    size = 12,colour = "black",angle = 90))
###Faceting
ModelDataCorrected <- rbind(GR6JCorrectedData_02,SWATCorrectedData_02)

#signature graph
AnnotateText6 <- data.frame(variable=c(220,220,220,220),value=c(300,300,300,300),
                            lab=c("Réalisé par : \n KIEMA W. ARSENE",
                                  "Réalisé par : \n KIEMA W. ARSENE",
                                  "Réalisé par : \n KIEMA W. ARSENE",
                                  "Réalisé par : \n KIEMA W. ARSENE"),
                            Model=factor(x = c("GR6J","GR6J","SWAT","SWAT"),levels = c("GR6J","SWAT")),
                            Methode=factor(x=c("scaling","eqm","scaling","eqm"),levels = c("scaling","eqm")))


ggplot(data = ModelDataCorrected,aes(x =j_day,y =value ))+
  geom_text(data =AnnotateText6,mapping = aes(x=variable,y =value,label=lab ),alpha=0.5,size=4.5)+
  geom_line(aes(colour=variable),size=1.4)+
  facet_grid(facets = Model~Methode)+
  scale_colour_manual(name="",
                      values = c("black","red","green"),
                      labels=c("Observ?","Simul?","Corrig?"))+
  scale_x_continuous(breaks =SWATCorrectedData_02$j_day[seq(1,366,by = 27)],
                     labels = SWATCorrectedData_02$DATE[seq(1,366,by = 27)] )+
  xlab("Jours")+
  ylab(expression(paste("D?bits [",{m^3},{.s^-1},"]")))+
  theme_bw()+
  theme(legend.position = "top",
        strip.placement = "inside",
        strip.background = element_rect(fill = NA,
                                        colour = "black",
                                        size = 1),
        strip.text = element_text(face = "bold",
                                  colour = "black",size = 12),
        axis.text.x = element_text(face = "bold",
                                   size = 10,colour = "black",angle = 90),
        axis.text.y =element_text(face = "bold",
                                  size = 10,colour = "black",angle = 0),
        axis.title.x =element_text(face = "bold",
                                   size = 12,colour = "black",angle = 0),
        axis.title.y = element_text(face = "bold",
                                    size = 12,colour = "black",angle = 90))
#Calcul des crit?res de performance
save_path_02 <- "D:/MODELISATION/Mouhoun/Output/data/"
##SWAT
    #Scaling
SWATData <- CorrectedData[,c(2,4,5)]
SWATDataScaling <- subset(x = SWATData,Methode=="scaling")
scaling_crit <- hydroGOF::gof.data.frame(sim =SWATDataScaling$SWAT,obs = SWATDataScaling$Obs)
    #eqm

SWATDataeqm <- subset(x = SWATData,Methode=="eqm")
eqm_crit <- hydroGOF::gof.data.frame(sim =SWATDataeqm$SWAT,obs = SWATDataeqm$Obs)

SWATCrit <- data.frame(Parameters=rownames(scaling_crit),
                       scaling=scaling_crit[,1],
                       eqm=eqm_crit)
#write.table(x =SWATCrit,file = paste0(save_path_02,"SWAT_Correct_Criters.txt"),quote = F,sep = "\t",row.names = F )
##GR6J
GR6JData <- CorrectedData[,c(3,4,5)]
#Scaling
GR6JDataScaling <- subset(x = GR6JData,Methode=="scaling")
scaling_crit_GR6J <- hydroGOF::gof.data.frame(sim =GR6JDataScaling$GR6J,obs = GR6JDataScaling$Obs)
#eqm
GR6JDataeqm<- subset(x = GR6JData,Methode=="eqm")
eqm_crit_GR6J <- hydroGOF::gof.data.frame(sim =GR6JDataeqm$GR6J,obs = GR6JDataeqm$Obs)

GR6J_Crit <- data.frame(Parameters=rownames(scaling_crit_GR6J),
                        scaling=scaling_crit_GR6J[,1],
                        eqm=eqm_crit_GR6J[,1])
write.table(x =GR6J_Crit,file = paste0(save_path_02,"GR6J_Correct_Criters.txt"),quote = F,sep = "\t",row.names = F )


##==============Comparaison des distribution statistiques==================
##Formatage des donn?es du mod?le SWAT
CorrectedSWATData_BoxPlotData <- CorrectedData%>%
  select(SWAT,Obs,Methode)%>%
  rename(Sim=SWAT)%>%
  transform(Model="SWAT")%>%
  melt(id.vars=c("Methode","Model"))
##Formatage des donn?es du mod?le GR6J
CorrectedGR6JData_BoxPlotData <- CorrectedData%>%
  select(GR6J,Obs,Methode)%>%
  rename(Sim=GR6J)%>%
  transform(Model="GR6J")%>%
  melt(id.vars=c("Methode","Model"))
##Fusion des donn?es des mod?les
CorrectedData_BoxPlotData <- rbind(CorrectedGR6JData_BoxPlotData,
                                   CorrectedSWATData_BoxPlotData)%>%

transform(Methode=factor(x =unlist(select(.,Methode)),
                        levels =c("scaling","eqm")),
                        Model=factor(x = unlist(select(.,Model)),
                        levels = c("GR6J","SWAT")),
                        variable=factor(x =unlist(select(.,variable)),
                        levels = c("Obs","Sim")))

##BoxPlot
#CorrectData_Scaling,CorrectData_eqm
#wilcox <- wilcox.test(x = CorrectData_Scaling$SWAT,CorrectData_Scaling$Obs)
#wilcox$p.value
AnnotateText <- data.frame(variable=c(1.4,1.4,1.4,1.4),value=c(1170,1170,1170,1170),
                         lab=c("Test de Wilcoxon: \n p-value = 2.12e-30",
                               "Test de Wilcoxon: \n p-value = 0.99",
                               "Test de Wilcoxon: \n p-value = 1.106e-5",
                               "Test de Wilcoxon: \n p-value = 0.99"),
                         Model=factor(x = c("GR6J","GR6J","SWAT","SWAT"),levels = c("GR6J","SWAT")),
                         Methode=factor(x=c("scaling","eqm","scaling","eqm"),levels = c("scaling","eqm")))

AnnotateText2 <- data.frame(variable=c(1.4,1.4,1.4,1.4),value=c(300,300,300,300),
                           lab=c("R?alis? par : \n KIEMA W. ARSENE",
                                 "R?alis? par : \n KIEMA W. ARSENE",
                                 "R?alis? par : \n KIEMA W. ARSENE",
                                 "R?alis? par : \n KIEMA W. ARSENE"),
                           Model=factor(x = c("GR6J","GR6J","SWAT","SWAT"),levels = c("GR6J","SWAT")),
                           Methode=factor(x=c("scaling","eqm","scaling","eqm"),levels = c("scaling","eqm")))


par(mar=c(0,0,0,0))
ggplot(data = CorrectedData_BoxPlotData,mapping = aes(x = variable,y = value),group=Methode)+
  geom_boxplot(aes(fill=variable),outlier.colour = "black",notch = F,
               notchwidth = .2,varwidth = F,na.rm = T)+
  facet_grid(facets = Model~Methode,scales = "free")+
  scale_y_continuous(breaks = seq(0,1300,by = 200),limits = c(0,1300))+
  geom_text(data =AnnotateText,mapping = aes(x=variable,y =value,label=lab ))+
  geom_text(data =AnnotateText2,mapping = aes(x=variable,y =value,label=lab ),alpha=0.5,size=4.5)+
  xlab("Variables")+
  ylab(expression(paste("D?bits [",{m^3},"/s]")))+
  scale_fill_manual(name=" ",
                    values = c("blue","tomato"),
                    labels=c("Obs","Sim"))+
  theme_bw()+
  theme(legend.position = "top",
        strip.placement = "inside",
        strip.background = element_rect(fill = NA,
                                        colour = "black",
                                        size = 1),
        strip.text = element_text(face = "bold",
                                  colour = "black",size = 12),
        axis.text.x = element_text(face = "bold",
                                   size = 10,colour = "black",angle = 0),
        axis.text.y =element_text(face = "bold",
                                  size = 10,colour = "black",angle = 0),
        axis.title.x =element_text(face = "bold",
                                   size = 12,colour = "black",angle = 0),
        axis.title.y = element_text(face = "bold",
                                    size = 12,colour = "black",angle = 90))


##Visualisation des hydrogramme mensuel
##Aggregation des donn?es corrig?es par la m?thode eqm
CorrectMontData_eqm <- CorrectData_eqm%>%
  select(-c(Methode,DATE))%>%
  aggregate(by=list(substr(x=CorrectData_eqm$DATE,start = 6,stop =7 )),FUN=mean,na.rm=T,subset=c("SWAT","GR6J","Obs"))%>%
  transform(Methode="eqm")

##Aggregation des donn?es corrig?es par la m?thode scaling
CorrectMontData_scaling <- CorrectData_Scaling%>%
  select(-c(Methode,DATE))%>%
  aggregate(by=list(substr(x=CorrectData_eqm$DATE,start = 6,stop =7 )),FUN=mean,na.rm=T,subset=c("SWAT","GR6J","Obs"))%>%
  transform(Methode="scaling")
##Donnees SWAT
MonthCorrectedSWATData <- rbind(CorrectMontData_scaling,CorrectMontData_eqm)%>%
select(SWAT,Obs,Methode)%>%
  rename(Sim=SWAT)%>%
  melt(id.vars="Methode")%>%
  transform(Model="SWAT")

##Donnees GR6J
MonthCorrectedGR6JData <- rbind(CorrectMontData_scaling,CorrectMontData_eqm)%>%
  select(GR6J,Obs,Methode)%>%
  rename(Sim=GR6J)%>%
  melt(id.vars="Methode")%>%
  transform(Model="GR6J")

CorrectedMonthData <- rbind(MonthCorrectedGR6JData,MonthCorrectedSWATData)%>%
transform(Mois=1:12,
          Methode=factor(x =unlist(select(.,Methode)),levels = c("scaling","eqm") ),
          variable=factor(x =unlist(select(.,variable)),levels = c("Obs","Sim") ))


##Visualisation 
AnnotateText3 <- data.frame(variable=c(8,8,8,8),value=c(200,200,200,200),
                            lab=c("R?alis? par : \n KIEMA W. ARSENE",
                                  "R?alis? par : \n KIEMA W. ARSENE",
                                  "R?alis? par : \n KIEMA W. ARSENE",
                                  "R?alis? par : \n KIEMA W. ARSENE"),
                            Model=factor(x = c("GR6J","GR6J","SWAT","SWAT"),levels = c("GR6J","SWAT")),
                            Methode=factor(x=c("scaling","eqm","scaling","eqm"),levels = c("scaling","eqm")))




ggplot(data = CorrectedMonthData,aes(x =Mois,y =value ))+
  geom_text(data =AnnotateText3,mapping = aes(x=variable,y =value,label=lab ),alpha=0.5)+
  geom_line(aes(colour=variable),size=1.2)+
  facet_grid(facets = Model~Methode)+
  scale_x_continuous(breaks = 1:12,labels = month.abb)+
  scale_colour_manual(name="",
                      values = c("black","green"),
                      labels=c("Obs","Corrig?"))+
  xlab("Mois")+
  ylab(expression(paste("D?bits [",{m^3},{.s^-1},"]")))+
  theme_bw()+
  theme(legend.position = "top",
        strip.placement = "inside",
        strip.background = element_rect(fill = NA,
                                        colour = "black",
                                        size = 1),
        strip.text = element_text(face = "bold",
                                  colour = "black",size = 12),
        axis.text.x = element_text(face = "bold",
                                   size = 10,colour = "black",angle = 90),
        axis.text.y =element_text(face = "bold",
                                  size = 10,colour = "black",angle = 0),
        axis.title.x =element_text(face = "bold",
                                   size = 12,colour = "black",angle = 0),
        axis.title.y = element_text(face = "bold",
                                    size = 12,colour = "black",angle = 90))
