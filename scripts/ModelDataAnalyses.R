#Analyse des r?sultats de calage des mod?les
rm(list = ls())
#save.image(file = "D:/MODELISATION/Mouhoun/Output/ModelCalib/GlobalEnvData.RData")
#load("D:/MODELISATION/Mouhoun/Output/ModelCalib/GlobalEnvData.RData")
#Libraries
library(plotrix)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(hydroGOF)
#--------------------Visualisation des r?sulatts de la calibration------
calib_SWAT_GR6J_Data <- readRDS(file = "G:/PROJET/Article/Modelisation/Mouhoun/Output/ModelCalib/Data_calibrage_SWAT_GR6J.rds")
#Visualisation avec le diagramme de taylor

taylor.diagram(ref = calib_SWAT_GR6J_Data$obs,model = calib_SWAT_GR6J_Data$obs,add = F,
               col ="black",pos.cor = T,xlab = "Ecart-type",ylab = "Ecart-type",main = "Diagramme de Taylor",
               ngamma = 4,sd.arcs = T,ref.sd = T,pcex =1.5,gamma.col = "black",normalize = F,
               grad.corr.lines = c(seq(0.1,1,by = 0.1),0.95,0.99))
taylor.diagram(ref =calib_SWAT_GR6J_Data$obs,model = calib_SWAT_GR6J_Data$SWAT,add = T,col = "red",
               pch =17,pcex =1.5,normalize = F)
taylor.diagram(ref =calib_SWAT_GR6J_Data$obs,model = calib_SWAT_GR6J_Data$GR6J,add = T,col = "blue",
               pch = 15,pcex =1.5,normalize = F )
legend(x = 250,y = 255,legend =c("SWAT","GR6J","Ref"),col = c("red","blue","black"),pch = c(17,15,19),pt.cex = 1.5)


#Boxplot
GR6j_boxplotData <- calib_SWAT_GR6J_Data[,c(3,4)] 
GR6j_boxplotData <- transform(GR6j_boxplotData,
                              lab="GR6J")
colnames(GR6j_boxplotData) <- c("Sim","Obs","lab")
GR6j_boxplotData2 <- melt(data = GR6j_boxplotData,id.vars = "lab")

SWAT_boxplotData <- calib_SWAT_GR6J_Data[,c(2,4)]
SWAT_boxplotData <- transform(SWAT_boxplotData,
                              lab="SWAT")
colnames(SWAT_boxplotData) <- c("Sim","Obs","lab")
SWAT_boxplotData2 <- melt(data = SWAT_boxplotData,id.vars = "lab")


BoxPlotData <- rbind(SWAT_boxplotData2,GR6j_boxplotData2)
BoxPlotData <- transform(BoxPlotData,variable=factor(x = BoxPlotData$variable,
                                                     levels = c("Obs","Sim")),
                         label=factor(x =BoxPlotData$lab,levels =c("GR6J","SWAT")  ))

BoxPlotData_2 <- transform(BoxPlotData,lab2="Calibration")
BoxPlotData2 <- subset(BoxPlotData,subset = lab=="GR6J")

annot_text <- data.frame(variable=1.5,value=950,
                         lab="Test de Wilcoxon: P-value = 6.683683e-20",label=factor(x = "SWAT",levels = c("GR6J","SWAT")))
test_wilcoxon <- wilcox.test(x = calib_SWAT_GR6J_Data$obs,y = calib_SWAT_GR6J_Data$SWAT)
test_wilcoxon$p.value
annot_text <- data.frame(variable=c(1.2,1.2),value=c(975,975),
                         lab=c("Test de Wilcoxon: \n p-value = 6.68e-20",
                               "Test de Wilcoxon: \n p-value = 0.017"),
                         label=factor(x = c("GR6J","SWAT"),levels = c("GR6J","SWAT")))

#label=expression(paste("Test de Wilcoxon: \n p-value = 6.68",{10^-20},sep = ""))
par(mar=c(0,0,0,0))
ggplot(data = BoxPlotData,mapping = aes(x = variable,y = value),group=lab)+
  geom_boxplot(aes(fill=variable),outlier.colour = "black",notch = F,
               notchwidth = .2,varwidth = F,na.rm = T)+
  facet_grid(facets = .~label,scales = "free")+
  geom_text(data =annot_text,mapping = aes(x=variable,y =value,label=lab ))+

  scale_y_continuous(breaks = seq(0,1000,by = 200),limits = c(0,1000))+
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




# Analyse de la saisonnalit?
calib_SWAT_GR6J_Data
SaisonnalData_1 <- aggregate(x = calib_SWAT_GR6J_Data[,-1],
                             by = list(substr(calib_SWAT_GR6J_Data$DATE,6,10)),
                             FUN = mean,na.rm=T)
SaisonnalData_2 <- SaisonnalData_1
SaisonnalData_2 <-aggregate(x = SaisonnalData_1[,-1],
                            by = list(substr(SaisonnalData_1$Group.1,1,2)),
                            FUN = mean,na.rm=T) 
SaisonnalGR6J <-SaisonnalData_2[,c(3,4)]
colnames(SaisonnalGR6J) <- c("Sim","Obs")
SaisonnalGR6J2 <- transform(SaisonnalGR6J,label="GR6J")
SaisonnalGR6J_2 <- melt(data = SaisonnalGR6J2,id.vars = "label")

SaisonnalSWAT <-SaisonnalData_2[,c(2,4)]
colnames(SaisonnalSWAT) <- c("Sim","Obs")
SaisonnalSWAT2 <- transform(SaisonnalSWAT,label="SWAT")
SaisonnalSWAT_2 <- melt(data = SaisonnalSWAT2,id.vars = "label")

SaisonnalGGplotData <- rbind(SaisonnalGR6J_2,SaisonnalSWAT_2)
SaisonnalGGplotData2 <- transform(SaisonnalGGplotData,Mois=c(1:12),
                                  variable=factor(x =SaisonnalGGplotData$variable,
                                                  levels =c("Obs","Sim")  ))
jours_calandar <- SaisonnalData_2$Group.1
jours_julian<- SaisonnalGGplotData2$Mois

#scale_x_continuous(breaks =jours_julian[seq(1,366,by = 27)],
                   #labels = jours_calandar[seq(1,366,by = 27)] )+

ggplot(data = SaisonnalGGplotData2,aes(x =Mois,y =value ))+
  geom_line(aes(colour=variable),size=1.2)+
  facet_grid(facets = .~label)+
  coord_equal(ratio = .75)+
  scale_colour_manual(name="",
                      values = c("black","red"))+
  scale_x_continuous(breaks =1:12,
                     labels =month.abb)+
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
  




#============= Visualisation des r?sultats de la validation==========

valid_SWAT_GR6J_Data <- readRDS(file = "G:/PROJET/Article/Modelisation/Mouhoun/Output/ModelCalib/Data_validation_SWAT_GR6J.rds")

##--------- Visualisation avec le diagramme de Taylor----------------

taylor.diagram(ref = valid_SWAT_GR6J_Data$obs,model = valid_SWAT_GR6J_Data$obs,add = F,
               col ="black",pos.cor = T,xlab = "Ecart-type",ylab = "Ecart-type",main = "Diagramme de Taylor",
               ngamma = 4,sd.arcs = T,ref.sd = T,pcex =1.5,gamma.col = "black",normalize = F,
               grad.corr.lines = c(seq(0.1,1,by = 0.1),0.95,0.99))
taylor.diagram(ref =valid_SWAT_GR6J_Data$obs,model = valid_SWAT_GR6J_Data$SWAT,add = T,col = "red",
               pch =17,pcex =1.5,normalize = F)
taylor.diagram(ref =valid_SWAT_GR6J_Data$obs,model = valid_SWAT_GR6J_Data$GR6J,add = T,col = "blue",
               pch = 15,pcex =1.5,normalize = F )
legend(x = 260,y = 245,legend =c("SWAT","GR6J","Ref"),col = c("red","blue","black"),pch = c(17,15,19),pt.cex = 1.5)

##--------------------Visualisation au moyen de boxplot------------------------
### Fortage des donn?es au format ggplot
# Donn?es du mod?les GR6J
GR6j_val_boxplotData <- valid_SWAT_GR6J_Data[,c(3,4)]
GR6j_val_boxplotData <- transform(GR6j_val_boxplotData,lab=factor(x = "GR6J"))
colnames(GR6j_val_boxplotData) <- c("Sim","Obs","lab")
GR6j_val_boxplotData2 <- melt(data = GR6j_val_boxplotData,id.vars = "lab")

#Donn?es du mod?le SWAT
SWAT_val_boxplotData <- valid_SWAT_GR6J_Data[,c(2,4)]
SWAT_val_boxplotData <-transform(SWAT_val_boxplotData,lab=factor(x = "SWAT"))
colnames(SWAT_val_boxplotData) <- c("Sim","Obs","lab")
SWAT_val_boxplotData2 <- melt(data = SWAT_val_boxplotData,id.vars = "lab")


# Fusion des donn?es
val_BoxPlotData <- rbind(SWAT_val_boxplotData2,GR6j_val_boxplotData2)
val_BoxPlotData <- transform(val_BoxPlotData,variable=factor(x = val_BoxPlotData$variable,levels = c("Obs","Sim")),
                             lab=factor(x = val_BoxPlotData$lab,levels = c('GR6J','SWAT')))


test_wilcoxon_val <- wilcox.test(x = valid_SWAT_GR6J_Data$SWAT,y = valid_SWAT_GR6J_Data$obs)
test_wilcoxon_val$p.value
annot_text_val <- data.frame(variable=c(1.2,1.8),value=c(975,975),
                         label=c("Test de Wilcoxon: \n p-value = 4.26e-34",
                               "Test de Wilcoxon: \n p-value = 5.71e-4"),
                         lab=factor(x = c("GR6J","SWAT"),levels = c("GR6J","SWAT")))



ggplot(data = val_BoxPlotData,mapping = aes(x = variable,y = value),group=lab)+
  geom_boxplot(aes(fill=variable),outlier.colour = "red",notch = F,
               notchwidth = .2,varwidth = F,na.rm = T)+
  facet_grid(facets = .~lab,scales = "free")+
  geom_text(data =annot_text_val,mapping = aes(x=variable,y =value,label=label ))+
  scale_y_continuous(breaks = seq(0,1000,by = 200),limits = c(0,1000))+
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
## Visualisation de l'ensemble des r?sultats 
val_BoxPlotData_2 <- transform(val_BoxPlotData,lab2="Validation")
val_BoxPlotData2 <- subset(val_BoxPlotData,subset = lab=="GR6J")

annot_text <- data.frame(variable=c(1.2,1.2),value=c(975,975),
                         lab=c("Test de Wilcoxon: \n p-value = 6.68e-20",
                               "Test de Wilcoxon: \n p-value = 0.017"),
                         label=factor(x = c("GR6J","SWAT"),levels = c("GR6J","SWAT")))


test_wilcoxon_val <- wilcox.test(x = valid_SWAT_GR6J_Data$SWAT,y = valid_SWAT_GR6J_Data$obs)
test_wilcoxon_val$p.value


annot_text_val <- data.frame(x=c(1.5,1.6,1.5,1.6),y=c(930,930,930,930),
                             label=c("Test de Wilcoxon: \n p-value = 6.68e-20",
                                     "Test de Wilcoxon: \n p-value = 1.7e-2",
                                    "Test de Wilcoxon: \n p-value = 4.26e-34",
                                     "Test de Wilcoxon: \n p-value = 5.71e-4"),
                             lab=factor(x = c("GR6J","SWAT","GR6J","SWAT"),levels = c("GR6J","SWAT")),
                             lab2=factor(x = c("Calibration","Calibration","Validation","Validation"),levels = c("Calibration","Validation")))


#signature (filigrane)
AnnotateText5 <- data.frame(x=c(1.5,1.6,1.5,1.6),y=c(300,300,300,300),
                            label=c("R?alis? par : \n KIEMA W. ARSENE",
                                  "R?alis? par : \n KIEMA W. ARSENE",
                                  "R?alis? par : \n KIEMA W. ARSENE",
                                  "R?alis? par : \n KIEMA W. ARSENE"),
                            lab=factor(x = c("GR6J","GR6J","SWAT","SWAT"),levels = c("GR6J","SWAT")),
                            lab2=factor(x=c("Calibration","Validation","Calibration","Validation"),levels = c("Calibration","Validation")))


BoxPlotData_2$label=NULL

CalibVal <- rbind(BoxPlotData_2,val_BoxPlotData_2)


ggplot(data = CalibVal,mapping = aes(x = variable,y = value),group=lab)+
  geom_boxplot(aes(fill=variable),outlier.colour = "black",notch = F,
               notchwidth = .2,varwidth = F,na.rm = T)+
  scale_y_continuous(breaks = seq(0,1000,by = 200),limits = c(0,1000))+
  geom_text(data =annot_text_val,mapping = aes(x=x,y =y,label=label ))+
  facet_grid(facets = lab2~lab,scales = "free")+
  xlab("Variables")+
  ylab(expression(paste("D?bits [",{m^3},"/s]")))+
  scale_fill_manual(name=" ",
                    values = c("blue","tomato"),
                    labels=c("Obs","Sim"))+
  geom_text(data =AnnotateText5,mapping = aes(x=x,y =y,label=label ),alpha=0.3,size=4.5)+
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

## Analyse de la saisonnalit?
SaisonnalData_SWAT_1 <- aggregate(x = valid_SWAT_GR6J_Data[,-1],
                             by = list(substr(valid_SWAT_GR6J_Data$DATE,6,10)),
                             FUN = mean,na.rm=T)
SaisonnalData_SWAT_2 <- SaisonnalData_SWAT_1
SaisonnalData_SWAT_2 <-aggregate(x = SaisonnalData_SWAT_1[,-1],
                            by = list(substr(SaisonnalData_SWAT_1$Group.1,1,2)),
                            FUN = mean,na.rm=T) 
SaisonnalGR6J_val <-SaisonnalData_SWAT_2[,c(3,4)]
colnames(SaisonnalGR6J_val) <- c("Sim","Obs")
SaisonnalGR6J_val2 <- transform(SaisonnalGR6J_val,label="GR6J")
SaisonnalGR6J_val_2 <- melt(data = SaisonnalGR6J_val2,id.vars = "label")

SaisonnalSWAT_val <-SaisonnalData_SWAT_2[,c(2,4)]
colnames(SaisonnalSWAT_val) <- c("Sim","Obs")
SaisonnalSWAT_val2 <- transform(SaisonnalSWAT_val,label="SWAT")
SaisonnalSWAT_val_2 <- melt(data = SaisonnalSWAT_val2,id.vars = "label")

SaisonnalGGplotDataSWAT <- rbind(SaisonnalGR6J_val_2,SaisonnalSWAT_val_2)
SaisonnalGGplotDataSWAT2 <- transform(SaisonnalGGplotDataSWAT,Mois=c(1:12),
                                      variable=factor(x = SaisonnalGGplotDataSWAT$variable,
                                                        levels = c("Obs","Sim")))


ggplot(data = SaisonnalGGplotDataSWAT2,aes(x =Mois,y =value ))+
  geom_line(aes(colour=variable),size=1.2)+
  #coord_equal(ratio = .75)+
  facet_grid(facets = .~label)+
  #scale_x_continuous(breaks =jours_julian[seq(1,366,by = 27)],
                    # labels = jours_calandar[seq(1,366,by = 27)] )+
  scale_colour_manual(name="",
                      values = c("black","red"))+
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


## Visualisation de l'ensemble
SaisonnalGGplotData3 <- transform(SaisonnalGGplotData2,label2="Calibration")
SaisonnalGGplotDataSWAT3 <- transform(SaisonnalGGplotDataSWAT2,label2="Validation")
CalibValidSaisonnal <- rbind(SaisonnalGGplotData3,SaisonnalGGplotDataSWAT3)


#Signature des graphiques
AnnotateText7 <- data.frame(variable=c(8,8,8,8),value=c(200,200,200,200),
                            lab=c("R?alis? par : \n KIEMA W. ARSENE",
                                  "R?alis? par : \n KIEMA W. ARSENE",
                                  "R?alis? par : \n KIEMA W. ARSENE",
                                  "R?alis? par : \n KIEMA W. ARSENE"),
                            Model=factor(x = c("GR6J","GR6J","SWAT","SWAT"),levels = c("GR6J","SWAT")),
                            Methode=factor(x=c("scaling","eqm","scaling","eqm"),levels = c("scaling","eqm")))

#scale_x_continuous(breaks =SaisonnalGGplotDataSWAT2$Mois[seq(1,366,by = 31)],
                   #labels = SaisonnalData_SWAT_2$Group.1[seq(1,366,by = 31)] )+
  


ggplot(data = CalibValidSaisonnal,aes(x =Mois,y =value ))+
  geom_line(aes(colour=variable),size=1.2)+
  facet_grid(facets = label2~label)+
  scale_x_continuous(breaks =1:12,
                     labels = month.abb )+
  scale_colour_manual(name="",
                      values = c("black","red"))+
  geom_text(data =AnnotateText7,mapping = aes(x=variable,y =value,label=lab ),alpha=0.2,size=4.5)+
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


# Cacul des crit?res
#calibration
Model_SWAT_calib <- hydroGOF::gof(sim = calib_SWAT_GR6J_Data$SWAT,obs=calib_SWAT_GR6J_Data$obs)
Model_GR6J_calib <- hydroGOF::gof(sim = calib_SWAT_GR6J_Data$GR6J,obs=calib_SWAT_GR6J_Data$obs)


#validation
Model_SWAT_valid <- hydroGOF::gof(sim =valid_SWAT_GR6J_Data$SWAT ,obs=valid_SWAT_GR6J_Data$obs)
Model_GRJ6_valid <- hydroGOF::gof(sim =valid_SWAT_GR6J_Data$GR6J ,obs=valid_SWAT_GR6J_Data$obs)


CalibCriterions <- data.frame(Criters=row.names(Model_SWAT_calib),
                              SWAT.cal=Model_SWAT_calib[,1],
                              GR6J.cal=Model_GR6J_calib[,1],
                              SWAT.val=Model_SWAT_valid[,1],
                              GR6J.val=Model_GRJ6_valid[,1])

write.table(x = CalibCriterions,file = "D:/MODELISATION/Mouhoun/Output/Validation_Calibration/Criterions.txt",quote = F,sep = "\t",row.names = F)
