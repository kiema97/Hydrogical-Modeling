SensTest2<- run_swat2012(
  project_path = project_path,
  output =define_output(file = "rch",
                        variable = "FLOW_OUT",
                        unit = 23),
  parameter = ParameterFast[1:4,-11],
  start_date = "1998-01-01",
  end_date = "2010-12-31",
  years_skip = 3,
  output_interval = "d", 
  keep_folder = F)


DATE <- as.Date(paste(Q_Obs_Data$ANNEES,Q_Obs_Data$MOIS,Q_Obs_Data$JOURS,sep = "-"))
Q_Obs_Data2<- data.frame(DATE,FlowOut=Q_Obs_Data$FlowOut2)


q_obs <- subset(Q_Obs_Data2,as.Date(Q_Obs_Data2$DATE)%in%as.Date(dip("2001-01-01","2010-12-31")))
q_sim <- subset(sim_data,as.Date(sim_data$date)%in%as.Date(q_obs$DATE))



#=====================OPTIMISATION=================================

# Extraction des param?tres sensibles

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

# Evolution Diff?rentielle

# Premi?re ex?cution

calibDEoptim <- DEoptim::DEoptim(fn = OptimSWATplus,
                                 lower = lower_vals, upper = upper_vals,
                                 control = DEoptim::DEoptim.control(NP = 500, trace = 10,VTR=-1,
                                                                    steptol=500,
                                                                    reltol = 10^-20,
                                                                    strategy = 6,
                                                                    itermax=5000))
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
