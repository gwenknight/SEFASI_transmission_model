################################ plotting the effect of intervention from int.time ##############
#### Function to simulate impact of interventions
### INPUTS
## Params = parameters 
## returnout = 1/0 if want to see output as go
## intervention = which to run: from 1-19 
## input_country = which country in SEFASI trio

epid_intervention <- function(
    
  params, 
  returnout,
  intervention, 
  input_country){
  
  # params <- c(LAMBDA_H=LAMBDA_H,LAMBDA_A=LAMBDA_A,LAMBDA_E=LAMBDA_E,
  #             beta_HH=beta_HH,  beta_AA=beta_AA,  beta_HE=beta_HE,  beta_AH=beta_AH,
  #             beta_EH=beta_EH,  beta_HA=beta_HA,  beta_EA=beta_EA,  beta_AE=beta_AE, 
  #             beta_EE=beta_EE,  mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,epsilon=epsilon)
  
  # Run the deterministic model for this set of parameters 
  if (input_country=="denmark"){
    out <- as.data.frame(ode(y=state,time=vectTime,func=AMRmodel_DENMARK,parms=params))
  } else if  (input_country=="england"){
    out <- as.data.frame(ode(y=state,time=vectTime,func=AMRmodel_ENGLAND,parms=params))
  } else{
    out <- as.data.frame(ode(y=state,time=vectTime,func=AMRmodel_SENEGAL,parms=params))
  }
  out$time = out$time+epid.start #rescale the time so that it runs from 2005 onwards 
  
  # New initial time
  state2<-c(H=out$H[out$time==int.time],A=out$A[out$time==int.time],E=out$E[out$time==int.time])
  if (intervention==1){ # 
    params2 <- params
    params2[,"LAMBDA_H"] <- 0
    } else if (intervention==2){ #
    params2 <- params
    params2[,"LAMBDA_A"] <- 0
  } else if (intervention==3) {
    params2 <- params
    params2[,"LAMBDA_E"] <- 0
  } else if (intervention==4){
    params2 <- params
    params2[,"beta_HH"] <- 0
  }else if (intervention==5){
    params2 <- params
    params2[,"beta_AA"] <- 0
  } else if (intervention==6){
    params2 <- params
    params2[,"beta_EE"] <- 0
  } else if (intervention==7){
    params2 <- params
    params2[,"beta_AH"] <- 0
    params2[,"beta_HA"] <- 0
  } else if (intervention==8){
    params2 <- params
    params2[,"beta_EH"] <- 0
  }else if (intervention==9){
    params2 <- params
    params2[,"beta_EH"] <- 0
    params2[,"beta_HE"] <- 0
  }else if (intervention==10){
    params2 <- params
    params2[,"beta_EA"] <- 0
    params2[,"beta_AE"] <- 0
  }else if (intervention==11){
    params2 <- params
    params2[,"beta_EA"] <- 0
  }else if (intervention==12){
    #### New ones - above just variations on single parameters 
    #DENMARK NAP 
    params2 <- params
    params2[,"LAMBDA_H"] <- params[,"LAMBDA_H"]*0.9
    params2[,"LAMBDA_A"] <- params[,"LAMBDA_A"]*0.8
    params2[,"beta_HH"] <- params[,"beta_HH"]*0.8
    params2[,"beta_HA"] <- params[,"beta_HA"]*0.8
    params2[,"beta_AA"] <- params[,"beta_AA"]*0.8
    params2[,"beta_AH"] <- params[,"beta_AH"]*0.8
    params2[,"beta_EE"] <- params[,"beta_EE"]*0.8
    params2[,"beta_EH"] <- params[,"beta_EH"]*0.8
    params2[,"beta_HE"] <- params[,"beta_HE"]*0.8
    params2[,"beta_AE"] <- params[,"beta_AE"]*0.8
    params2[,"beta_EA"] <- params[,"beta_EA"]*0.8
  } else if (intervention==13){ # ENGLAND NAP
    params2 <- params
    params2[,"LAMBDA_H"] <- params[,"LAMBDA_H"]*0.85
    params2[,"LAMBDA_A"] <- params[,"LAMBDA_A"]*0.75
    params2[,"beta_HH"] <- params[,"beta_HH"]*0.8
    params2[,"beta_HA"] <- params[,"beta_HA"]*0.8
    params2[,"beta_AA"] <- params[,"beta_AA"]*0.8
    params2[,"beta_AH"] <- params[,"beta_AH"]*0.8
    params2[,"beta_EE"] <- params[,"beta_EE"]*0.8
    params2[,"beta_EH"] <- params[,"beta_EH"]*0.8
    params2[,"beta_HE"] <- params[,"beta_HE"]*0.8
    params2[,"beta_AE"] <- params[,"beta_AE"]*0.8
    params2[,"beta_EA"] <- params[,"beta_EA"]*0.8
  } else if (intervention==14){ #SENEGAL NAP 
    params2 <- params
    params2[,"LAMBDA_H"] <- params[,"LAMBDA_H"]*0.95
    params2[,"LAMBDA_A"] <- params[,"LAMBDA_A"]*0.8
    params2[,"beta_HH"] <- params[,"beta_HH"]*0.9
    params2[,"beta_HA"] <- params[,"beta_HA"]*0.9
    params2[,"beta_AA"] <- params[,"beta_AA"]*0.9
    params2[,"beta_AH"] <- params[,"beta_AH"]*0.9
    params2[,"beta_EE"] <- params[,"beta_EE"]*0.9
    params2[,"beta_EH"] <- params[,"beta_EH"]*0.9
    params2[,"beta_HE"] <- params[,"beta_HE"]*0.9
    params2[,"beta_AE"] <- params[,"beta_AE"]*0.9
    params2[,"beta_EA"] <- params[,"beta_EA"]*0.9
  } else if (intervention==15){ # FARM 
    params2 <- params
    params2[,"LAMBDA_A"] <- params[,"LAMBDA_A"]*0.5
    params2[,"beta_HA"] <- params[,"beta_HA"]*0.5
    params2[,"beta_AH"] <- params[,"beta_AH"]*0.5
  }else if (intervention==16){ # HUMAN
    params2 <- params
    params2[,"LAMBDA_H"] <- params[,"LAMBDA_H"]*0.5
    params2[,"beta_HH"] <- params[,"beta_HH"]*0.5
  }else if (intervention==17){ # ENV
    params2 <- params
    params2[,"LAMBDA_E"] <- params[,"LAMBDA_E"]*0.5
    params2[,"beta_EE"] <- params[,"beta_EE"]*0.5
    params2[,"beta_EH"] <- params[,"beta_EH"]*0.5
    params2[,"beta_HE"] <- params[,"beta_HE"]*0.5
    params2[,"beta_AE"] <- params[,"beta_AE"]*0.5
    params2[,"beta_EA"] <- params[,"beta_EA"]*0.5
  }else if (intervention==18){ # HUMAN + ANIMAL CONTACT
    params2 <- params
    params2[,"beta_HA"] <- params[,"beta_HA"]*0.5
    params2[,"beta_AH"] <- params[,"beta_AH"]*0.5
  }else if (intervention==19){ # TRANSMISSION
    params2 <- params
    params2[,"beta_HH"] <- params[,"beta_HH"]*0.5
    params2[,"beta_HA"] <- params[,"beta_HA"]*0.5
    params2[,"beta_AA"] <- params[,"beta_AA"]*0.5
    params2[,"beta_AH"] <- params[,"beta_AH"]*0.5
    params2[,"beta_EE"] <- params[,"beta_EE"]*0.5
    params2[,"beta_EH"] <- params[,"beta_EH"]*0.5
    params2[,"beta_HE"] <- params[,"beta_HE"]*0.5
    params2[,"beta_AE"] <- params[,"beta_AE"]*0.5
    params2[,"beta_EA"] <- params[,"beta_EA"]*0.5
  }else if (intervention==20){ # USAGE
    params2 <- params
    params2[,"LAMBDA_H"] <- params[,"LAMBDA_H"]*0.5
    params2[,"LAMBDA_A"] <- params[,"LAMBDA_A"]*0.5
    params2[,"LAMBDA_E"] <- params[,"LAMBDA_E"]*0.5
>>>>>>> b644649050f2a34241d46fa6d31f48ffea7324c5
  } else {params2 <- 0}
  
  
  vectTime2 <- c( (int.time - epid.start):epid.duration)
  if (input_country=="denmark"){
    out2 <- as.data.frame(ode(y=state2,time=vectTime2,func=AMRmodel_DENMARK,parms=params2))
  } else if  (input_country=="england"){
    out2 <- as.data.frame(ode(y=state2,time=vectTime2,func=AMRmodel_ENGLAND,parms=params2))
  } else{
    out2 <- as.data.frame(ode(y=state2,time=vectTime2,func=AMRmodel_SENEGAL,parms=params2))
  }
  
  
  out2$time = out2$time+epid.start #rescale the time so that it runs from 2005 onwards 
  
  modelend.time.H <- out$H[out$time==end.time]
  modelend.time.A <- out$A[out$time==end.time]
  modelend.time.E <- out$E[out$time==end.time] 
  
  modelend.time.H.int <- out2$H[out2$time==end.time]
  modelend.time.A.int <- out2$A[out2$time==end.time]
  modelend.time.E.int <- out2$E[out2$time==end.time] 
  
  # Explore percentage impact
  differenceH_percent <-   (modelend.time.H -  modelend.time.H.int)/modelend.time.H
  differenceA_percent <-   (modelend.time.A -  modelend.time.A.int)/modelend.time.A
  differenceE_percent <-   (modelend.time.E -  modelend.time.E.int)/modelend.time.E
  
  differenceH <-   (modelend.time.H -  modelend.time.H.int)
  differenceA <-   (modelend.time.A -  modelend.time.A.int)
  differenceE <-   (modelend.time.E -  modelend.time.E.int)
  
  # Outputs
  summary <- c(modelend.time.H= modelend.time.H,
    modelend.time.A= modelend.time.A,
    modelend.time.E= modelend.time.E,
    modelend.time.H.int= modelend.time.H.int,
    modelend.time.A.int= modelend.time.A.int,
    modelend.time.E.int= modelend.time.E.int,
    differenceH =  differenceH,
    differenceA =  differenceA,
    differenceE =  differenceE,
    differenceH_percent=differenceH_percent,
    differenceA_percent=differenceA_percent,
    differenceE_percent=differenceE_percent
  )
  
  ### Choose what output
  if (returnout ==1){return(list(out2 = out2,summary = summary))} else{return(summary)}
}
