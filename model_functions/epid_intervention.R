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
    params2 <- c(LAMBDA_H=0,
                 LAMBDA_A=LAMBDA_A, 
                 LAMBDA_E=LAMBDA_E,
                 beta_HH=beta_HH,  beta_AA=beta_AA,  beta_HE=beta_HE,  beta_AH=beta_AH,
                 beta_EH=beta_EH,  beta_HA=beta_HA,  beta_EA=beta_EA,  beta_AE=beta_AE,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE,epsilon=epsilon)}
  else if (intervention==2){ #
    params2 <- c(LAMBDA_H=LAMBDA_H,
                 LAMBDA_A=0, 
                 LAMBDA_E=LAMBDA_E,
                 beta_HH=beta_HH,  beta_AA=beta_AA,  beta_HE=beta_HE,  beta_AH=beta_AH,
                 beta_EH=beta_EH,  beta_HA=beta_HA,  beta_EA=beta_EA,  beta_AE=beta_AE,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE,epsilon=epsilon)
  } else if (intervention==3) {
    params2 <- c(LAMBDA_H=LAMBDA_H,
                 LAMBDA_A=LAMBDA_A, 
                 LAMBDA_E=0,
                 beta_HH=beta_HH,  beta_AA=beta_AA,  beta_HE=beta_HE,  beta_AH=beta_AH,
                 beta_EH=beta_EH,  beta_HA=beta_HA,  beta_EA=beta_EA,  beta_AE=beta_AE,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE,epsilon=epsilon)
  } else if (intervention==4){
    params2 <- c(LAMBDA_H=LAMBDA_H,
                 LAMBDA_A=LAMBDA_A, 
                 LAMBDA_E=LAMBDA_E,
                 beta_HH=0,  beta_AA=beta_AA,  beta_HE=beta_HE,  beta_AH=beta_AH,
                 beta_EH=beta_EH,  beta_HA=beta_HA,  beta_EA=beta_EA,  beta_AE=beta_AE,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE,epsilon=epsilon)
  }else if (intervention==5){
    params2 <- c(LAMBDA_H=LAMBDA_H,
                 LAMBDA_A=LAMBDA_A, 
                 LAMBDA_E=LAMBDA_E,
                 beta_HH= beta_HH,  beta_AA=0,  beta_HE=beta_HE,  beta_AH=beta_AH,
                 beta_EH=beta_EH,  beta_HA=beta_HA,  beta_EA=beta_EA,  beta_AE=beta_AE,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE,epsilon=epsilon)
  } else if (intervention==6){
    params2 <- c(LAMBDA_H=LAMBDA_H,
                 LAMBDA_A=LAMBDA_A, 
                 LAMBDA_E=LAMBDA_E,
                 beta_HH= beta_HH,  beta_AA=beta_AA,  beta_HE=0,  beta_AH=beta_AH,
                 beta_EH=beta_EH,  beta_HA=beta_HA,  beta_EA=beta_EA,  beta_AE=beta_AE,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE,epsilon=epsilon)
  } else if (intervention==7){
    params2 <- c(LAMBDA_H=LAMBDA_H,
                 LAMBDA_A=LAMBDA_A, 
                 LAMBDA_E=LAMBDA_E,
                 beta_HH= beta_HH,  beta_AA=beta_AA,  beta_HE=beta_HE,  beta_AH=0,
                 beta_EH=beta_EH,  beta_HA=beta_HA,  beta_EA=beta_EA,  beta_AE=beta_AE,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE,epsilon=epsilon)
  } else if (intervention==8){
    params2 <- c(LAMBDA_H=LAMBDA_H,
                 LAMBDA_A=LAMBDA_A, 
                 LAMBDA_E=LAMBDA_E,
                 beta_HH= beta_HH,  beta_AA=beta_AA,  beta_HE=beta_HE,  beta_AH=beta_AH,
                 beta_EH=0,  beta_HA=beta_HA,  beta_EA=beta_EA,  beta_AE=beta_AE,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE,epsilon=epsilon)
  }else if (intervention==9){
    params2 <- c(LAMBDA_H=LAMBDA_H,
                 LAMBDA_A=LAMBDA_A, 
                 LAMBDA_E=LAMBDA_E,
                 beta_HH= beta_HH,  beta_AA=beta_AA,  beta_HE=beta_HE,  beta_AH=beta_AH,
                 beta_EH=beta_EH,  beta_HA=0,  beta_EA=beta_EA,  beta_AE=beta_AE,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE,epsilon=epsilon)
  }else if (intervention==10){
    params2 <- c(LAMBDA_H=LAMBDA_H,
                 LAMBDA_A=LAMBDA_A, 
                 LAMBDA_E=LAMBDA_E,
                 beta_HH= beta_HH,  beta_AA=beta_AA,  beta_HE=beta_HE,  beta_AH=beta_AH,
                 beta_EH=beta_EH,  beta_HA=beta_HA,  beta_EA=0,  beta_AE=beta_AE,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE,epsilon=epsilon)
  }else if (intervention==11){
    params2 <- c(LAMBDA_H=LAMBDA_H,
                 LAMBDA_A=LAMBDA_A, 
                 LAMBDA_E=LAMBDA_E,
                 beta_HH= beta_HH,  beta_AA=beta_AA,  beta_HE=beta_HE,  beta_AH=beta_AH,
                 beta_EH=beta_EH,  beta_HA=beta_HA,  beta_EA=beta_EA,  beta_AE=0,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE,epsilon=epsilon)
  }else if (intervention==12){
    params2 <- c(LAMBDA_H=LAMBDA_H,
                 LAMBDA_A=LAMBDA_A, 
                 LAMBDA_E=LAMBDA_E,
                 beta_HH= beta_HH,  beta_AA=beta_AA,  beta_HE=beta_HE,  beta_AH=beta_AH,
                 beta_EH=beta_EH,  beta_HA=beta_HA,  beta_EA=beta_EA,  beta_AE=beta_AE,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=0,epsilon=epsilon)
  }else if (intervention==13){
    params2 <- c(LAMBDA_H=LAMBDA_H,
                 LAMBDA_A=LAMBDA_A, 
                 LAMBDA_E=LAMBDA_E,
                 beta_HH= beta_HH*0.9,  beta_AA=beta_AA,  beta_HE=beta_HE,  beta_AH=beta_AH,
                 beta_EH=beta_EH,  beta_HA=beta_HA,  beta_EA=beta_EA,  beta_AE=beta_AE,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE,epsilon=epsilon)
  }else if (intervention==14){
    params2 <- c(LAMBDA_H=LAMBDA_H*0.75,
                 LAMBDA_A=LAMBDA_A, 
                 LAMBDA_E=LAMBDA_E,
                 beta_HH= beta_HH*0.75,  beta_AA=beta_AA,  beta_HE=beta_HE,  beta_AH=beta_AH,
                 beta_EH=beta_EH,  beta_HA=beta_HA,  beta_EA=beta_EA,  beta_AE=beta_AE,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE,epsilon=epsilon)
    
  } else if (intervention==15){ #NSP-AMR
    params2 <- c(LAMBDA_H=LAMBDA_H*0.8,
                 LAMBDA_A=LAMBDA_A*0.7, 
                 LAMBDA_E=LAMBDA_E,
                 beta_HH= beta_HH*0.8,  beta_AA=beta_AA*0.8,  beta_HE=beta_HE*0.8,  beta_AH=beta_AH*0.8,
                 beta_EH=beta_EH*0.8,  beta_HA=beta_HA*0.8,  beta_EA=beta_EA*0.8,  beta_AE=beta_AE*0.8,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE*0.8,epsilon=epsilon)
  } else if (intervention==16){ #NSP-AMR with 70% instead of 80%
    params2 <- c(LAMBDA_H=LAMBDA_H*0.8,
                 LAMBDA_A=LAMBDA_A*0.7, 
                 LAMBDA_E=LAMBDA_E,
                 beta_HH= beta_HH*0.7,  beta_AA=beta_AA*0.7,  beta_HE=beta_HE*0.7,  beta_AH=beta_AH*0.7,
                 beta_EH=beta_EH*0.7,  beta_HA=beta_HA*0.7,  beta_EA=beta_EA*0.7,  beta_AE=beta_AE*0.7,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE*0.7,epsilon=epsilon)
  } else if (intervention==17){ #50% reduction in drinking water
    params2 <- c(LAMBDA_H=LAMBDA_H,
                 LAMBDA_A=LAMBDA_A, 
                 LAMBDA_E=LAMBDA_E,
                 beta_HH= beta_HH,  beta_AA=beta_AA,  beta_HE=beta_HE,  beta_AH=beta_AH,
                 beta_EH=beta_EH*0.5,  beta_HA=beta_HA,  beta_EA=beta_EA*0.5,  beta_AE=beta_AE,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE,epsilon=epsilon)
  } else if (intervention==18){ #human animal interaction reduced by 50%
    params2 <- c(LAMBDA_H=LAMBDA_H,
                 LAMBDA_A=LAMBDA_A, 
                 LAMBDA_E=LAMBDA_E,
                 beta_HH= beta_HH,  beta_AA=beta_AA,  beta_HE=beta_HE,  beta_AH=beta_AH*0.5,
                 beta_EH=beta_EH,  beta_HA=beta_HA*0.5,  beta_EA=beta_EA,  beta_AE=beta_AE,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE,epsilon=epsilon)
  }else if (intervention==19){ # 50 % in use
    params2 <- c(LAMBDA_H=LAMBDA_H*0.5,
                 LAMBDA_A=LAMBDA_A*0.5, 
                 LAMBDA_E=LAMBDA_E*0.5,
                 beta_HH= beta_HH,  beta_AA=beta_AA,  beta_HE=beta_HE,  beta_AH=beta_AH,
                 beta_EH=beta_EH,  beta_HA=beta_HA,  beta_EA=beta_EA,  beta_AE=beta_AE,  
                 mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,beta_EE=beta_EE,epsilon=epsilon)
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
  
  if (returnout ==1){
    return(out2)} else{
      
      return(c(modelend.time.H= modelend.time.H,
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
      ))}
}
