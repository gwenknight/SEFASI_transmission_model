################################  Sampling function #####################################

Sampling <- function(aaa){
  Samples_SIR <- randomLHS(aaa, 7) #from https://www.sciencedirect.com/science/article/pii/S2542519619301305
  
  
  Samples_SIR[,2] <- 0 + (0.1-0)*Samples_SIR[,2] #LAMBDA_A #vary to max
  Samples_SIR[,1] <- 0 + (Samples_SIR[,2]-0)*Samples_SIR[,1]  #LAMBDA_H #vary to max so that H<A
  Samples_SIR[,3] <- 0 + (Samples_SIR[,1]-0)*Samples_SIR[,3] #LAMBDA_E #vary to max so that E<A
  Samples_SIR[,4] <- 0 + (input.table[input.table$parameter=='mu_max',]$value-0)*Samples_SIR[,4]  #mu_H #Carriage of ESBL or CRE at 12 months  community 25.4% patients 35.2%
  Samples_SIR[,5] <-  0 + (input.table[input.table$parameter=='mu_max',]$value-0)*Samples_SIR[,5]   #mu_A
  Samples_SIR[,6] <-  0 + (input.table[input.table$parameter=='mu_max',]$value-0)*Samples_SIR[,6]   #mu_E
  Samples_SIR[,7] <- 0 + (1-0)*Samples_SIR[,7]    #gamma

  
  
  paramsMat_SIR <- data.frame(
    LAMBDA_H=Samples_SIR[,1],LAMBDA_A=Samples_SIR[,2],LAMBDA_E=Samples_SIR[,3],
    beta_HH=0.5,  beta_AA=0.4,   beta_HA=0.2, beta_AH=0.24,
    beta_HE=0.5,  beta_EH=0.22,    beta_AE=0.5,   beta_EA=0.6,
    beta_EE = 0.5,
    mu_H = Samples_SIR[,4], mu_A =Samples_SIR[,5], mu_E = Samples_SIR[,6],gamma = Samples_SIR[,7],epsilon=1
  )
  return(paramsMat_SIR)
}
