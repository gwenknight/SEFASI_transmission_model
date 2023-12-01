################################  Sampling function #####################################
### Function to generate parameter samples limiting them to the pre-specified ranges 
### INPUTS
## aaa = how many samples to generate 
### OUTPUTS
## paramsMat_SIR = parameters with key relationship limits

sampling <- function(aaa){
  Samples_SIR <- randomLHS(aaa, 16) # Generate aaa number of the needed 16 parameters between 0 and 1
  
  ### Exposure to antibiotics
  Samples_SIR[,1] <- 0 + (1-0)*Samples_SIR[,1]#LAMBDA_H #vary to max 
  Samples_SIR[,2] <- 0 + (Samples_SIR[,1]-0)*Samples_SIR[,2]   #LAMBDA_A ##vary to max so that A<H
  Samples_SIR[,3] <- 0 + (1-0)*Samples_SIR[,3]#no information yet 
  
  ### Transmission within settings
  Samples_SIR[,4] <- input.table[input.table$parameter=='beta_HH_min',]$value + 
    (1-input.table[input.table$parameter=='beta_HH_min',]$value)*Samples_SIR[,4]  #beta_HH
  Samples_SIR[,5] <- 0 + (1-0)*Samples_SIR[,5] #beta_AA
  Samples_SIR[,12] <-  0 + (1-0)*Samples_SIR[,12] #beta_EE
  
  ### Transmission between settings
  Samples_SIR[,7]  <- 0 + (Samples_SIR[,4]-0)*Samples_SIR[,7] #beta_AH < beta_HH
  Samples_SIR[,6]  <- 0 + (Samples_SIR[,5]-0)*Samples_SIR[,6] #beta_HA < beta_AA
  Samples_SIR[,9]  <- 0 + (Samples_SIR[,4]-0)*Samples_SIR[,9] ##beta_EH < beta_HH
  Samples_SIR[,11] <- 0 + (Samples_SIR[,5]-0)*Samples_SIR[,11] #beta_EA < beta_AA
  
  Samples_SIR[,10] <- 0 + (1-0)*Samples_SIR[,10] #beta_AE 
  Samples_SIR[,8]  <- 0 + (1-0)*Samples_SIR[,8]  #beta_HE
  
  ### Clearance of AMR 
  ##Carriage of ESBL or CRE at 12 months  community 25.4% patients 35.2%
  Samples_SIR[,13] <- 0 + (input.table[input.table$parameter=='mu_max',]$value-0)*Samples_SIR[,13]  #mu_H 
  Samples_SIR[,14] <-  0 + (input.table[input.table$parameter=='mu_max',]$value-0)*Samples_SIR[,14]   #mu_A
  Samples_SIR[,15] <-  0 + (input.table[input.table$parameter=='mu_max',]$value-0)*Samples_SIR[,15]   #mu_E
  
  ### Rate of colonisation (multiples antibiotic exposure)
  Samples_SIR[,16] <- 0 + (1-0)*Samples_SIR[,16]    #gamma
  #Samples_SIR[,17] <- 0 + (1-0)*Samples_SIR[,17] #epsilon
  
  
  paramsMat_SIR <- data.frame(
    LAMBDA_H=Samples_SIR[,1],  LAMBDA_A=Samples_SIR[,2],LAMBDA_E=Samples_SIR[,3],
    beta_HH=Samples_SIR[,4],  beta_AA=Samples_SIR[,5],  beta_HA=Samples_SIR[,6],  beta_AH=Samples_SIR[,7],
    beta_HE=Samples_SIR[,8],  beta_EH=Samples_SIR[,9],  beta_AE=Samples_SIR[,10], beta_EA=Samples_SIR[,11],
    beta_EE = Samples_SIR[,12],
    mu_H = Samples_SIR[,13], mu_A =Samples_SIR[,14], mu_E = Samples_SIR[,15],
    gamma = Samples_SIR[,16],epsilon=1
  )
  return(paramsMat_SIR)
}
