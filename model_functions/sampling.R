################################  Sampling function #####################################
### Function to generate parameter samples limiting them to the pre-specified ranges 
### INPUTS
## aaa = how many samples to generate 
### OUTPUTS
## paramsMat_SIR = parameters with key relationship limits

sampling <- function(aaa){
  Samples <- randomLHS(aaa, 16) # Generate aaa number of the needed 16 parameters between 0 and 1
  
  list(
       beta_HH = 0.1,
       beta_AA = 0.1,
       beta_HA = 0.1,
       beta_AH = 0.1,
       beta_HE = 0.1,
       beta_EH = 0.1,
       beta_AE = 0.1,
       beta_EA = 0.1,
       beta_EE = 0.1,
       mu_H = 0.1,
       mu_A = 0.1,
       mu_E = 0.1,
       date_3gc = 1990) # after 1960
  
  
  
  
  ### Exposure to antibiotics
  Samples[,1] <- 0 + (1-0)*Samples[,1]#LAMBDA_H #vary to max 
  Samples[,2] <- 0 + (Samples[,1]-0)*Samples[,2]   #LAMBDA_A ##vary to max so that A<H
  Samples[,3] <- 0 + (1-0)*Samples[,3]#no information yet 
  
  ### Transmission within settings
  # Max 10% chance per month of contact
  Samples[,4] <- 0.001 + (0.1-0.001)*Samples[,4]  #beta_HH
  Samples[,5] <- 0.001 + (0.1-0.001)*Samples[,5] #beta_AA
  Samples[,12] <-  0.001 + (0.1-0.001)*Samples[,12] #beta_EE
  
  ### Transmission between settings
  Samples[,7]  <- 0 + (Samples[,4]-0)*Samples[,7] #beta_AH < beta_HH
  Samples[,6]  <- 0 + (Samples[,5]-0)*Samples[,6] #beta_HA < beta_AA
  Samples[,9]  <- 0 + (Samples[,4]-0)*Samples[,9] ##beta_EH < beta_HH
  Samples[,11] <- 0 + (Samples[,5]-0)*Samples[,11] #beta_EA < beta_AA
  
  Samples[,10] <- 0 + (0.1-0)*Samples[,10] #beta_AE 
  Samples[,8]  <- 0 + (0.1-0)*Samples[,8]  #beta_HE
  
  ### Clearance of AMR 
  ##Carriage of ESBL or CRE at 12 months  community 25.4% patients 35.2%
  Samples[,13] <- 0 + (1-0)*Samples[,13]  #mu_H 
  Samples[,14] <-  0 + (1-0)*Samples[,14]   #mu_A
  Samples[,15] <-  0 + (1-0)*Samples[,15]   #mu_E
  
  return(Samples)
}
