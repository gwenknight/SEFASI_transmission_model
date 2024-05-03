################################  AMRmodel #####################################
### Core AMR model functions of the three environments for Denmark, England and Senegal

### AMR Model
# parameters <- list(LAMBDA_H = 0.1,
#                    LAMBDA_A = 0.1,
#                    LAMBDA_E = 0.1,
#                    beta_HH = 0.1,
#                    beta_HA = 0.1,
#                    beta_HE = 0.1,
#                    beta_AA = 0.1,
#                    beta_AH = 0.1,
#                    beta_AE = 0.1,
#                    beta_EE = 0.1,
#                    beta_EA = 0.1,
#                    beta_EH = 0.1,
#                    mu_H = 0.1,
#                    mu_A = 0.1,
#                    mu_E = 0.1,
#                    date_3gc = 1990) # after 1960

AMRmodel <- function(times, init, u, parameters_in){
  # time parameters: years
  # init = initial conditions 
  # u = usage per year 
  # parameters = needed parameters
  # Checks on parameters
  if(length(parameters_in) > 16){break}
  
  parameters <- list(LAMBDA_H = parameters_in[1],
                     LAMBDA_A = parameters_in[2],
                     LAMBDA_E = parameters_in[3],
                     beta_HH = parameters_in[4],
                     beta_AA = parameters_in[5],
                     beta_EE = parameters_in[6],
                     beta_AH = parameters_in[7],
                     beta_HA = parameters_in[8],
                     beta_EH = parameters_in[9],
                     beta_EA = parameters_in[10],
                     beta_AE = parameters_in[11],
                     beta_HE = parameters_in[12],
                     mu_H = parameters_in[13],
                     mu_A = parameters_in[14],
                     mu_E = parameters_in[15],
                     paraset = parameters_in[16])
  
  ## initial vector
  pops <- as.data.frame(matrix(0,1+length(times),length(init))) 
  pops <- cbind(c(0,times), pops,parameters_in[16])
  colnames(pops) <- c("time","H","A","E","para")
  pops[1,] <- c(0,init,parameters_in[16]) # initial conditions
  max_pop <- 100000
  
  # usage in each environment
  u_humans <- as.numeric(unlist(u %>% filter(source == "humans") %>% pull(normalise_kg)))
  u_animals <- as.numeric(unlist(u %>% filter(source == "animals") %>% pull(normalise_kg)))
  u_env <- as.numeric(unlist(u %>% filter(source == "environ") %>% pull(normalise_kg)))
  
  # Simulate dynamics
  for(i in times){
    # At each time step = previous value + min of the proportion left to being all resistant
    pops[i+1, "H"] <- pops[i, "H"] + min(max_pop - pops[i, "H"], (1 + parameters$LAMBDA_H*u_humans[i])*parameters$beta_HH*pops[i, "H"]*(max_pop - pops[i, "H"]) + (1 + parameters$LAMBDA_H*u_humans[i])*parameters$beta_AH*(max_pop - pops[i, "H"])*pops[i, "A"] + (1 + parameters$LAMBDA_H*u_humans[i])*parameters$beta_EH*(max_pop - pops[i, "H"])*pops[i, "E"] - parameters$mu_H*pops[i, "H"])
    pops[i+1, "A"] <- pops[i, "A"] + min(max_pop - pops[i, "A"], (1 + parameters$LAMBDA_A*u_animals[i])*parameters$beta_AA*pops[i, "A"]*(max_pop - pops[i, "A"]) + (1 + parameters$LAMBDA_A*u_animals[i])*parameters$beta_HA*(max_pop - pops[i, "A"])*pops[i, "H"] + (1 + parameters$LAMBDA_A*u_animals[i])*parameters$beta_EA*(max_pop - pops[i, "A"])*pops[i, "E"] - parameters$mu_A*pops[i, "A"])
    pops[i+1, "E"] <- pops[i, "E"] + min(max_pop - pops[i, "E"], (1 + parameters$LAMBDA_E*u_env[i])*parameters$beta_EE*pops[i, "E"]*(max_pop - pops[i, "E"]) + (1 + parameters$LAMBDA_E*u_env[i])*parameters$beta_HE*(max_pop - pops[i, "E"])*pops[i, "H"] + (1 + parameters$LAMBDA_E*u_env[i])*parameters$beta_AE*(max_pop - pops[i, "E"])*pops[i, "A"] - parameters$mu_E*pops[i, "E"])
  }
  
  # Divide by max_pop to give proportions 
  pops[,c("H","A","E")] <- pops[,c("H","A","E")]/max_pop
  
  return(pops)
  
}

################################  Sampling function #####################################
### Function to generate parameter samples limiting them to the pre-specified ranges 
### INPUTS
## aaa = how many samples to generate 
### OUTPUTS
## paramsMat_SIR = parameters with key relationship limits

sampling <- function(aaa){
  Samples <- randomLHS(aaa, 15) # Generate aaa number of the needed 16 parameters between 0 and 1
  
  ### Exposure to antibiotics
  Samples[,1] <- 0 + (1-0)*Samples[,1]  #LAMBDA_H #vary to max 
  Samples[,2] <- 0 + (1-0)*Samples[,2]  #LAMBDA_A #vary to max
  Samples[,3] <- 0 + (1-0)*Samples[,3]  #LAMBDA_E no information yet 
  
  ### Transmission within settings
  # Max 0.1% chance per month of contact
  Samples[,4] <- 0 + (0.00001-0)*Samples[,4]  #beta_HH
  Samples[,5] <- 0 + (0.00001-0)*Samples[,5] #beta_AA
  Samples[,6] <-  0 + (0.00001-0)*Samples[,6] #beta_EE
  
  ### Transmission between settings
  Samples[,7]  <- 0 + (0.00001-0)*Samples[,7] # removed this condition: beta_AH < beta_HH
  Samples[,8]  <- 0 + (0.00001-0)*Samples[,8] #removed this condition: beta_HA < beta_AA
  Samples[,9]  <- 0 + (0.00001-0)*Samples[,9] ##removed this condition: beta_EH < beta_HH
  Samples[,10] <- 0 + (0.00001-0)*Samples[,10] #removed this condition: beta_EA < beta_AA
  
  Samples[,11] <- 0 + (0.00001-0)*Samples[,11] #beta_AE 
  Samples[,12] <- 0 + (0.00001-0)*Samples[,12]  #beta_HE
  
  ### Clearance of AMR 
  ##Carriage of ESBL or CRE at 12 months in community 25.4% patients 35.2%
  # Assume can clear in month on average
  Samples[,13] <- 0 + (1-0)*Samples[,13]  #mu_H 
  Samples[,14] <- 0 + (1-0)*Samples[,14]   #mu_A
  Samples[,15] <- 0 + (1-0)*Samples[,15]   #mu_E
  
  return(Samples)
}

###### Model of AMR in Denmark with time varying antibiotic usage
AMRmodel_DENMARK <- function(time,state,parameters){ #using package deSolve
  with(as.list(c(state,parameters)),{
    # Time varying antibiotic usage for Denmark
    
    LAMBDA_H_temp_input <- 1
    
    if (time<= 2)  {
      LAMBDA_H_temp <- LAMBDA_H_temp_input* den_use_h[den_use_h$year == round((2000+time),0),]$kg/initial_den_H
      LAMBDA_A_temp <- LAMBDA_H_temp_input/H_A_ratio_den 
    } else if ((time>2) & (time<= 3))  {
      LAMBDA_H_temp <- LAMBDA_H_temp_input* den_use_h[den_use_h$year == round((2000+time),0),]$kg/initial_den_H
      LAMBDA_A_temp <- (LAMBDA_H_temp_input/H_A_ratio_den)*ratio_den_2003_A
    } else if ((time>3) & (time<= 15))  {  
      LAMBDA_H_temp <- LAMBDA_H_temp_input* den_use_h[den_use_h$year == round((2000+time),0),]$kg/initial_den_H
      LAMBDA_A_temp <- (LAMBDA_H_temp_input/H_A_ratio_den)*ratio_den_2003_A + (time-3)*(((LAMBDA_H_temp_input/H_A_ratio_den)*ratio_den_2015_A - (LAMBDA_H_temp_input/H_A_ratio_den)*ratio_den_2003_A)/(15-3)) 
    } else if ((time>15) & (time<= 16))  {  
      LAMBDA_H_temp <- LAMBDA_H_temp_input* den_use_h[den_use_h$year == round((2000+time),0),]$kg/initial_den_H
      LAMBDA_A_temp <- (LAMBDA_H_temp_input/H_A_ratio_den)*ratio_den_2016_A
    }  else if ((time>16) & (time<= 18))   { 
      LAMBDA_H_temp<- LAMBDA_H_temp_input* den_use_h[den_use_h$year == round((2000+time),0),]$kg/initial_den_H
      LAMBDA_A_temp <- (LAMBDA_H_temp_input/H_A_ratio_den)*ratio_den_2016_A + (time-16)*(((LAMBDA_H_temp_input/H_A_ratio_den)*ratio_den_2018_A - (LAMBDA_H_temp_input/H_A_ratio_den)*ratio_den_2016_A)/(18-16))
    } else if ((time>18) & (time<= 20))  {
      LAMBDA_H_temp <- LAMBDA_H_temp_input* den_use_h[den_use_h$year == round((2000+time),0),]$kg/initial_den_H
      LAMBDA_A_temp <-   (LAMBDA_H_temp_input/H_A_ratio_den)*ratio_den_2018_A + (time-18)*(((LAMBDA_H_temp_input/H_A_ratio_den)*ratio_den_2020_A - (LAMBDA_H_temp_input/H_A_ratio_den)*ratio_den_2018_A)/(20-18)) 
    } else if ((time> 20))  {
      LAMBDA_H_temp <- LAMBDA_H_temp_input* den_use_h[den_use_h$year == (2019),]$kg/initial_den_H 
      LAMBDA_A_temp <- (LAMBDA_H_temp_input/H_A_ratio_den)*ratio_den_2021_A }
    
    LAMBDA_H <- LAMBDA_H_temp
    LAMBDA_A <- LAMBDA_A_temp
    
    # Equations governing Human - Animal - Environment interaction 
    dH <- (1 + LAMBDA_H)*beta_HH*H*(1-H) + (1 + LAMBDA_H)*beta_AH*(1-H)*A + (1 + LAMBDA_H)*beta_EH*(1-H)*E - mu_H*H
    dA <- (1 + LAMBDA_A)*beta_AA*A*(1-A) + (1 + LAMBDA_A)*beta_HA*(1-A)*H + (1 + LAMBDA_A)*beta_EA*(1-A)*E - mu_A*A
    dE <-  (1 + LAMBDA_E)*beta_EE*E*(1-E) + (1 + LAMBDA_E)*beta_HE*(1-E)*H + (1 + LAMBDA_E)*beta_AE*(1-E)*A - mu_E*E
    # print(c(time,LAMBDA_H,LAMBDA_A,LAMBDA_E))
    return(  list(c(dH,dA,dE)))
  })}

### Least squares function 
LS_simple  <- function(DATA_INPUT, input_country, res.table){
  
  # Extract the resistance prevalence data for this country 
  res_H_country <- res.table[res.table$country==input_country & res.table$var=="H",]
  res_A_country <- res.table[res.table$country==input_country & res.table$var=="A",]
  res_E_country <- res.table[res.table$country==input_country & res.table$var=="E",]
  
  # Generate empty matrix to store LL
  LL_H_country <- rep(NA,nrow(res_H_country))
  LL_A_country <- rep(NA,nrow(res_A_country))
  LL_E_country <- rep(NA,nrow(res_E_country))
  
  # At what times are there data to compare to? 
  times_H_country <- res_H_country$time
  times_A_country <- res_A_country$time
  times_E_country <- res_E_country$time
  
  # For the datapoints check how far model from data: GK ERROR here: res_H_country no necessarily correct year! 
  for (i in 1:length(LL_H_country)) {
    #LL_H_country[i] = res_H_country$percent[i]*log(eval(parse(text=paste("DATA_INPUT$model",times_H_country[i],".H",sep="")))) + 
    #  (1- res_H_country$percent[i])*log(1-eval(parse(text=paste("DATA_INPUT$model",times_H_country[i],".H",sep=""))))   
    # Least squares
    #LL_H_country[i] = (res_H_country$percent[i]/100 - eval(parse(text=paste("DATA_INPUT$model",times_H_country[i],".H",sep=""))))^2
    LL_H_country[i] = (res_H_country$percent[i]/100 - DATA_INPUT %>% filter(year == as.numeric(res_H_country[i,"time"]), name == "H") %>% select(mean_vl))^2 # eval(parse(text=paste("DATA_INPUT$model",times_H_country[i],".H",sep=""))))^2
    
  }
  
  for (i in 1:length(LL_A_country)) {
    #LL_A_country[i] = res_A_country$percent[i]*log(eval(parse(text=paste("DATA_INPUT$model",times_A_country[i],".A",sep="")))) + (1- res_A_country$percent[i])*log(1-eval(parse(text=paste("DATA_INPUT$model",times_A_country[i],".A",sep=""))))   
    
    # Least squares
    #LL_A_country[i] = (res_A_country$percent[i]/100 - eval(parse(text=paste("DATA_INPUT$model",times_A_country[i],".A",sep=""))))^2
    LL_A_country[i] = (res_A_country$percent[i]/100 - DATA_INPUT %>% filter(year == as.numeric(res_A_country[i,"time"]), name == "A") %>% select(mean_vl))^2 
  }
  
  for (i in 1:length(LL_E_country)) {
    #LL_E_country[i] = res_E_country$percent[i]*log(eval(parse(text=paste("DATA_INPUT$model",times_E_country[i],".E",sep="")))) + (1- res_E_country$percent[i])*log(1-eval(parse(text=paste("DATA_INPUT$model",times_E_country[i],".E",sep=""))))   
    # Least squares
    #LL_E_country[i] = (res_E_country$percent[i]/100 - eval(parse(text=paste("DATA_INPUT$model",times_E_country[i],".E",sep=""))))^2
    LL_E_country[i] = (res_E_country$percent[i]/100 - DATA_INPUT %>% filter(year == as.numeric(res_E_country[i,"time"]), name == "E") %>% select(mean_vl))^2 
  }
  
  
  #MLE<- sum(LL_H_country)/length(LL_H_country) + sum(LL_A_country)/length(LL_A_country) +  sum(LL_E_country)/length(LL_E_country)
  LS <- sum(unlist(LL_H_country)) + sum(unlist(LL_A_country)) +  sum(unlist(LL_E_country))
  #print(MLE)
  
  return(LS)
}

############# AMRmodel with interventions
AMRmodel_interv <- function(times, init, u, new_u, parameters_in){
  # time parameters: years
  # init = initial conditions
  # u = usage per year
  # parameters = needed parameters

  # Run the model on these parameters
  A <- AMRmodel(times, init, u, parameters_in)

  # Initial conditions now
  init_new <- tail(A,1)
  # Store
  Store_A <- matrix(0,dim(A)[1]*21,6)

  for(intervention in 0:20){ # 20 interventions

    names(parameters_in) <- c("LAMBDA_H","LAMBDA_A","LAMBDA_E","beta_HH","beta_AA","beta_EE","beta_AH",
                              "beta_HA","beta_EH","beta_EA","beta_AE","beta_HE","mu_H","mu_A","mu_E","para")

    if (intervention==0){ # Baseline
      params2 <- parameters_in
    } else if (intervention==1){ #
      params2 <- parameters_in
      params2["LAMBDA_H"] <- 0
    } else if (intervention==2){ #
      params2 <- parameters_in
      params2["LAMBDA_A"] <- 0
    } else if (intervention==3){
      params2 <- parameters_in
      params2["LAMBDA_E"] <- 0
    } else if (intervention==4){
      params2 <- parameters_in
      params2["beta_HH"] <- 0
    }else if (intervention==5){
      params2 <- parameters_in
      params2["beta_AA"] <- 0
    } else if (intervention==6){
      params2 <- parameters_in
      params2["beta_EE"] <- 0
    } else if (intervention==7){
      params2 <- parameters_in
      params2["beta_AH"] <- 0
      params2["beta_HA"] <- 0
    } else if (intervention==8){
      params2 <- parameters_in
      params2["beta_EH"] <- 0
    }else if (intervention==9){
      params2 <- parameters_in
      params2["beta_EH"] <- 0
      params2["beta_HE"] <- 0
    }else if (intervention==10){
      params2 <- parameters_in
      params2["beta_EA"] <- 0
      params2["beta_AE"] <- 0
    }else if (intervention==11){
      params2 <- parameters_in
      params2["beta_EA"] <- 0
    }else if (intervention==12){
      #### New ones - above just variations on single parameters
      #DENMARK NAP
      params2 <- parameters_in
      params2["LAMBDA_H"] <- parameters_in["LAMBDA_H"]*0.9
      params2["LAMBDA_A"] <- parameters_in["LAMBDA_A"]*0.8
      params2["beta_HH"] <- parameters_in["beta_HH"]*0.8
      params2["beta_HA"] <- parameters_in["beta_HA"]*0.8
      params2["beta_AA"] <- parameters_in["beta_AA"]*0.8
      params2["beta_AH"] <- parameters_in["beta_AH"]*0.8
      params2["beta_EE"] <- parameters_in["beta_EE"]*0.8
      params2["beta_EH"] <- parameters_in["beta_EH"]*0.8
      params2["beta_HE"] <- parameters_in["beta_HE"]*0.8
      params2["beta_AE"] <- parameters_in["beta_AE"]*0.8
      params2["beta_EA"] <- parameters_in["beta_EA"]*0.8
    } else if (intervention==13){ # ENGLAND NAP
      params2 <- parameters_in
      params2["LAMBDA_H"] <- parameters_in["LAMBDA_H"]*0.85
      params2["LAMBDA_A"] <- parameters_in["LAMBDA_A"]*0.75
      params2["beta_HH"] <- parameters_in["beta_HH"]*0.8
      params2["beta_HA"] <- parameters_in["beta_HA"]*0.8
      params2["beta_AA"] <- parameters_in["beta_AA"]*0.8
      params2["beta_AH"] <- parameters_in["beta_AH"]*0.8
      params2["beta_EE"] <- parameters_in["beta_EE"]*0.8
      params2["beta_EH"] <- parameters_in["beta_EH"]*0.8
      params2["beta_HE"] <- parameters_in["beta_HE"]*0.8
      params2["beta_AE"] <- parameters_in["beta_AE"]*0.8
      params2["beta_EA"] <- parameters_in["beta_EA"]*0.8
    } else if (intervention==14){ #SENEGAL NAP
      params2 <- parameters_in
      params2["LAMBDA_H"] <- parameters_in["LAMBDA_H"]*0.95
      params2["LAMBDA_A"] <- parameters_in["LAMBDA_A"]*0.95
      params2["LAMBDA_H"] <- parameters_in["LAMBDA_E"]*0.95
      params2["beta_HH"] <- parameters_in["beta_HH"]*0.9
      params2["beta_HA"] <- parameters_in["beta_HA"]*0.9
      params2["beta_AA"] <- parameters_in["beta_AA"]*0.9
      params2["beta_AH"] <- parameters_in["beta_AH"]*0.9
      params2["beta_EE"] <- parameters_in["beta_EE"]*0.9
      params2["beta_EH"] <- parameters_in["beta_EH"]*0.9
      params2["beta_HE"] <- parameters_in["beta_HE"]*0.9
      params2["beta_AE"] <- parameters_in["beta_AE"]*0.9
      params2["beta_EA"] <- parameters_in["beta_EA"]*0.9
    } else if (intervention==15){ # FARM
      params2 <- parameters_in
      params2["LAMBDA_A"] <- parameters_in["LAMBDA_A"]*0.5
      params2["beta_HA"] <- parameters_in["beta_HA"]*0.5
      params2["beta_AH"] <- parameters_in["beta_AH"]*0.5
    }else if (intervention==16){ # HUMAN
      params2 <- parameters_in
      params2["LAMBDA_H"] <- parameters_in["LAMBDA_H"]*0.5
      params2["beta_HH"] <- parameters_in["beta_HH"]*0.5
    }else if (intervention==17){ # ENV
      params2 <- parameters_in
      params2["LAMBDA_E"] <- parameters_in["LAMBDA_E"]*0.5
      params2["beta_EE"] <- parameters_in["beta_EE"]*0.5
      params2["beta_EH"] <- parameters_in["beta_EH"]*0.5
      params2["beta_HE"] <- parameters_in["beta_HE"]*0.5
      params2["beta_AE"] <- parameters_in["beta_AE"]*0.5
      params2["beta_EA"] <- parameters_in["beta_EA"]*0.5
    }else if (intervention==18){ # HUMAN + ANIMAL CONTACT
      params2 <- parameters_in
      params2["beta_HA"] <- parameters_in["beta_HA"]*0.5
      params2["beta_AH"] <- parameters_in["beta_AH"]*0.5
    }else if (intervention==19){ # TRANSMISSION
      params2 <- parameters_in
      params2["beta_HH"] <- parameters_in["beta_HH"]*0.5
      params2["beta_HA"] <- parameters_in["beta_HA"]*0.5
      params2["beta_AA"] <- parameters_in["beta_AA"]*0.5
      params2["beta_AH"] <- parameters_in["beta_AH"]*0.5
      params2["beta_EE"] <- parameters_in["beta_EE"]*0.5
      params2["beta_EH"] <- parameters_in["beta_EH"]*0.5
      params2["beta_HE"] <- parameters_in["beta_HE"]*0.5
      params2["beta_AE"] <- parameters_in["beta_AE"]*0.5
      params2["beta_EA"] <- parameters_in["beta_EA"]*0.5
    }else if (intervention==20){ # USAGE
      params2 <- parameters_in
      params2["LAMBDA_H"] <- parameters_in["LAMBDA_H"]*0.5
      params2["LAMBDA_A"] <- parameters_in["LAMBDA_A"]*0.5
      params2["LAMBDA_E"] <- parameters_in["LAMBDA_E"]*0.5
    }

    # Run with new parameter set 
    params2 <- as.numeric(params2)
    times <- seq(1,(5)*52,1) # 5 yr time horizon 
    A_new <- AMRmodel(times, init_new[c("H","A","E")], new_u, params2)
    A_new$inter <- intervention
    # Store 
    Store_A[((intervention)*dim(A_new)[1]+1):((intervention+1)*dim(A_new)[1]),] <- as.matrix(A_new)
  }
  Store_A <- as.data.frame(Store_A)
  colnames(Store_A) <- c("time","H","A","E","para","interven")

  return(Store_A)  
}
