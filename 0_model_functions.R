################################  AMRmodel #####################################
### Core AMR model functions of the three environments for Denmark, England and Senegal

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

###### Model of AMR in Senegal with no time varying antibiotic usage due to a lack of data
AMRmodel_SENEGAL <- function(time,state,parameters){ #using package deSolve
  with(as.list(c(state,parameters)),{
    
    # Equations governing Human - Animal - Environment interaction 
    dH <- (1 + LAMBDA_H)*beta_HH*H*(1-H) + (1 + LAMBDA_H)*beta_AH*(1-H)*A + (1 + LAMBDA_H)*beta_EH*(1-H)*E - mu_H*H
    dA <- (1 + LAMBDA_A)*beta_AA*A*(1-A) + (1 + LAMBDA_A)*beta_HA*(1-A)*H + (1 + LAMBDA_A)*beta_EA*(1-A)*E - mu_A*A
    dE <-  (1 + LAMBDA_E)*beta_EE*E*(1-E) + (1 + LAMBDA_E)*beta_HE*(1-E)*H + (1 + LAMBDA_E)*beta_AE*(1-E)*A - mu_E*E
    # print(c(time,LAMBDA_H,LAMBDA_A,LAMBDA_E))
    return(  list(c(dH,dA,dE)))
  })}

###### Model of AMR in England with time varying antibiotic usage
AMRmodel_ENGLAND <- function(time,state,parameters){ #using package deSolve
  with(as.list(c(state,parameters)),{ #
    
    # Time varying antibiotic usage for England 
    if (time <= time1_eng)  {LAMBDA_H <- LAMBDA_H
    LAMBDA_A <- LAMBDA_H/H_A_ratio_eng } else if ((time >time1_eng) & (time < time2_eng))  {  
      LAMBDA_H <- LAMBDA_H + ((time - time1_eng)*(((LAMBDA_H*ratio_eng_2017_H) - LAMBDA_H)/(time2_eng-time1_eng)))
      LAMBDA_A <- LAMBDA_H/H_A_ratio_eng + ((time - time1_eng)*(((LAMBDA_H/H_A_ratio_eng)*ratio_eng_2017_A - LAMBDA_H/H_A_ratio_eng )/(time2_eng-time1_eng)))} else if (time>=time2_eng)   {
        LAMBDA_H <- LAMBDA_H*ratio_eng_2017_H
        LAMBDA_A <- (LAMBDA_H/H_A_ratio_eng)*ratio_eng_2017_A}
    
    # Equations governing Human - Animal - Environment interaction 
    dH <- (1 + LAMBDA_H)*beta_HH*H*(1-H) + (1 + LAMBDA_H)*beta_AH*(1-H)*A + (1 + LAMBDA_H)*beta_EH*(1-H)*E - mu_H*H
    dA <- (1 + LAMBDA_A)*beta_AA*A*(1-A) + (1 + LAMBDA_A)*beta_HA*(1-A)*H + (1 + LAMBDA_A)*beta_EA*(1-A)*E - mu_A*A
    dE <-  (1 + LAMBDA_E)*beta_EE*E*(1-E) + (1 + LAMBDA_E)*beta_HE*(1-E)*H + (1 + LAMBDA_E)*beta_AE*(1-E)*A - mu_E*E
    # print(c(time,LAMBDA_H,LAMBDA_A,LAMBDA_E))
    return(  list(c(dH,dA,dE)))
  })}


################################  Simple simulator of epidemiology  #####################################
############# epid function = wrapper for ode and country models ##########################
##### INPUTS
### Lambda_H : episilon = parameters for code AMR model 
### returnout = if 1 then gives output
### input_country = which of the 3 SEFASI countries to input

##### OUTPUTS
### model simulation output for that country 

epid <- function(params, returnout, input_country){
  # params <- c(LAMBDA_H=LAMBDA_H,LAMBDA_A=LAMBDA_A,LAMBDA_E=LAMBDA_E,
  #             beta_HH=beta_HH,  beta_AA=beta_AA,  beta_HE=beta_HE,  beta_AH=beta_AH,
  #             beta_EH=beta_EH,  beta_HA=beta_HA,  beta_EA=beta_EA,  beta_AE=beta_AE, 
  #             beta_EE=beta_EE,  mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,epsilon=epsilon)
  # 
  
  #run the core AMR model using desolve
  if (input_country=="denmark"){
    out <- as.data.frame(ode(y=state,time=vectTime,func=AMRmodel_DENMARK,parms=params))
  } else if  (input_country=="england"){
    out <- as.data.frame(ode(y=state,time=vectTime,func=AMRmodel_ENGLAND,parms=params))
  } else if (input_country == "senegal"){
    out <- as.data.frame(ode(y=state,time=vectTime,func=AMRmodel_SENEGAL,parms=params))
  }
  
  out$time = out$time+epid.start #rescale the time so that it runs from 2000 onwards 
  
  # Add in year
  for(i in 1:22){
    assign( paste("model",1999+i,".H",sep=""),
            out$H[out$time==1999+i] )
    
    assign( paste("model",1999+i,".A",sep=""),
            out$A[out$time==1999+i] )
    
    assign( paste("model",1999+i,".E",sep=""),
            out$E[out$time==1999+i] )
  }
  
  
  ## if returnout is input as 1 then return out (all data)
  if (returnout ==1){
    return(out)} else{
      
      return(c(model2000.H=model2000.H,
               model2001.H=model2001.H,
               model2002.H=model2002.H,
               model2003.H=model2003.H,
               model2004.H=model2004.H,
               model2005.H=model2005.H,
               model2006.H=model2006.H,
               model2007.H=model2007.H,
               model2008.H=model2008.H,
               model2009.H=model2009.H,
               model2010.H=model2010.H,
               model2011.H=model2011.H,
               model2012.H=model2012.H,
               model2013.H=model2013.H,
               model2014.H=model2014.H,
               model2015.H=model2015.H,
               model2016.H=model2016.H,
               model2017.H=model2017.H,
               model2018.H=model2018.H,
               model2019.H=model2019.H,
               model2020.H=model2020.H,
               model2021.H=model2021.H,
               model2000.A=model2000.A,
               model2001.A=model2001.A,
               model2002.A=model2002.A,
               model2003.A=model2003.A,
               model2004.A=model2004.A,
               model2005.A=model2005.A,
               model2006.A=model2006.A,
               model2007.A=model2007.A,
               model2008.A=model2008.A,
               model2009.A=model2009.A,
               model2010.A=model2010.A,
               model2011.A=model2011.A,
               model2012.A=model2012.A,
               model2013.A=model2013.A,
               model2014.A=model2014.A,
               model2015.A=model2015.A,
               model2016.A=model2016.A,
               model2017.A=model2017.A,
               model2018.A=model2018.A,
               model2019.A=model2019.A,
               model2020.A=model2020.A,
               model2021.A=model2021.A,
               model2000.E=model2000.E,
               model2001.E=model2001.E,
               model2002.E=model2002.E,
               model2003.E=model2003.E,
               model2004.E=model2004.E,
               model2005.E=model2005.E,
               model2006.E=model2006.E,
               model2007.E=model2007.E,
               model2008.E=model2008.E,
               model2009.E=model2009.E,
               model2010.E=model2010.E,
               model2011.E=model2011.E,
               model2012.E=model2012.E,
               model2013.E=model2013.E,
               model2014.E=model2014.E,
               model2015.E=model2015.E,
               model2016.E=model2016.E,
               model2017.E=model2017.E,
               model2018.E=model2018.E,
               model2019.E=model2019.E,
               model2020.E=model2020.E,
               model2021.E=model2021.E
      ))}
}


### Least squares
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
    LL_H_country[i] = (res_H_country$percent[i]/100 - eval(parse(text=paste("DATA_INPUT$model",times_H_country[i],".H",sep=""))))^2
    
  }
  
  for (i in 1:length(LL_A_country)) {
    #LL_A_country[i] = res_A_country$percent[i]*log(eval(parse(text=paste("DATA_INPUT$model",times_A_country[i],".A",sep="")))) + (1- res_A_country$percent[i])*log(1-eval(parse(text=paste("DATA_INPUT$model",times_A_country[i],".A",sep=""))))   
    
    # Least squares
    LL_A_country[i] = (res_A_country$percent[i]/100 - eval(parse(text=paste("DATA_INPUT$model",times_A_country[i],".A",sep=""))))^2
  }
  
  for (i in 1:length(LL_E_country)) {
    #LL_E_country[i] = res_E_country$percent[i]*log(eval(parse(text=paste("DATA_INPUT$model",times_E_country[i],".E",sep="")))) + (1- res_E_country$percent[i])*log(1-eval(parse(text=paste("DATA_INPUT$model",times_E_country[i],".E",sep=""))))   
    # Least squares
    LL_E_country[i] = (res_E_country$percent[i]/100 - eval(parse(text=paste("DATA_INPUT$model",times_E_country[i],".E",sep=""))))^2
  }
  
  
  #MLE<- sum(LL_H_country)/length(LL_H_country) + sum(LL_A_country)/length(LL_A_country) +  sum(LL_E_country)/length(LL_E_country)
  LS <- sum(LL_H_country) + sum(LL_A_country) +  sum(LL_E_country)
  #print(MLE)
  
  return(LS)
}
