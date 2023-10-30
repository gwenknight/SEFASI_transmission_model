################################  Simple simulator of epidemiology  #####################################
############# epid function = wrapper for ode and country models ##########################
##### INPUTS
### Lambda_H : episilon = parameters for code AMR model 
### returnout = if 1 then gives output
### input_country = which of the 3 SEFASI countries to input

##### OUTPUTS
### model simulation output for that country 

epid <- function(LAMBDA_H, LAMBDA_A, LAMBDA_E,
                 beta_HH, beta_AA, 
                 beta_HA, beta_AH, 
                 beta_HE, beta_EH, 
                 beta_AE, beta_EA,
                 beta_EE,
                 mu_H, mu_A, mu_E,gamma,epsilon, returnout, input_country
){
  params <- c(LAMBDA_H=LAMBDA_H,LAMBDA_A=LAMBDA_A,LAMBDA_E=LAMBDA_E,
              beta_HH=beta_HH,  beta_AA=beta_AA,  beta_HE=beta_HE,  beta_AH=beta_AH,
              beta_EH=beta_EH,  beta_HA=beta_HA,  beta_EA=beta_EA,  beta_AE=beta_AE, 
              beta_EE=beta_EE,  mu_H = mu_H, mu_A = mu_A, mu_E = mu_E,gamma=gamma,epsilon=epsilon)
  
  
  #run the core AMR model using desolve
  if (input_country=="denmark"){
    out <- as.data.frame(ode(y=state,time=vectTime,func=AMRmodel_DENMARK,parms=params))
  } else if  (input_country=="england"){
    out <- as.data.frame(ode(y=state,time=vectTime,func=AMRmodel_ENGLAND,parms=params))
  } else{
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
