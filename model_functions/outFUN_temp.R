
################################  outFUN fitting function #####################################
outFUN_temp <- function(bbb,a0){ #feed in the sample (bbb), along with the number of the file you want to name (a0)
  outMat_SIR = apply(bbb,1,function(x) { 
    epid( as.list(x)$LAMBDA_H, as.list(x)$LAMBDA_A, as.list(x)$LAMBDA_E,
          as.list(x)$beta_HH,   as.list(x)$beta_AA,   as.list(x)$beta_HA,   as.list(x)$beta_AH,
          as.list(x)$beta_HE,   as.list(x)$beta_EH,   as.list(x)$beta_AE,   as.list(x)$beta_EA,  
          as.list(x)$beta_EE,
          as.list(x)$mu_H,  as.list(x)$mu_A,  as.list(x)$mu_E ,as.list(x)$gamma,
          as.list(x)$epsilon,0, a0
          
    )})
  #  print(outMat_SIR)
  ResMat_LHS = 
    cbind(bbb,t(outMat_SIR)) 
  
  write.csv(ResMat_LHS,paste0("output/OUT_", a0,".csv")) #will write a csv file called OUT_a0
}
