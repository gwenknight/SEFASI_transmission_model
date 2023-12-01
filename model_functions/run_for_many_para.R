################################  outFUN fitting function #####################################

### Not parallel
outFUN_temp <- function(bbb,a0){ #feed in the sample (bbb), along with the number of the file you want to name (a0)
  outMat_SIR = apply(bbb,1,function(x) { 
    
    epid(x, 0, a0)
    
  })
    #print(outMat_SIR)
  ResMat_LHS = cbind(bbb,t(outMat_SIR)) 
  
  write.csv(ResMat_LHS,paste0("output/OUT_", a0,".csv")) #will write a csv file called OUT_a0
}

### Parallel
outFUN <- function(bbb,input_country){ #feed in the sample (bbb), along with the number of the file you want to name (a0)
  # Make cluser
  cl<- makePSOCKcluster(detectCores(), useXDR=FALSE) # makeCluster(detectCores()-1, useXDR=FALSE) #
  x <- bbb
  
  clusterExport(cl=cl, varlist = c("x","epid","AMRmodel_ENGLAND","AMRmodel_SENEGAL","AMRmodel_DENMARK","state","epid.start","epid.duration",
                                   "vectTime","int.time","eval.time","end.time","time1_eng","time2_eng",
                                   "initial_eng_H","initial_eng_A","ratio_eng_2017_H","ratio_eng_2017_A",
                                   "time1_den","time2_den","time3_den","time4_den","time5_den",
                                   "initial_den_H","initial_den_A",
                                   "ratio_den_2003_H","ratio_den_2003_A",
                                   "ratio_den_2015_H","ratio_den_2015_A",
                                   "ratio_den_2016_H","ratio_den_2016_A",
                                   "ratio_den_2018_H","ratio_den_2018_A","ratio_den_2020_A","ratio_den_2021_A","H_A_ratio_den","H_A_ratio_eng", "den_use_h", "input_country"#,"den_usage"
  ),
  envir=environment())
  
  clusterEvalQ(cl=cl,c(library(deSolve),library(data.table), library(magrittr)))
  outMat_SIR = parApply(cl,x,1,function(x) {  # see below for working example of this function
    # apply(bbb,1,function(x) {
    epid( x, 0, #returnout
          input_country)})
  
  ResMat_LHS = 
    cbind(bbb,t(outMat_SIR)) 
  fwrite(ResMat_LHS,paste0("output/OUT_",input_country,".csv")) #will write a csv file called OUT_a0
  stopCluster(cl)
}

outFUN <- cmpfun(outFUN)


