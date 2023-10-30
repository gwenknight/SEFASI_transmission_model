
################################  outFUN fitting function #####################################
add_LL <- function(DATA_INPUT,input_country){ #feed in the sample (bbb), along with the number of the file you want to name (a0)
  outMat_SIR = apply(DATA_INPUT,1,function(x) {
    LL_function(DATA_INPUT,input_country
    )})
  OUT_LL = 
    cbind(DATA_INPUT,t(outMat_SIR)) 
  write.csv(OUT_LL,paste0("LL_", input_country,".csv")) #will write a csv file called OUT_a0
}