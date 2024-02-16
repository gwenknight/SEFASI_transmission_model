#### plot output vs data 


plot_output_data <- function(SIR.unique){
  
  c <- colnames(SIR.unique)
  c[1] <- "run"
  colnames(SIR.unique) <- c
  
  model_output <- SIR.unique %>% 
    pivot_longer("model2000.H":"model2021.E") %>% 
    mutate(environment = sub(".*\\.", "", name), 
           year = as.numeric(substring(str_extract(name, "^[^\\.:]+"), nchar(str_extract(name, "^[^\\.:]+"))-3)))
  
  data_fit <- res.table.fit %>% filter(country == input_country) %>% mutate(environment = var)
  
  ggplot(model_output, aes(x=year, y = 100*value, group = run)) + geom_line() + 
    geom_point(data = data_fit,aes(x=time, y = percent, group = country), col = "red") + 
    facet_wrap(~environment,ncol = 3) 
}  