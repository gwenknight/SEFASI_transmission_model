# SEFASI_model
A repository with the code to run the transmission model used in the SEFASI project for assessing the impact of One Health interventions. 

# Run the model 
To run the model use the code in 
(1) "run_model.R"
- this will generate a parameter set and run the model for this for each of the 3 countries (Denmark Senegal England)
- to speed this up change the size of p1 (the sampled parameters) from 100,000 (standard)

To explore the output run 
(2) "explore_model_output.R" 
- this will compare the FULLDATA outputs for each country to the data and generate the likelihood and the top 100 best fits and plot these

To explore this model output and select the parameter sets that generate runs closest to the data 
use the code in 
"explore_model_output.R"

To generate the intervention impact use 
"run_interventions.R"

