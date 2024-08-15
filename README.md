# SEFASI_model
A repository with the code to run the transmission model used in the SEFASI project for assessing the impact of One Health interventions. 

# Run the model 
(1) Calculate initial needed inputs: Run 0_usage_data.R to generate usage file for model fit 
(2) Run the model: Use 1_model_fit.R which calls other 0_xx.R files. 
  If running for the first time then uncomment rows 22-33. 
  Note this is the main running of the model and so is computationally expensive
  
(3) Explore model output: If running the model for > 10,000 runs then use 2_explore_model_output_big.R, 
otherwise use 2_explore_model_output.R 
  This determines the best 100 parameters and generates Figures 2 and 3
  
(4) Run interventions: Run 3_run_interventions.R. Generates Figures 4 and 5

(5) Perform univariate sensitivity: Run 4_sensitivity.R. Generates Figure 6. 

### Sensitivity analyses
(A) Fit to only human and animal data (i.e. remove environmental data = "sensE")
This uses the same model runs, just compares them only to human and animal data. 
So run (1) - (2) as above (or just start again at 3, can use previous model runs)

Then for (3): use 2_sensE_explore_model_output_big.R followed by
          (4): use 3_sens_run_interventions.R with the sensitivity indicator set to "sensE"

(B) Include a time-varying transmission rate for animal <-> human ("AHHA")
This uses new model runs. 
(1) In addition to step (1) above, firstly the shape of the transmission rate must be calculated by running 0_sens_transmission_AHHA.R

(2) Then new model runs must be performed using 1_sens_model_fit_transmissionAHHA.R. 

(3) The fit of these to the data is performed using 2_sensAHHA_explore_model_output_big.R. 

(4) use 3_sens_run_interventions.R with the sensitivity indicator set to "senstahha"



##### Old code
If running only a small number of runs then 2_explore_model_output.R should still work. 