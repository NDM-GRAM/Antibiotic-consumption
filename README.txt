Code for the model of antibiotic usage in LMICs is in the antibiotic_usage folder. 
Code for the data cleaning and model of antibiotic consumption in LMICs and HICs is found in the antibiotic consumption folder.
Additional functions are in the misc folder

study_schematic.png shows an overview of the study and hoe estimates for antibitoic usage and antibiotic consumption were produced.

The MBG code for the antibiotic consumption model is adapted from code from the Local Burden of Disease (LBD) group of the Institute for
Health Metrics and Evaluation (IHME), available at https://github.com/ihmeuw/lbd.

For the Spatial-Temporal Gaussian Proccess Regression (ST-GPR) part of the model for antibiotic consumption in LMICs, the 13_LMIC_launch_stgpr.R 
script calls on code from the from IHME. The code is available at: https://github.com/ihmeuw/ihme-modeling/tree/master/gbd_2017/risk_factors_code/st_gpr

All code is written by Annie Browne (Annie.Browne@ndm.ox.ac.uk) unless otherwise specified.

The code accompanies the manscript:
Browne, A.J et al Global antibiotic consumption in humans, 2000 to 2018: a spatial modelling study. (2020); in preperation.