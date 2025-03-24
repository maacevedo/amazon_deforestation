# Amazon Deforestation

## Quick Summary

This repository describes an analysis of deforestation in the Brazilian Amazon that compares deforestation in indigenous territories and in buffers surrounding them. 

## Authors 

R. Walker, M. Acevedo, C. Simmons, J. Correia, M. Esbach, E. Arima, W. Cardoso, R. Leal, C. Urgiles, M. Manning

R. Walker collected the data

M. Acevedo conducted these statistical analyses. 

## File descriptions

``Dec30_Input_output_ALL-1.csv`` is the data

``beta_deforestation_bootstrap_5.R`` describes a beta regression analysis and estimation of amount of forest conserved. This code also includes estimation of confidence intervals on this calculation using simulation and non-parametric bootstrapping.

``neutral.R`` describes a simple regression on neutral landscapes to estimate the effect that you would expect just by chance based on the characteristics of the Amazon in Brazil.

``beta_deforestation_bootstrap_5_neutral.R`` is similar to ``beta_deforestation_bootstrap_5.R`` but this corrects for the neutral landscape effect and estimates CI accounting also for uncertainty in the estimation of this neutral effect.



