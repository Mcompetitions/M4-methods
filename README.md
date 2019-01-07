# M4-methods
This repository is dedicated to the M4 forecasting competition, the continuation of the previous three ones organized by Spyros Makridakis (for more information please visit https://www.mcompetitions.unic.ac.cy/).

Each folder includes source code that can be used for reproducing the forecasts submitted to the M4 Competition, as well as a short description of the methods utilized. Note that not all of the participants shared their code, meaning that some of the methods might not be available at this repository.

The “Dataset” folder includes the Train and Test set of the competition, as well as an Info file providing additional information per series, i.e. their ID (M4id), domain (category), frequency (Frequency), number of forecasts requested (Horizon), seasonal periods (SP) and starting date (StartingDate). Note that originally the starting date wasn’t provided to the participants. The “Frequency” variable is used for estimating the MASE evaluation measure (corresponds to the m value of the respective equation).

The “Point Forecasts” folder includes the point forecasts of all the M4 valid submissions (49), as well as those of the ten benchmarks and two standards of comparison. The “Submission Info” file can be used for matching the submission IDs with the M4 methods.

The “Prediction Intervals” folder includes the prediction intervals of all the M4 valid submissions (20), as well as those of the one benchmark and two standards of comparison. The “Submission Info” file can be used for matching the submission IDs with the M4 methods.

The “Benchmarks and Evaluation” file involves the R code used for generating the forecasts of the Statistical benchmarks of the competition. It can be also used for performing the evaluation (sMAPE, MASE and OWA estimation).

The “ML_benchmarks” file involves the Python code used for generating the forecasts of the Machine Learning benchmarks of the competition.

The “4Theta method” file involves the R code used for generating the forecasts of the 4Theta method. This method, which is a modification of the original Theta method (Assimakopoulos & Nikolopoulos, 2000 – winning method of M3), was submitted by two of the organizers (Spiliotis & Assimakopoulos) and, therefore, wasn’t eligible for a prize.
