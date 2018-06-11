This is the source code of the submission "Weighted Ensemble of Statistical Models" to the M4 forecasting competition by ProLogistica Soft.


1. REQUIREMENTS

This software was developed and tested on Windows 7 Professional (64bit) in R 3.5.0, with the following packages:
  M4comp2018 0.1.0 (https://github.com/carlanetto/M4comp2018)
  trend 1.1.0
  pracma 2.1.4
  forecast 8.2 (version is important here, expect different results on forecast 8.3)
  sets 1.0-18
  dplyr 0.7.4
  pbapply 1.3-4
  robustbase 0.93-0
  abind 1.4-5
  parallel 3.5.0
  stats 3.5.0

We would also like to credit the forecTheta 2.2 package. We use a slightly modified version of forecTheta::otm function, so the package itself is not required.

For trouble-free usage 12 GB RAM is strongly recommended.


2. QUICK GUIDE

Set a path to the "tester" directory (see the comment at the top of M4tools.R).

For the forecasting demo on a small subset of M4 data set, see M4/M4demo.R
For the step-by-step example experiment setup on M4 Yearly data, go to M4/tests/M4test.yearly.R
The code to fully replicate our solution (for the entirety of M4 data) resides in M4/final/M4final.all.R

For the details of our method, refer to M4-Method-Description-ProLogistica_Soft.pdf


NOTE: Due to a minor mistake we made when preparing the final submission, a small group of series got assigned wrong forecasts. This means there will be a tiny discrepancy between our submission and the result calculated by the attached source code (where the problem was fixed). This concerns roughly 250 series from Daily category.

---------------------------------------------

ProLogistica Soft Sp. z. o. o.
ul. Ostrowskiego 9
53-238 Wrocław, Poland
kontakt@prologistica.pl
www.prologistica.pl
