# LoopMixEntension
Scripts for the reclassification of uncertain observation project with extension including lasso regularization and oberserver bias correction.
This README file was generated on 2023-03-29 by Emy Guilbault

GENERAL INFORMATION
#.............................................
Author Information A. Principal Investigator Contact Information Name: Emy Guilbault Institution: The University of Helsinki Address: Yliopistonkatu 4, 00100 Helsinki, FINLAND, Email: guilbaultemy@gmail.com

B. Associate or Co-investigator Contact Information Name: Ian Renner Institution: The University of Newcastle Address: University Dr, Callaghan NSW 2308 Email: ian.renner@newcastle.edu.au

C. Alternate Contact Information Name: Michael Mahony Institution: The University of Newcastle Address: University Dr, Callaghan NSW 2308 Email: michael.mahony@newcastle.edu.au

D. Alternate Contact Information Name: Eric J. Beh Institution: National Institute for Applied Statistics Research Australia (NIASRA), University of Wollongong & Centre for Multi-Dimensional Data Visualisation (MuViSU), Stellenbosch University Email: eric.beh@newcastle.edu.au


SHARING/ACCESS INFORMATION
#.............................................
Licenses/restrictions placed on the data: CC0 for the processed data, Rscript and environmental information file created.

Links to publications that cite or use the data: NA

Links to other publicly accessible locations of the data: The real data used in this work is available at https://github.com/EmyGlblt/LoopMixArticle. Information related to this is describe there.

Links/relationships to ancillary data sets: NA

Was data derived from another source? No.


DATA & FILE OVERVIEW
#.............................................
File List: 
Scripts
    - ResSimLBtest1.r : performances measures plots based on the test 1 (unequal abundances and no correlation) for lasso and bias correction.
    - IntLB.r : How to plot the intensity distribution for the different initialization methods simulation results.
    - Coeffplot.r: hot to obtain the plot of the coefficient resulting from the simulations for 20, 50 and 80% of hidden observation for test 1.
    - plotTruehide23.r: plot the distribution of points when we hide from the truth (the 1000 simulations are summarized)
    - LassoTestsimpackage23: functions used in the 
    - NewRealdata_Jan21.R: R script to analyse the real data.

Data file
    - ResLBtest1.RDATA: Summarized performances results from the simulation of test 1 (unequal abundances and no correlation) for lasso and bias correction.
    - ResnoLnoBtest1.RDATA: Summarized performances results from the simulation of test 1 (unequal abundances and no correlation) without lasso and without bias correction.
    - ResnoLBtest1.RDATA: Summarized performances results from the simulation of test 1 (unequal abundances and no correlation) without lasso and with bias correction.
    - ResLnoBtest1.RDATA: Summarized performances results from the simulation of test 1 (unequal abundances and no correlation) with lasso and without bias correction.
    - ResInttest1LB.RDATA: Summarized intensity results from the simulation of test 1 (unequal abundances and no correlation) for lasso and bias correction.
    - RescoefLBtest1.RData: Summarized coefficient results from the simulation of test 1 (unequal abundances and no correlation) for lasso and bias correction.
    - DifAb_nocor_Oct20.RDATA : Information on the initial process created to be used in the simulation
    - RealDataMenv.RDATA: prepared data and information for use in the algorithm.
    - TrueM.RDATA: saved information about the real data, point pattern created and environmental information
    - LMtrue22all.RDATA: example of application exercise hidding from the truth where we volunterily hide known species points accross 1000 simulations. We have summarized the information such that the simulations where run 32 times with 20 to 50 runs to reach 1000 simulations. The file only provide a summary of these 32 averaged results for simplicity.

  


Relationship between files, if important: 

Additional related data collected that was not included in the current data package: Simulation data and script are available in the github page mentioned before. The real data file is available at https://github.com/EmyGlblt/LoopMixArticle.
  - ResSimLBtest1.r make use of ResLBtest1.RDATA, ResnoLBtest1.RDATA, ResLnoBtest1.RDATA, and ResnoLnoBtest1.RDATA to compare the performances between the different implementation of lasso and bias correction
  - IntLB.r uses the summarized results in ResInttest1LB.RDATA and the true patterns and intensity created for the simulations in DifAb_nocor_Oct20.RDATA (Test 1).
  - Coeffplot.r uses the summarized results in RescoefLBtest1.RData and the true patterns and their coefficients created for the simulations in DifAb_nocor_Oct20.RDATA (Test 1).
  - plotTruehide23.r uses LMtrue22all.RDATA to access the results of test of hidding from the truth and TrueM.RDATA which has saved the data (species and environmental) information.
   - NewRealdata_Jan21.R uses the function in LassoTestsimpackage23 to run the algorithm and the set up of the data (species and environmental) from RealDataMenv.RDATA.


Are there multiple versions of the dataset? yes A. If yes, name of file(s) that was updated: i. Why was the file updated? The file was updated to be cleaned and make it available for publication. ii. When was the file updated? 29.03.2023.


METHODOLOGICAL INFORMATION
#.............................................

Methods for processing the data: We use only the species label that were known from Ozra (2012) and the ALA (species identified after 2006). The other ALA data were considered as unknown and to be relabelled through our algorithm. We converted the specie coordinates to UTM. For more information refer to Guilbault et al., 2021.

Instrument- or software-specific information needed to interpret the data: To use the data, the software R is to be used with the following packages: library(spatstat) library(lattice) library(sp) library(maptools) library(raster) library(geostatsp) library(rgdal) library(caret) library(gridExtra) library(grid) library(latticeExtra)

Standards and calibration information, if appropriate: NA

Environmental/experimental conditions: NA

Describe any quality-assurance procedures performed on the data: The data were reviewed with Michael Mahony, specialist of this species.

People involved with sample collection, processing, analysis and/or submission: Emy Guilbault, Ian Renner, Michale Mahony and Eric Beh.

DATA-SPECIFIC INFORMATION FOR: 
