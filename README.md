# LoopMixEntension
Scripts for the reclassification of uncertain observation project with extension including lasso regularization and oberserver bias correction..
This README file was generated on 2023-03-08 by Emy Guilbault

GENERAL INFORMATION

Author Information A. Principal Investigator Contact Information Name: Emy Guilbault Institution: The University of Helsinki Address: Yliopistonkatu 4, 00100 Helsinki, FINLAND, Email: guilbaultemy@gmail.com

B. Associate or Co-investigator Contact Information Name: Ian Renner Institution: The University of Newcastle Address: University Dr, Callaghan NSW 2308 Email: ian.renner@newcastle.edu.au

C. Alternate Contact Information Name: Michael Mahony Institution: The University of Newcastle Address: University Dr, Callaghan NSW 2308 Email: michael.mahony@newcastle.edu.au


SHARING/ACCESS INFORMATION

Licenses/restrictions placed on the data: CC0 for the processed data, Rscript and environmental information file created.

Links to publications that cite or use the data: NA

Links to other publicly accessible locations of the data: The data presented here are also available at https://github.com/EmyGlblt/LoopMixEntension.

Links/relationships to ancillary data sets: NA

Was data derived from another source? No.


DATA & FILE OVERVIEW

File List: NewRealdata_Jan21.R: R script to analyse the data describe below.

Relationship between files, if important: 

Additional related data collected that was not included in the current data package: Simulation data and script are available in the github page mentioned before.

Are there multiple versions of the dataset? yes A. If yes, name of file(s) that was updated: i. Why was the file updated? The file was updated to be cleaned and make it available for publication. ii. When was the file updated? 02/08/2021

METHODOLOGICAL INFORMATION

Methods for processing the data: We use only the species label that were known from Ozra (2012) and the ALA (species identified after 2006). The other ALA data were considered as unknown and to be relabelled through our algorithm. We converted the specie coordinates to UTM.

Instrument- or software-specific information needed to interpret the data: To use the data, the software R is to be used with the following packages: library(spatstat) library(lattice) library(sp) library(maptools) library(raster) library(geostatsp) library(rgdal) library(caret) library(gridExtra) library(grid) library(latticeExtra)

Standards and calibration information, if appropriate: NA

Environmental/experimental conditions: NA

Describe any quality-assurance procedures performed on the data: The data were reviewed with Michael Mahony, specialist of this species.

People involved with sample collection, processing, analysis and/or submission: Emy Guilbault, Ian Renner, Michale Mahony and Eric Beh.

DATA-SPECIFIC INFORMATION FOR: 
