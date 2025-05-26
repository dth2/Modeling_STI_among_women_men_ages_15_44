# Modeling_STI_among_women_men_ages_15_44

This repository contains the EpiModelHIV version used for the analysis reported in"

Hamilton DT, Katz DA, Haderxhanaj LT, Copen CE, Spicknall IH, Hogben M. Modeling the impact of changing sexual behaviors with opposite-sex partners and STI testing among women and men ages 15-44 on STI diagnosis rates in the United States 2012-2019. Infect Dis Model. 2023 Oct 30;8(4):1169-1176. doi: 10.1016/j.idm.2023.10.005. PMID: 38074076; PMCID: PMC10709507.

The specific version of the EpiModelHIV R package used for this study can be downloaded from the EpiModelHIV_het_sti branch of this repository.

Before proceeding it is highly recommended that researchers thoroughly read the paper and associated technical appendix. 

# Installation

You can install the version of `EpiModelHIV` used for this analysis in R using the `remotes` R package:
```
remotes::install_github("dth2/Modeling_STI_among_women_men_ages_15_44",ref="EpiModelHIV_het_sti")
```

The versions for all other R packages used for this analysis can be found in the renv.lock file


# Data requirements
The primary data sources for this analysis are the NSFG (2011-2013 and 2017-2019). The NSFG is publicly available from the Centers for Disease Control and Prevention website https://www.cdc.gov/nchs/nsfg/nsfg_questionnaires.htm. The National Center for Health Statistics provides detailed guides for working with these data including options to download the data, management scripts, merging scripts and weighting scripts for SPSS, SAS, or STATA. For this study we opted to use the SPSS versions. The scripts for data cleaning and the initial variable construction are written in SPSS code but presented here as .txt for accesability.     

# Repeat the analysis

1) Files NSFG_01 through NSFG_14 are .txt files containing the SPSS code to convert the publicly available data files to those used in our analysis. NSFGFile 001 and 002 will convert the NSFG data files from wave 2011-2013, 2013-2015, 2015-2017, and 2017-2019 in to a single multi-wave data file following the National Center for Health Statistics guidlines for merging the data. The data from the four waves are harmonized and the the survey weights are added to the data files. Male and female data sets are combined seperatly. Scripts _11 through _14 extract just the 2011 and 2019 data used in this analysis, construct the individual variables needed for this study and output a self-weighted sample of the data to inform the sexual network model. Users will need to redefine all directory paths.

Inputfile = 2011_2013_MaleData.dat
Inputfile = 2011_2013_FemRespData.dat
Inputfile = 2013_2015_MaleData.dat
Inputfile = 2013_2015_FemRespData.dat
Inputfile = 2015_2017_MaleData.dat
Inputfile = 2015_2017_FemRespData.dat
Inputfile = 2017_2019_MaleData.dat
Inputfile = 2017_2019_FemRespData.dat

Output file = NSFGsexnet2011m_reduced_national.sav
Output file = NSFGsexnet2011f_reduced_national.sav
Output file = NSFGsexnet2019m_reduced_national.sav
Output file = NSFGsexnet2019f_reduced_national.sav

The male files for 2011 and 2019 should be merges with their respective female respiondent files with the following renaming conventions
ADD FILES /FILE=*
  /RENAME (condcwp CONDFREQ CONFREQ FSTSEXAGE PXCONFRQ PXCONFRQ2 PXCONFRQ3 RFSXAGEGP SEXFREQ 
    STDTST12=d0 d1 d2 d3 d4 d5 d6 d7 d8 d9)
  /FILE='PATH DIRECTORY\NSFGsexnet20XXf_reduced_national.sav'
  /RENAME (AGEFSTSX CONDOMR CURRPRTT=d10 d11 d12)
  /DROP=d0 d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12.
EXECUTE.

Output file = NSFGsexnet2011_reduced_national.sav
Output file = NSFGsexnet2019_reduced_national.sav

Convert the SPSS data files to .rdata files using stattransfer or a prefered data conversion application.

The remainder of the analysis is performed in R.


4) The Versions of R and all of the required R packages with versions are listed in the renvlock file. You can use the renv package to build the required R environment.

5) The R scripts "001 Make SWS of US 2019data.R" and "002 Make SWS of US 2011data.R" prep the NSFG data for use in the EpiModel environment. The output files are a self weighted sample of the NSFG data output as a long file that contains data on each sexual relatioship from the egos sampled with replacement from the NSFG and a wide file that contains a single row of attributes for each ego in the selfweighted sample from the NSFG data that will be used to inform the network model.

Input files = NSFGsexnet2011_reduced_national.rdata
Input files = NSFGsexnet2019_reduced_national.rdata

Output files = d_het_2011.rda
Output files = l_het_2011.rda
Output files = d_het_2019.rda
Output files = l_het_2019.rda

   
7) The Make NW_params.R and Make NW_stats.R R scripts should be loaded in to the R environment. These functions will be used to generate the network paramters and network statistics used by EpiModelHIV from the NSFG data. Specifically NW_params will calculate the individual-level network parameters and NW_stats will calculates the final target statistics (the g(y) for the ERGM and TERGM models) by applying the individual-level network statistics calcualted by the previous function against the population size and structure defined in the current analysis.

8) The 01 Set-up.R script will run the Make NW_params.R and NW_stats.R function on the appropriate data files and save the output for use by the next script in the analysis. If the directory path for the NW_params.R and Make NW_stats.R R scripts are specified in the 01 Set-up.R script they will be sourced automatically.

Input data = d_het_2011.rda
Input data = l_het_2011.rda
Input data = d_het_2019.rda
Input data = l_het_2019.rda

Output = netparams_2011.rda
Output = netstats_2011.rda
Output = netparams_2019.rda
Output = netstats_2019.rda

10) 02 estimate.R will estimate the 6 TERGMs/ERGMs that represent the sexual networks estimated from the 2011 and 2019 NSFG data. These models will drive the simulation of the 6 relational networks. The primary output is a single est object that contains all 6 network models that will be used in simulation. However, due to the size of the file each ERGM/TERGM model is saved seperately for use in the next step "diagnostics". Using just one model at a time preserves memory and was required to prevent system crashes. Estimation may take hours to days depending on computing power.

Input file = l_het_2011.rda
Input file = netparams_2011.rda
Input file = netstats_2011.rda

Input file = l_het_2019.rda
Input file = netparams_2019.rda
Input file = netstats_2019.rda

Output file = small_object_est_2011.rds
Output file = small_object_est_2019.rds

12) 03 diagnostics.R calculates the goodness of fit statistics for each of the network models as both static networks and dynamic networks. The output will provide the percent difference between the simulated output and the input values for target statistic in the ERGM/TERGM model. .tiff files are produced to graphically show the values of each netwrk statistic across simulations compaired to the target statistic. Path directory to store the .tiff files will need to be specified.  

13) 04 EpiStats.R calculates and stores the partnership-type-specific prediction models for behavior within the relational network that are estimated from the NSFG data. This includes prediction models for the number of sex acts within a partnership at each time step, condom use, and STI testing.

Input data = d_het_2011.rda
Input data = l_het_2011.rda
Input data = d_het_2019.rda
Input data = l_het_2019.rda

Output data = EpiStats_2011.rda
Output data = EpiStats_2019.rda

15) Once 02 estimate.R has estimated the ERGM and TERGM models, they have been determined to be satisfactory based on the diagnostics, and the EpiStats models have been estimated, the analysis can proceed to burn-in and calibration simulations.

16) The initial burn-in simulations must be run to reach an epidemic equalibrium and verify the network dynamics. We used 50 years as our starting burn-in period to cycle through a complete turnover in the population ages 15-65. The duraction of the burn-in is designated as nsteps in the sim.burn.R script.

17) Files for the burn-in process are in the burn_repo folder. the burn-in runs are executed on an HPC system. They were run on the HYAC-MOX HPC at the University of Washington using a SLURM scheduler. These files should be loaded on to the HPC system. master.burn.sh is the SLURM script to execute a burn-in simulation on the HPC system. master.burn.sh will call the runsim.sh script which will load R on the HPC system and execute the simulation script sim.burn.sh  sim.process.burn.R will process the output from the burn-in and produce plots comparing the simulated values for race-and-sex-specific GC and CT diagnoses to the target values. This file can also be modified to compaire any other epidemic outcomes or netwrok features stored by EpimodelHIV. 

18) If the netwrok statistics are not preserved by the initial ERGM/TERGM model estimates they can be adjuscted using the 05 adjust coef.R script and the burn-in can be repeated to confirm that the estimated netwok models are reproducing the target statistics. The burn-in may take hours to days depending on comuting power, the number of replicates, and the number of time steps specified. 

19) Once the network models are adjusted and the network statistics are preserved with satisfactory fidelity the Epidemic dynamics must be calibrated usind Approximate Baysian Computation. The files to run the ABC can be found in the ABC_repo folder. These files are desighned to be run on a HPC system. There were run on the HYAC-MOX HPC at the University of Washington using a SLURM scheduler.

20) The master.mox.sh script is executed on the HPC system to initiate the ABC. It is assumed the user has installed the required R version and all of the required R packages on their HPC system. The master file call the runsim which will source both R and the simulation file on the HPC system.

21) sim.abc.R defines each of the tuning parameters and their priors and the target statistics. Here the tuning parameters are race-specific probablities of GC and CT aquisition given exposure and duractions of untreated infection. Target statistics were race-and-sex-specific GC and CT diagnoses per 100,000 individuals.

22) sim.post.R will extract the posteriar distributions for each tuning parameter and plot it relative to the target value and the prior. 

23) Follwoing the ABC repeat step 17 using the output parameters from the ABC. We used 2000 replicates of the burn-in to identify a realization that most closely matched our epidemic target statistics. This realization will serve as the initial conditions for the epidemic simulations used in this study

24) Epidemic simulations were run on the HYAC-MOX HPC at the University of Washington using a SLURM scheduler. These files should be loaded on to the HPC system. master.burn.sh is the SLURM script to execute a burn-in simulation on the HPC system. There are 7 sim files that will run the scenarios evaluated in this study. Each scenario is run for 520 time steps (ten years)

sim_base.R  - the baseline scenario
