# Modeling_STI_among_women_men_ages_15_44

This repository contains the EpiModelHIV version used for the analysis reported in “Modeling the impact of changing sexual behaviors with opposite-sex partners and STI testing among women and men ages 15-44 on STI diagnosis rates in the United States 2012-2019.” The R package can be downloaded from the EpiModelHIV_het_sti branch of this repository.

Hamilton DT, Katz DA, Haderxhanaj LT, Copen CE, Spicknall IH, Hogben M. Modeling the impact of changing sexual behaviors with opposite-sex partners and STI testing among women and men ages 15-44 on STI diagnosis rates in the United States 2012-2019. Infect Dis Model. 2023 Oct 30;8(4):1169-1176. doi: 10.1016/j.idm.2023.10.005. PMID: 38074076; PMCID: PMC10709507.

Before proceeding it is highly recommended that researchers thoroughly read the paper and associated technical appendix. 

# Installation

You can install the version of `EpiModelHIV` used for this analysis in R using `remotes`:
```
remotes::install_github("dth2/Modeling_STI_among_women_men_ages_15_44",ref="EpiModelHIV_het_sti")
```

The versions for all other R packages used for this analysis can be found in the renv.lock file


# Data requirements
The primary data sources for this analysis are the NSFG (2011-2013 and 2017-2019). The NSFG is publicly available from the Centers for Disease Control and Prevention website https://www.cdc.gov/nchs/nsfg/nsfg_questionnaires.htm. The National Center for Health Statistics provides detailed guides for working with these data and options to download the data using SPSS, SAS, or STATA. For this study we opted to use the SPSS versions so the scripts for cleaning variable construction are written in SPSS code.     

# Repeat the analysis

1) Files NSFG_01 through NSFG_14 are test files containing the SPSS code to convert the publicly available NSFGFile 001 and 002 will convert the NSFG data files from wave 2011-2013, 2013-2015, 2015-2017, and 2017-2019 in to the data files used for this analysis. The data from the four waves are first harmonized and reduced to a subset a variables used or considered for use in this study. Male and female data sets are combined seperatly. Once the four waves of data are combined the scripts _11 through _14 eaxtrract just the 2011 and 2019 data used in this analysis, construct the individual variables needed for this study and output a self-weighted sample of the data to inform the sexual network model. Users will need to redefine all directory paths. One the NSFG data has been cleaned and combined the remainder of the analysis is performed in R.

2) The Versions of R and all of the required R packages with versions are listed in the renvlock file. You can use the renv package to build the required R environment.

3) The R scripts 001 Make SWS of US 2019data.R and 002 Make SWS of US 2011data.R prep the NSFG data for use in the EpiModel environment. The output files are a self weighted sample of the NSFG data output as a long file that contains data on each sexual relatioship from the egos sampled with replacement from the NSFG and a wide file that contains a single row of attributes for each ego in the selfweighted sample from the NSFG data that will be used to inform the network model.
   
5) The Make NW_params.R and Make NW_stats.R R scripts should be loaded in to the R environment. These functions will be used to generate the network paramters and network statistics used by EpiModelHIV from the NSFG data. Specifically NW_params will calculate the individual-level network parameters and NW_stats will 

