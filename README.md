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
