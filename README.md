# Modeling_STI_among_women_men_ages_15_44

This repository contains the EpiModelHIV version used for the analysis reported in “Modeling the impact of changing sexual behaviors with opposite-sex partners and STI testing among women and men ages 15-44 on STI diagnosis rates in the United States 2012-2019.” 

Hamilton DT, Katz DA, Haderxhanaj LT, Copen CE, Spicknall IH, Hogben M. Modeling the impact of changing sexual behaviors with opposite-sex partners and STI testing among women and men ages 15-44 on STI diagnosis rates in the United States 2012-2019. Infect Dis Model. 2023 Oct 30;8(4):1169-1176. doi: 10.1016/j.idm.2023.10.005. PMID: 38074076; PMCID: PMC10709507.

Before proceeding it is highly recommended that researchers thoroughly read the paper and associated technical appendix. 


EpiModelHIV
===============

[![Build Status](https://travis-ci.org/statnet/EpiModelHIV.svg?branch=master)](https://travis-ci.org/statnet/EpiModelHIV)

Modules for simulating HIV/STI transmission dynamics among men who have sex with men and heterosexual populations, developed as an extension to our general network-based epidemic modeling platform, [EpiModel](http://epimodel.org).

`EpiModel` and `EpiModelHIV` use the statistical framework of temporal exponential-family random graph models to fit and simulate models of dynamic networks. These [statistical methods](http://onlinelibrary.wiley.com/doi/10.1111/rssb.12014/abstract) have been developed and implemented as open-source software, building on the extensive efforts of the [Statnet](https://statnet.org/) research group to build software tools for the representation, analysis, and visualization of complex network data.

These packages combine these Statnet methods with an agent-based epidemic modeling engine to simulate HIV transmission over networks, allowing for complex dependencies between the network, epidemiological, and demographic changes in the simulated populations. Readers new to these methods are recommended to consult the [EpiModel](http://epimodel.org) resources, including the main methods paper [Vignette](http://doi.org/10.18637/jss.v084.i08) describing the theory and implementation.


# Installation

You can install this version of `EpiModelHIV` used for this analysis in R using `remotes`:
```
remotes::install_github("dth2/Modeling_STI_among_women_men_ages_15_44",ref="EpiModelHIV_het_sti")
```

The versions for all other R packages used for this analysis can be found in the renv.lock file

Documentation on using this software package is forthcoming, although limited function documentation is provided within the package and available with the `help(package = "EpiModelHIV")` command.

    
