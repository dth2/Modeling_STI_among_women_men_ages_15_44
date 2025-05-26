# #' Setup the inputs for C30 model estimation
library(tidyverse)
library(EpiModel)

##2011
#Pull in the data
load("~/CAMP-HET-STI/NSFG/data/d_het_2011.rda")
load("~/CAMP-HET-STI/NSFG/data/l_het_2011.rda")
d<-d_het
l<-l_het

source("~/CAMP-HET-STI/Make NW_params.R")
netparams_2011 <- build_netparams(d,l)

source("~/CAMP-HET-STI/Make NW_stats.R")
netstats_2011 <- build_netstats(netparams_2011, d,l)



save(netparams_2011,file = "~/CAMP-HET-STI/est/netparams_2011.rda")
save(netstats_2011,file = "~/CAMP-HET-STI/est/netstats_2011.rda")

save(netparams_2011,file = "~/CAMP-HET-STI/scenarios/Model testing/est/netparams_2011.rda")
save(netstats_2011,file = "~/CAMP-HET-STI/scenarios/Model testing/est/netstats_2011.rda")

save(netparams_2011,file = "~/CAMP-HET-STI/scenarios/calib/est/netparams_2011.rda")
save(netstats_2011,file = "~/CAMP-HET-STI/scenarios/calib/est/netstats_2011.rda")


##2019
#Pull in the data
load("~/CAMP-HET-STI/NSFG/data/d_het_2019.rda")
load("~/CAMP-HET-STI/NSFG/data/l_het_2019.rda")
d<-d_het
l<-l_het

source("~/CAMP-HET-STI/Make NW_params.R")
netparams_2019 <- build_netparams(d,l)

source("~/CAMP-HET-STI/Make NW_stats.R")
netstats_2019 <- build_netstats(netparams_2019, d,l)



save(netparams_2019,file = "~/CAMP-HET-STI/est/netparams_2019.rda")
save(netstats_2019,file = "~/CAMP-HET-STI/est/netstats_2019.rda")

save(netparams_2019,file = "~/CAMP-HET-STI/scenarios/Model testing/est/netparams_2019.rda")
save(netstats_2019,file = "~/CAMP-HET-STI/scenarios/Model testing/est/netstats_2019.rda")

save(netparams_2019,file = "~/CAMP-HET-STI/scenarios/calib/est/netparams_2019.rda")
save(netstats_2019,file = "~/CAMP-HET-STI/scenarios/calib/est/netstats_2019.rda")
