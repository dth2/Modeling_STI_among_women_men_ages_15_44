

library(EpiModelHPC)
setwd("/homes/dth2/CAMP-HET-STI/scenarios/burn")


##No intervention

vars <- NULL


sbatch_master(vars,
              simno.start = 1000,
              master.file = "master.sh",
              runsim.file = "runsim.sh",
              build.runsim = TRUE,
              ckpt = TRUE,
              walltime = "10:00:00",
              nsims = 5000,
              ncores = 20,
              mem = "100G",
              rscript.file = "sim.burn.R")



